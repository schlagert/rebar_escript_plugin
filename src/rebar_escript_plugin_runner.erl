%%%=============================================================================
%%% Copyright 2014, Tobias Schlager <schlagert@github.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% A helper module used only at runtime. It is packaged into the created
%%% escript and sets up the environment. This is also the place where the
%%% default application starter {@link default_main/1} is located.
%%%
%%% If an application wants to write a custom `main/1' function, please note
%%% that this function needs to block the calling process. Otherwise, the
%%% escript will exit immediately.
%%% @end
%%%=============================================================================
-module(rebar_escript_plugin_runner).

%% API
-export([main/3]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Called from the generated escript main module. This is needed to make `priv'
%% directories available to the applications the usual way. If present, the
%% execution is delegated to the the main module's `main/1' function. The main
%% module is the module with the name of the application to start (in case
%% there is only one application to be started).
%%
%% `priv' directory content is made available by extracting it prior to starting
%% the applications to a temporary directory. The heart mechanism is used to
%% make sure the temporary files are cleaned on program exit. Regardless, heart
%% can still be used to restart the node, since the `HEART_COMMAND' environment
%% variable will still be queried (and executed) on node death. The
%% `code_path_choice' parameter of the code server has to be set to `relaxed'
%% (which is the default anyways) for this to work.
%%
%% @see http://www.erlang.org/doc/man/code.html
%% @end
%%------------------------------------------------------------------------------
main(AppsToStart, Args, PackagedApps) ->
    {OsFamily, _OsName} = os:type(),
    TempDir = mk_temp(OsFamily),
    setup_heart(OsFamily, TempDir),
    [prim_cp_priv_dir(TempDir, Packaged) || Packaged <- PackagedApps],
    NewPaths = filelib:wildcard(filename:join([TempDir, "*"])),
    true = code:set_path(NewPaths ++ code:get_path()),
    invoke_main(AppsToStart, Args).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Searches for a `main/1' function in the module with the name of the given
%% application (if only one) and calls it. If there is no such function or
%% multiple applications need to be started, a default main function is used
%% that starts everything including dependency applications.
%%------------------------------------------------------------------------------
invoke_main(Apps = [App], Args) ->
    case code:ensure_loaded(App) of
        {module, App} ->
            case erlang:function_exported(App, main, 1) of
                true ->
                    App:main(Args);
                false ->
                    default_main(Apps)
            end;
        _ ->
            default_main(Apps)
    end;
invoke_main(Apps, _Args) ->
    default_main(Apps).

%%------------------------------------------------------------------------------
%% @private
%% Starts the given list of applications along with their dependency
%% applications and blocks the calling process infinitely.
%%------------------------------------------------------------------------------
default_main(Apps) ->
    [{ok, _} = application:ensure_all_started(App, permanent) || App <- Apps],
    timer:sleep(infinity).

%%------------------------------------------------------------------------------
%% @private
%% Create and return a directory in the system specific temporary directory. If
%% the directory already exists it will be deleted beforehand.
%%------------------------------------------------------------------------------
mk_temp(OsFamily) ->
    EScript = filename:basename(escript:script_name()),
    TempDir = filename:join([get_tmp(OsFamily), EScript]),
    rm_rf(TempDir),
    ok = file:make_dir(TempDir),
    TempDir.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_tmp(OsFamily) when OsFamily =:= nt; OsFamily =:= windows ->
    os:getenv("TEMP");
get_tmp(_OsFamily) ->
    filename:join(["/", "tmp"]).

%%------------------------------------------------------------------------------
%% @private
%% Setup a temporary heart command that cleans up the given temporary directory
%% on node death *before* executing the restart command specified in the
%% `HEART_COMMAND' environment variable.
%%------------------------------------------------------------------------------
setup_heart(OsFamily, TempDir) ->
    case os:getenv("HEART_COMMAND") of
        Empty when Empty =:= false; Empty =:= "" ->
            HeartCmd = "";
        Cmd when OsFamily =:= nt; OsFamily =:= windows ->
            HeartCmd = " & " ++ Cmd;
        Cmd ->
            HeartCmd = " ; " ++ Cmd
    end,
    case OsFamily of
        _ when OsFamily =:= nt; OsFamily =:= windows ->
            ok = heart:set_cmd("rmdir \"" ++ TempDir ++ "\" /s /q" ++ HeartCmd);
        _ ->
            ok = heart:set_cmd("rm -rf '" ++ TempDir ++ "'" ++ HeartCmd)
    end.

%%------------------------------------------------------------------------------
%% @private
%% Extracts the `priv' directory of the given application into a temporary
%% directory (on disk) using the `erl_prim_loader'.
%%------------------------------------------------------------------------------
prim_cp_priv_dir(TempDir, App) ->
    PrivDir = code:priv_dir(App),
    ["priv" | [AppDir | _]] = lists:reverse(filename:split(PrivDir)),
    prim_cp_dir(filename:join([TempDir, AppDir, "priv"]), PrivDir).

%%------------------------------------------------------------------------------
%% @private
%% Recursively copy all files from one directory to another keeping the original
%% directory structure using the `erl_prim_loader'. Specifically, this function
%% can extract directories contained in `.ez' archives. Empty directories will
%% not be created/copied.
%%------------------------------------------------------------------------------
prim_cp_dir(TargetDir, SrcDir) ->
    [prim_cp_file(TargetDir, SrcDir, File) || File <- prim_list_dir(SrcDir)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
prim_cp_file(TargetDir, SrcDir, SrcFile) ->
    TargetFile = filename:join([TargetDir, relative_name(SrcDir, SrcFile)]),
    ok = filelib:ensure_dir(TargetFile),
    {ok, Binary, SrcFile} = erl_prim_loader:get_file(SrcFile),
    ok = file:write_file(TargetFile, Binary).

%%------------------------------------------------------------------------------
%% @private
%% Recursively list the files in a directory using the `erl_prim_loader'.
%% Specifically, this function can list directories contained in `.ez' archives.
%%------------------------------------------------------------------------------
prim_list_dir(Dir) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Files} ->
            prim_list_dir(Dir, Files, []);
        error ->
            []
    end.
prim_list_dir(Dir, Files0, Acc0) ->
    lists:foldl(
      fun(File, Acc1) ->
              Path = filename:join([Dir, File]),
              case erl_prim_loader:list_dir(Path) of
                  {ok, Files1} ->
                      prim_list_dir(Path, Files1, Acc1);
                  error ->
                      Acc1 ++ [Path]
              end
      end, Acc0, Files0).

%%------------------------------------------------------------------------------
%% @private
%% Recursively remove a directory, the returned term should be ignored.
%%------------------------------------------------------------------------------
rm_rf(Dir) ->
    rm_rf(Dir, filelib:wildcard(filename:join([Dir, "**"]))).
rm_rf(Dir, []) ->
    file:del_dir(Dir);
rm_rf(Dir, [File | Rest]) ->
    case filelib:is_dir(File) of
        true ->
            case file:del_dir(File) of
                ok ->
                    rm_rf(Dir, Rest);
                {error, eexist} ->
                    rm_rf(Dir, Rest ++ [File])
            end;
        false ->
            ok = file:delete(File),
            rm_rf(Dir, Rest)
    end.

%%------------------------------------------------------------------------------
%% @private
%% Converts an absolute path into a relative one. The path is made relative to
%% the given base directory. If `BasePath' is not a parent path of `Filename'
%% this function fails.
%%------------------------------------------------------------------------------
relative_name(BasePath, Filename) ->
    1 = string:rstr(Filename, BasePath),
    case string:substr(Filename, string:len(BasePath) + 1) of
        [$/ | RelName] ->
            RelName;
        RelName ->
            RelName
    end.
