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
%%% @end
%%%=============================================================================
-module(rebar_escript_plugin).

%% API
-export([post_compile/2,
         post_clean/2]).

-define(TEMP_DIR, ".escript").
-define(MAIN_MODULE, rebar_escript_plugin_main).

-include_lib("kernel/include/file.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Create a standalone escript of an application and its dependencies in the
%% project's base directory after compilation with `rebar compile'.
%%
%% All applications located in `deps_dir' and `lib_dirs' will be included. If
%% not provided, the plugin will create a start module with a default `main/1'
%% function which uses {@link application:ensure_all_started/2} to bootstrap
%% the application.
%% @end
%%------------------------------------------------------------------------------
-spec post_compile(rebar_config:config(), file:filename()) -> ok.
post_compile(Config, AppFile) ->
    case rebar_utils:processing_base_dir(Config) of
        true ->
            TempDir = temp_dir(Config),
            ok = rebar_utils:ensure_dir(filename:join([TempDir, "dummy"])),
            AppFiles = [AppFile | app_files(dep_dirs(Config))],
            PackagedApps = mk_links(TempDir, Config, AppFiles),
            ok = prepare_runner_module(),
            ok = prepare_main_module(app_name(Config, AppFile), PackagedApps),
            Ez = create_ez(TempDir, Config, AppFile),
            ok = create_escript(Ez, Config, AppFile);
        false ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Remove the plugin's temporary directory and custom files after `rebar clean'.
%% @end
%%------------------------------------------------------------------------------
-spec post_clean(rebar_config:config(), file:filename()) -> ok.
post_clean(Config, AppFile) ->
    case rebar_utils:processing_base_dir(Config) of
        true  ->
            {OsFamily, _OsName} = os:type(),
            rm_rf(temp_dir(Config)),
            rm_rf(main_path(rebar_escript_plugin_runner)),
            rm_rf(main_path(?MAIN_MODULE)),
            rm_rf(escript_path(OsFamily, Config, AppFile));
        false ->
            ok
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Searches the given list of `lib' and `deps' directories for `.app' files,
%% excluding system applications and the plugin itself.
%%------------------------------------------------------------------------------
app_files(DepDirs) ->
    ExcludedApps = excluded_apps(),
    AppDirLists = [filelib:wildcard(filename:join([D, "*"])) || D <- DepDirs],
    [AppFile || AppDir <- lists:append(AppDirLists),
                {true, AppFile} <- [rebar_app_utils:is_app_dir(AppDir)],
                may_include(strip_extension(AppFile), ExcludedApps)].

%%------------------------------------------------------------------------------
%% @private
%% Creates links to the application directories corresponding to the given list
%% of `.app' files in a temporary directory. This is needed to get the directory
%% structure desired by the code server for the created `.ez' archive.
%%------------------------------------------------------------------------------
mk_links(TempDir, Config, AppFiles) ->
    [mk_link(TempDir, Config, AppFile) || AppFile <- AppFiles].

%%------------------------------------------------------------------------------
%% @private
%% Creates a link to the application directory corresponding to the given
%% `.app' file.
%%------------------------------------------------------------------------------
mk_link(TempDir, Config, AppFile) ->
    LinkName = app_link(TempDir, Config, AppFile),
    case file:make_symlink(app_dir(AppFile), LinkName) of
        ok ->
            app_name(Config, AppFile);
        {error, eexist} ->
            app_name(Config, AppFile);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rm_rf(Path) -> rebar_file_utils:rm_rf(Path).

%%------------------------------------------------------------------------------
%% @private
%% Returns the path to an application directory, based on the path to its
%% `.app' file.
%%------------------------------------------------------------------------------
app_dir(AppFile) ->
    SplittedPath = filename:split(filename:absname(AppFile)),
    [_AppFileName | [_SrcOrEbin | RestPath]] = lists:reverse(SplittedPath),
    filename:join(lists:reverse(RestPath)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
app_name(Config, AppFile) ->
    element(2, rebar_app_utils:app_name(Config, AppFile)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
app_vsn(Config, AppFile) ->
    element(2, rebar_app_utils:app_vsn(Config, AppFile)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
app_name_and_vsn(Config, AppFile) ->
    AppName = atom_to_list(app_name(Config, AppFile)),
    AppVsn = app_vsn(Config, AppFile),
    AppName ++ "-" ++ AppVsn.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
app_link(TempDir, Config, AppFile) ->
    filename:join([TempDir, app_name_and_vsn(Config, AppFile)]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
temp_dir(Config) -> filename:join([rebar_utils:base_dir(Config), ?TEMP_DIR]).

%%------------------------------------------------------------------------------
%% @private
%% Returns a list of the `lib' and `deps' directories used by the current
%% project/application. This includes the path configured as `deps_dir' and the
%% paths specified as `lib_dirs' in the `rebar.config' file.
%%------------------------------------------------------------------------------
dep_dirs(Config) ->
    BaseDir = rebar_utils:base_dir(Config),
    {true, DepsDir} = rebar_deps:get_deps_dir(Config),
    LibDirs = rebar_config:get_local(Config, lib_dirs, []),
    [DepsDir | [filename:join([BaseDir, Dir]) || Dir <- LibDirs]].

%%------------------------------------------------------------------------------
%% @private
%% Creates an `.ez' archive (in memory) to be embedded in the escript. The
%% archive will contain `ebin' and `priv' directories of all dependent
%% applications. The archive will have the following content layout:
%%
%% application-version.ez
%%  + application-version/ebin
%%  + application-version/priv
%%  + dependency1-version/ebin
%%  + dependency1-version/priv
%%  + dependency2-version/ebin
%%  + dependency2-version/priv
%%  + ...
%%
%%------------------------------------------------------------------------------
create_ez(TempDir, Config, AppFile) ->
    {ok, {_, Archive}} =
        zip:create(
          app_name_and_vsn(Config, AppFile) ++ ".ez",
          filelib:wildcard(filename:join(["*", "{ebin,priv}"]), TempDir),
          [{cwd, TempDir}, {uncompress, all}, memory]),
    Archive.

%%------------------------------------------------------------------------------
%% @private
%% Returns the path to the escript to create. On windows systems the file will
%% have the extension `.escript', unix systems will omit the extension.
%%------------------------------------------------------------------------------
escript_path(OsFamily, Config, AppFile) ->
    AppName = atom_to_list(app_name(Config, AppFile)),
    Extension = escript_extension(OsFamily),
    filename:join([rebar_utils:base_dir(Config), AppName ++ Extension]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
create_escript(Ez, Config, AppFile) ->
    {OsFamily, _OsName} = os:type(),
    EScript = escript_path(OsFamily, Config, AppFile),
    HeartArg = "-heart",
    MainArg = "-escript main " ++ atom_to_list(?MAIN_MODULE),
    EmuArgs = string:join([HeartArg, MainArg, get_emu_args(Config)], " "),
    Sections = [shebang, comment, {emu_args, EmuArgs}, {archive, Ez}],
    ok = escript:create(EScript, Sections),
    ok = set_executable(OsFamily, EScript).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
strip_extension(FilePath) -> filename:rootname(filename:basename(FilePath)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
escript_extension(nt)      -> ".escript";
escript_extension(windows) -> ".escript";
escript_extension(_)       -> "".

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_executable(unix, EScript) ->
    {ok, Info} = file:read_file_info(EScript),
    NewMode = (((Info#file_info.mode bor 8#1) bor 8#10) bor 8#100),
    ok = file:write_file_info(EScript, Info#file_info{mode = NewMode});
set_executable(_, _EScript) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
main_path(Module) ->
    filename:join([rebar_utils:ebin_dir(), atom_to_list(Module) ++ ".beam"]).

%%------------------------------------------------------------------------------
%% @private
%% Writes the code of the runner module into the `ebin' directory of the
%% application to package. This code will be needed at runtime to setup the
%% correct environment.
%%------------------------------------------------------------------------------
prepare_runner_module() ->
    Module = rebar_escript_plugin_runner,
    {Module, Binary, _} = code:get_object_code(Module),
    ok = file:write_file(main_path(Module), Binary).

%%------------------------------------------------------------------------------
%% @private
%% Generates and writes the code of the main module into the `ebin' directory
%% of the application to package. This code provides the main entry point for
%% escript execution (it provides the `main/1' function).
%%------------------------------------------------------------------------------
prepare_main_module(AppName, PackagedApps) ->
    {ok, T1, _} = erl_scan:string(
                    "-module("
                    ++ atom_to_list(?MAIN_MODULE)
                    ++ ")."),
    {ok, T2, _} = erl_scan:string("-export([main/1])."),
    {ok, T3, _} = erl_scan:string(
                    "main(Args) ->"
                    "    rebar_escript_plugin_runner:main("
                    ++ atom_to_list(AppName)
                    ++ ", Args, "
                    ++ lists:flatten(io_lib:format("~w", [PackagedApps]))
                    ++ ")."),
    {ok, F1} = erl_parse:parse_form(T1),
    {ok, F2} = erl_parse:parse_form(T2),
    {ok, F3} = erl_parse:parse_form(T3),
    {ok, ?MAIN_MODULE, Binary} = compile:forms([F1, F2, F3]),
    ok = file:write_file(main_path(?MAIN_MODULE), Binary).

%%------------------------------------------------------------------------------
%% @private
%% Returns a custom emulator argument string, if one. Please note that the
%% `-escript main' parameter is forbidden. It must point to the internal
%% generated module. Otherwise the execution environment will not be setup
%% correctly. Additionally, the `-heart' parameter is always set (for cleanup)
%% and it is therefore not necessary to specify it.
%%------------------------------------------------------------------------------
get_emu_args(Config) ->
    Cfg = rebar_config:get(Config, ?MODULE, []),
    case proplists:get_value(emu_args, Cfg, "") of
        EmuArgs when is_list(EmuArgs) -> EmuArgs
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
may_include(AppStr, ExcludedApps) ->
    lists:all(fun(App) -> string:str(App, AppStr) =:= 0 end, ExcludedApps).

%%------------------------------------------------------------------------------
%% @private
%% Returns a list of strings representing applications to exclude from
%% packaging.
%%------------------------------------------------------------------------------
excluded_apps() ->
    [atom_to_list(?MODULE) | filelib:wildcard("*", code:lib_dir())].
