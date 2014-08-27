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
-define(START_MODULE, rebar_escript_plugin_starter).

-include_lib("kernel/include/file.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Create a standalone escript of the application and its dependencies in the
%% project's base directory after compilation with `rebar compile'.
%% @end
%%------------------------------------------------------------------------------
-spec post_compile(rebar_config:config(), file:filename()) -> ok.
post_compile(Config, AppFile) ->
    case rebar_app_utils:is_app_dir() of
        {true, AppFile} ->
            TempDir = temp_dir(Config),
            ok = ensure_dir(TempDir),
            ok = mk_link(TempDir, Config, AppFile),
            case rebar_utils:processing_base_dir(Config) of
                true ->
                    {ok, Main} = check_main(Config, AppFile),
                    Ez = create_ez(TempDir, Config, AppFile),
                    ok = create_escript(Main, Ez, Config, AppFile);
                false ->
                    ok
            end;
        false ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Remove the plugin's temporary directory after `rebar clean'.
%% @end
%%------------------------------------------------------------------------------
-spec post_clean(rebar_config:config(), file:filename()) -> ok.
post_clean(Config, AppFile) ->
    case rebar_utils:processing_base_dir(Config) of
        true  ->
            {OsFamily, _OsName} = os:type(),
            rm_rf(temp_dir(Config)),
            rm_rf(main_path(?START_MODULE)),
            rm_rf(escript_path(OsFamily, Config, AppFile));
        false ->
            ok
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
ensure_dir(Path) -> rebar_utils:ensure_dir(filename:join([Path, "dummy"])).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
mk_link(TempDir, Config, AppFile) ->
    LinkName = app_link(TempDir, Config, AppFile),
    case file:make_symlink(app_dir(AppFile), LinkName) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rm_rf(Path) -> rebar_file_utils:rm_rf(Path).

%%------------------------------------------------------------------------------
%% @private
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
%%------------------------------------------------------------------------------
check_main(Config, AppFile) ->
    AppName = app_name(Config, AppFile),
    case code:ensure_loaded(AppName) of
        {module, AppName} ->
            case erlang:function_exported(AppName, main, 1) of
                true ->
                    {ok, AppName};
                false ->
                    {create_main(?START_MODULE, AppName), ?START_MODULE}
            end;
        _ ->
            {create_main(?START_MODULE, AppName), ?START_MODULE}
    end.

%%------------------------------------------------------------------------------
%% @private
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
%%------------------------------------------------------------------------------
escript_path(OsFamily, Config, AppFile) ->
    AppName = atom_to_list(app_name(Config, AppFile)),
    Extension = escript_extension(OsFamily),
    filename:join([rebar_utils:base_dir(Config), AppName ++ Extension]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
create_escript(Main, Ez, Config, AppFile) ->
    {OsFamily, _OsName} = os:type(),
    EScript = escript_path(OsFamily, Config, AppFile),
    EmuArgs = "-escript main " ++ atom_to_list(Main),
    Sections = [shebang, comment, {emu_args, EmuArgs}, {archive, Ez}],
    ok = escript:create(EScript, Sections),
    ok = set_executable(OsFamily, EScript).

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
%%------------------------------------------------------------------------------
create_main(Module, AppName) ->
    {ok, T1, _} = erl_scan:string("-module(" ++ atom_to_list(Module) ++ ")."),
    {ok, T2, _} = erl_scan:string("-export([main/1])."),
    {ok, T3, _} = erl_scan:string(
                    "main([]) ->"
                    ++ "    {ok, _} = application:ensure_all_started("
                    ++ atom_to_list(AppName)
                    ++ "),"
                    ++ "    timer:sleep(infinity)."),
    {ok, F1} = erl_parse:parse_form(T1),
    {ok, F2} = erl_parse:parse_form(T2),
    {ok, F3} = erl_parse:parse_form(T3),
    {ok, Module, Binary} = compile:forms([F1, F2, F3]),
    ok = file:write_file(main_path(Module), Binary).
