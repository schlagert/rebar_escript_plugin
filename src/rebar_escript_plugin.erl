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
%%% A `rebar' plugin to package applications or releases as executable escript
%%% similar to `rebar escriptize' but without its limitations. The plugin
%%% automatically executes as `post_compile' or `post_generate' hook.
%%% To use it specify it in the `plugins' section of your project's
%%% `rebar.config', e.g. `{plugins, [rebar_escript_plugin]}.'
%%% Depending on your preference put the plugin somewhere into your code path
%%% or simply include it as project dependency.
%%% @end
%%%=============================================================================
-module(rebar_escript_plugin).

%% API
-export([post_compile/2,
         post_generate/2,
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
    IsBaseDir = rebar_utils:processing_base_dir(Config),
    IsAppDir = rebar_app_utils:is_app_dir(),
    IsRelDir = rebar_rel_utils:is_rel_dir(),
    case {IsBaseDir, IsAppDir, IsRelDir} of
        {true, {true, AppFile}, false} ->
            BaseDir = rebar_utils:base_dir(Config),
            TempDir = temp_dir(BaseDir),
            ok = rebar_utils:ensure_dir(filename:join([TempDir, "dummy"])),
            ok = prepare_runner_module(TempDir),
            AppsToStart = [AppName] = [app_name(Config, AppFile)],
            AppFiles = [AppFile | app_files(dep_dirs(BaseDir, Config))],
            PackagedApps = mk_links(TempDir, Config, AppFiles),
            ok = prepare_main_module(TempDir, AppsToStart, PackagedApps),
            Ez = create_ez(TempDir, Config, AppFile, PackagedApps),
            ok = create_escript(Ez, BaseDir, Config, atom_to_list(AppName));
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Create a standalone escript of a release in the release's base directory
%% after generation with `rebar generate'.
%% @end
%%------------------------------------------------------------------------------
-spec post_generate(rebar_config:config(), file:filename()) -> ok.
post_generate(Config, RelToolFile) ->
    case rebar_rel_utils:is_rel_dir() of
        {true, RelToolFile} ->
            BaseDir = filename:dirname(RelToolFile),
            RelToolConfig = reltool_cfg(Config, RelToolFile),
            {RelFile, LibDir} = reltool_info(Config, RelToolConfig),
            ok = prepare_runner_module(LibDir),
            {RelName, AppsToStart} = rel_info(RelFile),
            AppFiles = [AppFile | _] = app_files([LibDir]),
            PackagedApps = [app_name(Config, AppF) || AppF <- AppFiles],
            ok = prepare_main_module(LibDir, AppsToStart, PackagedApps),
            Ez = create_ez(LibDir, Config, AppFile, PackagedApps),
            ok = create_escript(Ez, BaseDir, Config, RelName);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Remove the plugin's temporary directory and custom files after `rebar clean'.
%% @end
%%------------------------------------------------------------------------------
-spec post_clean(rebar_config:config(), file:filename()) -> ok.
post_clean(Config, ResourceFile) ->
    IsBaseDir = rebar_utils:processing_base_dir(Config),
    IsAppDir = rebar_app_utils:is_app_dir(),
    IsRelDir = rebar_rel_utils:is_rel_dir(),
    case {IsBaseDir, IsAppDir, IsRelDir} of
        {true, {true, AppFile}, false} when AppFile =:= ResourceFile ->
            {OsFamily, _OsName} = os:type(),
            BaseDir = rebar_utils:base_dir(Config),
            rm_rf(temp_dir(BaseDir)),
            AppName = atom_to_list(app_name(Config, AppFile)),
            rm_rf(escript_path(OsFamily, BaseDir, AppName));
        {_, false, {true, RelToolFile}} when RelToolFile =:= ResourceFile ->
            {OsFamily, _OsName} = os:type(),
            BaseDir = filename:dirname(RelToolFile),
            RelToolConfig = reltool_cfg(Config, RelToolFile),
            {RelFile, LibDir} = reltool_info(Config, RelToolConfig),
            {RelName, _AppsToStart} = rel_info(RelFile),
            rm_rf(main_path(LibDir, ?MAIN_MODULE)),
            rm_rf(main_path(LibDir, rebar_escript_plugin_runner)),
            rm_rf(escript_path(OsFamily, BaseDir, RelName));
        _ ->
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
reltool_cfg(Config, RelToolFile) ->
    element(2, rebar_rel_utils:load_config(Config, RelToolFile)).

%%------------------------------------------------------------------------------
%% @private
%% Return the paths to the releases `.rel' file and its `lib' directory.
%%------------------------------------------------------------------------------
reltool_info(Config, RelToolConfig) ->
    {Name, Version} = rebar_rel_utils:get_reltool_release_info(RelToolConfig),
    TargetDir = rebar_rel_utils:get_target_dir(Config, RelToolConfig),
    {filename:join([TargetDir, "releases", Version, Name ++ ".rel"]),
     filename:join([TargetDir, "lib"])}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
rel_info(RelFile) ->
    {ok, [{release, {RelName, _}, _, Apps}]} = file:consult(RelFile),
    {RelName, [App || {App, _Version} <- Apps]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
temp_dir(BaseDir) -> filename:join([BaseDir, ?TEMP_DIR]).

%%------------------------------------------------------------------------------
%% @private
%% Returns a list of the `lib' and `deps' directories used by the current
%% project/application. This includes the path configured as `deps_dir' and the
%% paths specified as `lib_dirs' in the `rebar.config' file.
%%------------------------------------------------------------------------------
dep_dirs(BaseDir, Config) ->
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
%%  + rebar_escript_plugin_main.beam
%%  + rebar_escript_plugin_runner.beam
%%  + application-version/ebin
%%  + application-version/priv
%%  + dependency1-version/ebin
%%  + dependency1-version/priv
%%  + dependency2-version/ebin
%%  + dependency2-version/priv
%%  + ...
%%
%%------------------------------------------------------------------------------
create_ez(TempDir, Config, AppFile, PackagedApps) ->
    Apps = string:join([atom_to_list(App) || App <- PackagedApps], ","),
    Paths = filename:join(["{" ++ Apps ++ "}-*", "{ebin,priv}"]),
    {ok, {_, Archive}} =
        zip:create(
          app_name_and_vsn(Config, AppFile) ++ ".ez",
          filelib:wildcard(Paths, TempDir) ++
              filelib:wildcard("*.beam", TempDir),
          [{cwd, TempDir}, {uncompress, all}, memory]),
    Archive.

%%------------------------------------------------------------------------------
%% @private
%% Returns the path to the escript to create. On windows systems the file will
%% have the extension `.escript', unix systems will omit the extension.
%%------------------------------------------------------------------------------
escript_path(OsFamily, BaseDir, EScript) ->
    Extension = escript_extension(OsFamily),
    filename:join([BaseDir, EScript ++ Extension]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
create_escript(Ez, BaseDir, Config, EScript) ->
    {OsFamily, _OsName} = os:type(),
    Path = escript_path(OsFamily, BaseDir, EScript),
    HeartArg = "-heart",
    MainArg = "-escript main " ++ atom_to_list(?MAIN_MODULE),
    EmuArgs = string:join([HeartArg, MainArg, get_emu_args(Config)], " "),
    Sections = [shebang, comment, {emu_args, EmuArgs}, {archive, Ez}],
    ok = escript:create(Path, Sections),
    ok = set_executable(OsFamily, Path).

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
main_path(TempDir, Module) ->
    filename:join([TempDir, atom_to_list(Module) ++ ".beam"]).

%%------------------------------------------------------------------------------
%% @private
%% Writes the code of the runner module into the temporary plugin directory.
%% This code will be needed at runtime to setup the correct environment.
%%------------------------------------------------------------------------------
prepare_runner_module(TempDir) ->
    Module = rebar_escript_plugin_runner,
    {Module, Binary, _} = code:get_object_code(Module),
    ok = file:write_file(main_path(TempDir, Module), Binary).

%%------------------------------------------------------------------------------
%% @private
%% Generates and writes the code of the main module into the temporary plugin
%% directory. This code provides the main entry point for escript execution (it
%% provides the `main/1' function).
%%------------------------------------------------------------------------------
prepare_main_module(TempDir, AppsToStart, PackagedApps) ->
    {ok, T1, _} = erl_scan:string(
                    "-module("
                    ++ atom_to_list(?MAIN_MODULE)
                    ++ ")."),
    {ok, T2, _} = erl_scan:string("-export([main/1])."),
    {ok, T3, _} = erl_scan:string(
                    "main(Args) ->"
                    "    rebar_escript_plugin_runner:main("
                    ++ lists:flatten(io_lib:format("~w", [AppsToStart]))
                    ++ ", Args, "
                    ++ lists:flatten(io_lib:format("~w", [PackagedApps]))
                    ++ ")."),
    {ok, F1} = erl_parse:parse_form(T1),
    {ok, F2} = erl_parse:parse_form(T2),
    {ok, F3} = erl_parse:parse_form(T3),
    {ok, ?MAIN_MODULE, Binary} = compile:forms([F1, F2, F3]),
    ok = file:write_file(main_path(TempDir, ?MAIN_MODULE), Binary).

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
