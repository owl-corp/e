%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for e_commands module.
%%% @end
%%%-------------------------------------------------------------------
-module(e_commands_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_HOME, "/tmp/e_test_home").
-define(VERSIONS_DIR, filename:join(?TEST_HOME, ".e/versions")).
-define(CURRENT_FILE, filename:join(?TEST_HOME, ".e/current")).

setup() ->
    % Clean up any previous test data
    os:cmd("rm -rf " ++ ?TEST_HOME),
    % Set HOME to test directory
    os:putenv("HOME", ?TEST_HOME),
    ok.

cleanup(_) ->
    % Clean up test data
    os:cmd("rm -rf " ++ ?TEST_HOME),
    ok.

%% Test that install creates a directory
install_version_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         e_commands:install_version("25.3"),
         VersionDir = filename:join(?VERSIONS_DIR, "25.3"),
         ?assert(filelib:is_dir(VersionDir)),
         ?assert(filelib:is_file(filename:join(VersionDir, ".installed")))
     end}.

%% Test installing same version twice
install_duplicate_version_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         e_commands:install_version("25.3"),
         e_commands:install_version("25.3"),
         VersionDir = filename:join(?VERSIONS_DIR, "25.3"),
         ?assert(filelib:is_dir(VersionDir))
     end}.

%% Test setting current version
use_version_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         e_commands:install_version("25.3"),
         e_commands:use_version("25.3"),
         ?assert(filelib:is_file(?CURRENT_FILE)),
         {ok, Content} = file:read_file(?CURRENT_FILE),
         ?assertEqual(<<"25.3">>, string:trim(Content))
     end}.

%% Test uninstalling a version
uninstall_version_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         e_commands:install_version("25.3"),
         e_commands:install_version("26.2.1"),
         e_commands:use_version("26.2.1"),
         e_commands:uninstall_version("25.3"),
         VersionDir = filename:join(?VERSIONS_DIR, "25.3"),
         ?assertNot(filelib:is_dir(VersionDir))
     end}.

%% Test listing multiple versions
list_dir_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         e_commands:install_version("25.3"),
         e_commands:install_version("26.2.1"),
         {ok, Versions} = file:list_dir(?VERSIONS_DIR),
         ?assertEqual(2, length(Versions)),
         ?assert(lists:member("25.3", Versions)),
         ?assert(lists:member("26.2.1", Versions))
     end}.

