%%%-------------------------------------------------------------------
%%% @doc
%%% Main entry point for the 'e' Erlang version manager CLI.
%%% @end
%%%-------------------------------------------------------------------
-module(e).
-export([main/1]).

%% @doc Main entry point for escript
main(Args) ->
    case Args of
        [] ->
            print_help(),
            halt(0);
        ["help"] ->
            print_help(),
            halt(0);
        ["--help"] ->
            print_help(),
            halt(0);
        ["-h"] ->
            print_help(),
            halt(0);
        ["list"] ->
            e_commands:list_versions(),
            halt(0);
        ["current"] ->
            e_commands:current_version(),
            halt(0);
        ["install", Version] ->
            e_commands:install_version(Version),
            halt(0);
        ["use", Version] ->
            e_commands:use_version(Version),
            halt(0);
        ["uninstall", Version] ->
            e_commands:uninstall_version(Version),
            halt(0);
        _ ->
            io:format("Unknown command. Use 'e help' for usage information.~n"),
            halt(1)
    end.

%% @doc Print help information
print_help() ->
    io:format("e - Erlang Version Manager~n~n"),
    io:format("Usage: e <command> [options]~n~n"),
    io:format("Commands:~n"),
    io:format("  list              List installed Erlang versions~n"),
    io:format("  current           Show the current active Erlang version~n"),
    io:format("  install <version> Install a specific Erlang version~n"),
    io:format("  use <version>     Switch to a specific Erlang version~n"),
    io:format("  uninstall <version> Uninstall a specific Erlang version~n"),
    io:format("  help              Show this help message~n~n"),
    io:format("Examples:~n"),
    io:format("  e list~n"),
    io:format("  e install 26.2.1~n"),
    io:format("  e use 26.2.1~n"),
    io:format("  e current~n").
