%%%-------------------------------------------------------------------
%%% @doc
%%% Commands implementation for the Erlang version manager.
%%% @end
%%%-------------------------------------------------------------------
-module(e_commands).
-export([
    list_versions/0,
    current_version/0,
    install_version/1,
    use_version/1,
    uninstall_version/1
]).

-define(E_HOME, ".e").
-define(VERSIONS_DIR, "versions").

%% @doc List installed Erlang versions
list_versions() ->
    VersionsDir = get_versions_dir(),
    case filelib:is_dir(VersionsDir) of
        true ->
            case file:list_dir(VersionsDir) of
                {ok, Versions} ->
                    case Versions of
                        [] ->
                            io:format("No Erlang versions installed.~n"),
                            io:format("Use 'e install <version>' to install a version.~n");
                        _ ->
                            CurrentVersion = get_current_version(),
                            io:format("Installed Erlang versions:~n"),
                            lists:foreach(fun(V) ->
                                Marker = case V of
                                    CurrentVersion -> " * ";
                                    _ -> "   "
                                end,
                                io:format("~s~s~n", [Marker, V])
                            end, lists:sort(Versions))
                    end;
                {error, Reason} ->
                    io:format("Error listing versions: ~p~n", [Reason])
            end;
        false ->
            io:format("No Erlang versions installed.~n"),
            io:format("Use 'e install <version>' to install a version.~n")
    end.

%% @doc Show the current active Erlang version
current_version() ->
    case get_current_version() of
        undefined ->
            io:format("No Erlang version currently active.~n"),
            io:format("Use 'e use <version>' to activate a version.~n");
        Version ->
            io:format("Current Erlang version: ~s~n", [Version])
    end.

%% @doc Install a specific Erlang version
install_version(Version) ->
    io:format("Installing Erlang ~s...~n", [Version]),
    VersionsDir = get_versions_dir(),
    VersionDir = filename:join(VersionsDir, Version),
    
    case filelib:is_dir(VersionDir) of
        true ->
            io:format("Erlang ~s is already installed.~n", [Version]);
        false ->
            % Create the versions directory if it doesn't exist
            ok = filelib:ensure_dir(filename:join(VersionDir, "dummy")),
            
            % For a basic implementation, we'll create a placeholder
            % In a real implementation, this would download and compile Erlang
            io:format("~n"),
            io:format("NOTE: This is a basic version manager implementation.~n"),
            io:format("To install Erlang ~s, you would typically:~n", [Version]),
            io:format("  1. Download the source from https://erlang.org/download/otp_src_~s.tar.gz~n", [Version]),
            io:format("  2. Extract and compile the source~n"),
            io:format("  3. Install to ~~/.e/versions/~s~n~n", [Version]),
            io:format("For now, creating a placeholder installation...~n"),
            
            % Create a marker file to indicate this version is "installed"
            MarkerFile = filename:join(VersionDir, ".installed"),
            ok = file:write_file(MarkerFile, Version),
            
            io:format("Placeholder created for Erlang ~s~n", [Version]),
            io:format("Use 'e use ~s' to activate this version.~n", [Version])
    end.

%% @doc Switch to a specific Erlang version
use_version(Version) ->
    VersionsDir = get_versions_dir(),
    VersionDir = filename:join(VersionsDir, Version),
    
    case filelib:is_dir(VersionDir) of
        false ->
            io:format("Erlang ~s is not installed.~n", [Version]),
            io:format("Use 'e install ~s' to install it first.~n", [Version]);
        true ->
            % Set the current version
            CurrentFile = get_current_file(),
            ok = filelib:ensure_dir(CurrentFile),
            case file:write_file(CurrentFile, Version) of
                ok ->
                    io:format("Now using Erlang ~s~n", [Version]),
                    io:format("~n"),
                    io:format("NOTE: To use this version in your shell, you need to:~n"),
                    io:format("  1. Add ~~/.e/versions/~s/bin to your PATH~n", [Version]),
                    io:format("  2. Or source the e shell integration script~n");
                {error, Reason} ->
                    io:format("Error setting current version: ~p~n", [Reason])
            end
    end.

%% @doc Uninstall a specific Erlang version
uninstall_version(Version) ->
    VersionsDir = get_versions_dir(),
    VersionDir = filename:join(VersionsDir, Version),
    
    case filelib:is_dir(VersionDir) of
        false ->
            io:format("Erlang ~s is not installed.~n", [Version]);
        true ->
            CurrentVersion = get_current_version(),
            case Version of
                CurrentVersion ->
                    io:format("Cannot uninstall the currently active version.~n"),
                    io:format("Use 'e use <other-version>' first.~n");
                _ ->
                    case delete_dir_recursive(VersionDir) of
                        ok ->
                            io:format("Uninstalled Erlang ~s~n", [Version]);
                        {error, Reason} ->
                            io:format("Error uninstalling version: ~p~n", [Reason])
                    end
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Get the home directory for e
get_e_home() ->
    Home = os:getenv("HOME"),
    filename:join(Home, ?E_HOME).

%% @doc Get the versions directory
get_versions_dir() ->
    filename:join(get_e_home(), ?VERSIONS_DIR).

%% @doc Get the current version file path
get_current_file() ->
    filename:join(get_e_home(), "current").

%% @doc Get the current active version
get_current_version() ->
    CurrentFile = get_current_file(),
    case file:read_file(CurrentFile) of
        {ok, Binary} ->
            string:trim(binary_to_list(Binary));
        {error, _} ->
            undefined
    end.

%% @doc Recursively delete a directory
delete_dir_recursive(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(fun(File) ->
                Path = filename:join(Dir, File),
                case filelib:is_dir(Path) of
                    true -> delete_dir_recursive(Path);
                    false -> file:delete(Path)
                end
            end, Files),
            file:del_dir(Dir);
        {error, Reason} ->
            {error, Reason}
    end.
