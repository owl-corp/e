# e

Erlang version manager - A basic CLI tool for managing multiple Erlang versions.

## Overview

`e` is a lightweight Erlang version manager that allows you to install and switch between different versions of Erlang/OTP on your system.

## Building

Build the escript:

```bash
rebar3 escriptize
```

The executable will be created at `_build/default/bin/e`.

## Installation

To make `e` available system-wide, copy the built executable to a directory in your PATH:

```bash
sudo cp _build/default/bin/e /usr/local/bin/
```

Or add the build directory to your PATH:

```bash
export PATH=$PATH:/path/to/e/_build/default/bin
```

## Usage

### List installed versions

```bash
e list
```

Shows all installed Erlang versions. The currently active version is marked with `*`.

### Install a version

```bash
e install <version>
```

Example:
```bash
e install 26.2.1
```

**Note**: This basic implementation creates a placeholder installation. In a production version manager, this would download and compile Erlang from source.

### Switch to a version

```bash
e use <version>
```

Example:
```bash
e use 26.2.1
```

Sets the specified version as the current active version.

### Show current version

```bash
e current
```

Displays the currently active Erlang version.

### Uninstall a version

```bash
e uninstall <version>
```

Example:
```bash
e uninstall 25.3
```

Removes an installed version. You cannot uninstall the currently active version.

### Help

```bash
e help
```

Shows usage information and available commands.

## Testing

Run the test suite:

```bash
rebar3 eunit
```

## How it works

`e` stores installed Erlang versions in `~/.e/versions/` and tracks the current version in `~/.e/current`.

## Future Enhancements

- Actual Erlang source download and compilation
- Pre-built binary downloads
- Shell integration for automatic PATH management
- Version aliases (e.g., `stable`, `latest`)
- List available versions from erlang.org
- Parallel compilation support

