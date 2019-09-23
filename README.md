# DEPRECATED :warning:

See [spago](https://github.com/purescript/spago) which provides the same functionality and is updated regularly.

<hr>

# `psc-package`

[![Build Status](https://travis-ci.org/purescript/psc-package.svg?branch=master)](https://travis-ci.org/purescript/psc-package)

`psc-package` is an executable which helps manage PureScript dependencies via Git. It can be used directly, but it is also designed to be used by external tools.

**See the [guide](https://psc-package.readthedocs.io/en/latest/) to learn how to use Psc-Package.**

## Installation

- Download the binary for your platform from [the releases page](https://github.com/purescript/psc-package/releases), and copy it somewhere on your PATH, or
- Build from source, using `stack install`.

If you're a **Windows Chocolatey** user, then you can install `psc-package` from the [official repo](https://chocolatey.org/packages/psc-package):

```
$ choco install psc-package
```

## Usage

```shell
# install or update the dependencies listed in psc-package.json
$ psc-package install

# install or update the package and add it to psc-package.json if not listed
$ psc-package install <package>

# list available commands
$ psc-package --help
```

## Design Goals

- `psc-package` should enable simple package management without the need to run Node (compare with Bower).
- `psc-package` should enable reproducible builds, at least as far as reproducing PureScript dependencies.
- `psc-package` should have a good out-of-the-box user experience (compare with Stack in Haskell), while still supporting custom package sets for advanced use cases.
- `psc-package` should only depend on tools which are available on all systems (for example, the Git client)
