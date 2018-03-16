# `psc-package`

[![Build Status](https://travis-ci.org/purescript/psc-package.svg?branch=master)](https://travis-ci.org/purescript/psc-package)

`psc-package` is an executable which helps manage PureScript dependencies via Git. It can be used directly, but it is also designed to be used by external tools.

## Installation

- Download the binary for your platform from [the releases page](https://github.com/purescript/psc-package/releases), and copy it somewhere on your PATH, or
- Build from source, using `stack install`.

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

## Concepts

### Package Sets

A _package set_ is a mapping from package names to:

- the Git repository URL for the package
- the Git ref which should be passed to `git clone` to clone the appropriate version (usually a tag name, but a SHA is also valid)
- the package's transitive dependencies

A package set repository contains a `packages.json` file which contains all mapping information. `psc-package` uses this information to decide which repos need to be cloned.

The default package set is [purescript/package-sets](https://github.com/purescript/package-sets), but it is possible to create custom package sets by forking an existing package set or creating a new one from scratch. One benefit of using the default package set is that it is verified by a continuous integration process.

## The `psc-package.json` format

Here is a simple project configuration:

```json
{
    "name": "my-project",
    "set": "psc-0.10.2",
    "source": "https://github.com/purescript/package-sets.git",
    "depends": [
        "prelude"
    ]
}
```

It defines:

- The project name
- The package set to use to resolve dependencies (this corresponds to a branch or tag of the package set source repository)
- The package set source repository Git URL (change this if you want to host your own package sets)
- Any dependencies of the project, as a list of names of packages from the package set

## How To

### Create a project

A new package can be created using `psc-package init`. This will:

- Create a simple `psc-package.json` file based on the current compiler version
- Add the Prelude as a dependency (this can be removed later)
- Sync the local package database (under the `.psc-package/` directory) by cloning any necessary repositories.

### Add dependencies

To add a dependency, either:

- Use the `install <package name>` command, which will update the project configuration automatically, or
- Modify the `psc-package.json` file, and sync manually by running the `install` command (previously `update`).

### Build a project

Active project dependencies and project source files under `src` can be compiled using the `build` command.

This command is provided as a convenience until external tools add support for `psc-package`. It _might_ be removed in future.

### Query the local package database

The local package database can be queried using the following commands:

- `sources` - list source directories for active package versions. This can be useful when building a command for, say, running PSCi.
- `dependencies` - list all transitive dependencies

### Add a package to the package set

Adding your package to the package set means that others can easily install it as a dependency.

Please note that your package will be removed from the set if it is not kept up to date. It can be easily re-added later if this happens.

Adding a package is a manual process right now. We would like to add commands to make this process simpler, but for now, please follow these steps:

- go to the [package-sets repository](https://github.com/purescript/package-sets) and fork the repository.
- open the `packages.json` file and make a new entry to add your package, copying the format used for existing packages. The key will be the package name without the preceding `purescript-` as in Bower packages. It should have three fields defined:

* `dependencies` - a list of the dependencies used for this package
* `repo` - a git url for the package. We most often use the format `https://github.com/{user}/purescript-{project-name}.git`
* `version` - the git tag that will be used, using the format `v{Major}.{Minor}.{Patch}`

- when you have added your package, you will want to test this.

First, you will need to create an empty `psc-package.json` to test the package set in use.

`echo '{ "name": "", "set": "testing", "source": "", "depends": [] }' > psc-package.json`

Make the required directory structure for the package sets:

`mkdir -p .psc-package/testing/.set`

Then copy over `packages.json` into the directory:

`cp packages.json .psc-package/testing/.set/packages.json`

- after this setup, you can use the `verify` command of psc-package, e.g. `psc-package verify aff`. This will verify the package and its reverse dependencies.

If this builds correctly, you can then push up this branch and make a pull request. Travis will verify your package builds correctly, and then we will try to merge your pull request. Your package will then be available in the next tagged package set.

### Formatting the package set

When creating your pull request, make sure to run the `format` command to pretty-print the packages file. This helps us avoid problems in the future with git diffs and so on.

### Update a package in the set

Similar to the above, you will need to do the setup. You can then modify the version field to the version you wish to use. Once you have updated the package set, run the copy command and verify the package you have modified.

Then you can make a pull request. Again, once Travis verifies your change, we will merge it into `master` and your change will be available in the next tag.

### Add a package from Bower

If you have Bower installed on your system, you can easily add a package from Bower to the package set. Use the Bower package name to add the package using the `add-from-bower` command:

```
psc-package add-from-bower <purescript-something>
```

## FAQ

### Can I add a dependency which is not in the package set?

Not right now. We might add this feature in future, but for now, consider either:

- Adding your dependency to the package set if possible, or
- Creating your own custom package set

### Why are my changes not updated in my package set?

Package sets are cached based on a git reference (e.g. tag or branch)
to the project directory `.psc-package`. If you are making changes to
a package set and reusing the package reference then you will need to
clear the cache for the changes to take effect.

```
$ rm -rf .psc-package
$ psc-package install
```
