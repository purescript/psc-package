# Introduction

## What is a Package Set?

A _package set_ is a mapping from package names to:

- the Git repository URL for the package
- the Git ref which should be passed to `git clone` to clone the appropriate version (usually a tag name, but a SHA is also valid)
- the package's transitive dependencies

A package set repository contains a `packages.json` file which contains all mapping information. `psc-package` uses this information to decide which repos need to be cloned.

The default package set is [purescript/package-sets](https://github.com/purescript/package-sets), but it is possible to create custom package sets in many ways:

* by preparing a package set with Dhall using the packages.dhall from releases: <https://github.com/purescript/package-sets/releases>
* forking `purescript/package-sets` to your own repository
* creating a new one from scratch

. One benefit of using the default package set is that it is verified by a continuous integration process.

## The `psc-package.json` format

Here is a simple project configuration:

```json
{
    "name": "my-project",
    "set": "psc-0.13.6",
    "source": "https://github.com/purescript/package-sets.git",
    "depends": [
        "prelude"
    ]
}
```

It defines:

- The project name
- The package set to use to resolve dependencies. This corresponds to a branch or tag of the package set source repository if you use a Git URL as your source. Otherwise it serves as just an identifier.
- The package set source, which is either a repository Git URL or a path to a package set `packages.json` file.
- Any dependencies of the project, as a list of names of packages from the package set
