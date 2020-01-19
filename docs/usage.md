# Using Psc-Package from your project

## Frequently used commands

```shell
# install or update the dependencies listed in psc-package.json
$ psc-package install

# install or update the package and add it to psc-package.json if not listed
$ psc-package install <package>

# list available commands
$ psc-package --help
```

## Create a project

A new package can be created using `psc-package init`. This will:

- Create a simple `psc-package.json` file based on the current compiler version
- Add the Prelude as a dependency (this can be removed later)
- Sync the local package database (under the `.psc-package/` directory) by cloning any necessary repositories.

## Add dependencies

To add a dependency, either:

- Use the `install <package name>` command, which will update the project configuration automatically, or
- Modify the `psc-package.json` file, and sync manually by running the `install` command (previously `update`).

## Build a project

Active project dependencies and project source files under `src` can be compiled using the `build` command.

This command is provided as a convenience until external tools add support for `psc-package`. It _might_ be removed in future.

## Query the local package database

The local package database can be queried using the following commands:

- `sources` - list source directories for active package versions. This can be useful when building a command for, say, running PSCi.
- `dependencies` - list all transitive dependencies

## Local package sets

In `psc-package.json`, you can set `"source"` to be a path to a local file:

```json
{
  "name": "name",
  "set": "local",
  "source": "packages.json",
  "depends": [
    "aff"
  ]
}
```

From here, you can generate a local packages.json file in any way you please and use this package set directly. Consider if you use Dhall:

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.5-20200103/packages.dhall sha256:0a6051982fb4eedb72fbe5ca4282259719b7b9b525a4dda60367f98079132f30

in    upstream
    ⫽ { calpis =
          { dependencies = [ "prelude" ]
          , repo = "https://github.com/justinwoo/purescript-calpis.git"
          , version = "v0.1.0"
          }
      }
```

This definition takes an existing release and adds the "calpis" package. Then we can generate a package set from this by running Dhall-JSON:

> dhall-to-json --file packages.dhall --output packages.json

Then we can install this package expected:

```bash
$ psc-package install calpis
Installing calpis
psc-package.json file was updated

$ cat psc-package.json
{
  "name": "name",
  "set": "local",
  "source": "packages.json",
  "depends": [
    "aff",
    "calpis"
  ]
}
```
