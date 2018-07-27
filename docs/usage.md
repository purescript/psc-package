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
