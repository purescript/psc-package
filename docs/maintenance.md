# Working with package sets

## Add a package to the package set

Adding your package to the package set means that others can easily install it as a dependency.

Please note that your package will be removed from the set if it is not kept up to date. It can be easily re-added later if this happens.

Adding a package is a manual process right now. We would like to add commands to make this process simpler, but for now, please follow these steps:

- go to the [package-sets repository](https://github.com/purescript/package-sets) and fork the repository.
- open the `packages.json` file and make a new entry to add your package, copying the format used for existing packages. The key will be the package name without the preceding `purescript-` as in Bower packages. It should have three fields defined:

* `dependencies` - a list of the dependencies used for this package
* `repo` - a git url for the package. We most often use the format `https://github.com/{user}/purescript-{project-name}.git`
* `version` - the git tag that will be used, using the format `v{Major}.{Minor}.{Patch}`

- run `./travis.sh <PACKAGE>`, e.g. `./travis.sh aff`, to verify your package set changes.

If this builds correctly, you can then push up this branch and make a pull request. Travis will verify your package builds correctly, and then we will try to merge your pull request. Your package will then be available in the next tagged package set.

## Formatting the package set

When creating your pull request, make sure to run the `format` command to pretty-print the packages file. This helps us avoid problems in the future with git diffs and so on.

## Update a package in the set

Similar to the above, you will need to do the setup. You can then modify the version field to the version you wish to use. Once you have updated the package set, run the copy command and verify the package you have modified.

Then you can make a pull request. Again, once Travis verifies your change, we will merge it into `master` and your change will be available in the next tag.

## Alternative package sets

You may also be interested in using Spacchetti, a package set project using Dhall to define and work with package sets: <https://github.com/justinwoo/spacchetti>.

It also defines ways to define a local package set that inherits from a remote one, so you can define local overrides and additions to your package set easily.
