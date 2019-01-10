# FAQ

## Is there an easier way to manage my package sets than to edit packages.json?

Yes. For example, the [Spacchetti](https://github.com/justinwoo/spacchetti/) project uses Dhall to allow for easily maintaining a fork with separate type-checked sub-sets of packages and to allow for users to have local overrides of package sets.

## How come I can't install (some package) from the package set?

You should make sure you're using the correct [package-set release](https://github.com/purescript/package-sets/releases) and have updated the value of "set" in your `psc-package.json` file. See [The `psc-package.json` format](https://github.com/purescript/psc-package#the-psc-packagejson-format) section for more details.

## Can I add a dependency which is not in the package set?

Not right now. We might add this feature in future, but for now, consider either:

- Adding your dependency to the package set if possible, or
- Creating your own custom package set

There used to be a feature called "add-from-bower", which could modify a package set from [purescript/package-sets](https://github.com/purescript/package-sets/). This feature was removed as it proved to be very buggy and confusing for users, who thought that this feature would let them install arbitrary bower dependencies. See <https://github.com/purescript/psc-package/issues/121>.

## Why is Add-From-Bower gone?

The feature named "add-from-bower" never did what users expected it to do: to install packages to a project from Bower.

Instead, what this feature did was to add packages to a package set from [purescript/package-sets](https://github.com/purescript/package-sets/). However, due to the buggy nature of the feature, it was agreed that the feature should be removed and left up to other tools to handle. See <https://github.com/purescript/psc-package/issues/121>, where the top post describes how the [Spacchetti](https://github.com/spacchetti/spacchetti) package set adds Bower dependencies to the package set.

If you really would like to mix Psc-Package and Bower dependencies, you have two "real" options today. However, these will break if your packages are not all compatible, because this defeats the guarantees of having a package set.

1. Install the Bower dependencies anyway, and then only source the globs for individual dependencies, so you do not have conflicting definitions of other common modules. You can do this by supplying pass-through arguments: `psc-package build -- 'bower_componenents/purescript-my-lib/src/**/*.purs'`
2. Use the "Bower style" installation of Psc-Package2Nix: <https://github.com/justinwoo/psc-package2nix/tree/master/test-bower-style>. This will require that you use a system that can use the [Nix](https://nixos.org/nix/) package manager. See the link for more details.

## Why are my changes not updated in my package set?

Package sets are cached based on a git reference (e.g. tag or branch)
to the project directory `.psc-package`. If you are making changes to
a package set and reusing the package reference then you will need to
clear the cache for the changes to take effect.

```
$ rm -rf .psc-package
$ psc-package install
```

## Can I use Psc-Package with Nix?

Yes, now there is a solution for using Psc-Package with Nix: <https://github.com/justinwoo/psc-package2nix>. Please file issues in that project if you run into any.
