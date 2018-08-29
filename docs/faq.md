# FAQ

## Is there an easier way to manage my package sets than to edit packages.json?

Yes. For example, the [Spacchetti](https://github.com/justinwoo/spacchetti/) project uses Dhall to allow for easily maintaining a fork with separate type-checked sub-sets of packages and to allow for users to have local overrides of package sets.

## How come I can't install (some package) from the package set?

You should make sure you're using the correct [package-set release](https://github.com/purescript/package-sets/releases) and have updated the value of "set" in your `psc-package.json` file. See [The `psc-package.json` format](https://github.com/purescript/psc-package#the-psc-packagejson-format) section for more details.

## Can I add a dependency which is not in the package set?

Not right now. We might add this feature in future, but for now, consider either:

- Adding your dependency to the package set if possible, or
- Creating your own custom package set

## Why are my changes not updated in my package set?

Package sets are cached based on a git reference (e.g. tag or branch)
to the project directory `.psc-package`. If you are making changes to
a package set and reusing the package reference then you will need to
clear the cache for the changes to take effect.

```
$ rm -rf .psc-package
$ psc-package install
```
