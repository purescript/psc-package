# Installation

## Any platform

You can install Psc-Package on any platform by downloading the binary for your platform from [the releases page](https://github.com/purescript/psc-package/releases) and copying it somewhere on your PATH.

## npm

You can install Psc-Package through the npm package: <https://www.npmjs.com/package/psc-package>

```
# globally
npm i -g psc-package

# for your project
npm i -S psc-package
```

This should work on Linux, OSX, and Windows. Please report issues at <https://github.com/justinwoo/npm-psc-package-bin-simple> if this does not work as expected.

## Linux/OSX

### Nix

You should be able to use the derivation provided in nixpkgs: <https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/compilers/purescript/psc-package/default.nix>.

If you're not on NixOS, you might use `nix-env -i psc-package`.

## Windows

If you're a **Windows Chocolatey** user, then you can install `psc-package` from the [official repo](https://chocolatey.org/packages/psc-package):

```
$ choco install psc-package
```

## Travis

```yaml
language: c
dist: trusty
sudo: required

cache:
  directories:
  - .psc-package
  - output

env:
  - PATH=$HOME/purescript:$HOME/psc-package:$PATH

install:
  - TAG=v0.12.0
  - PSC_PACKAGE_TAG=v0.4.1
  - wget -O $HOME/purescript.tar.gz https://github.com/purescript/purescript/releases/download/$TAG/linux64.tar.gz
  - tar -xvf $HOME/purescript.tar.gz -C $HOME/
  - chmod a+x $HOME/purescript
  - wget -O $HOME/psc-package.tar.gz https://github.com/purescript/psc-package/releases/download/$PSC_PACKAGE_TAG/linux64.tar.gz
  - tar -xvf $HOME/psc-package.tar.gz -C $HOME/
  - chmod a+x $HOME/psc-package

script:
  - ./travis.sh
```

See <https://github.com/purescript/package-sets/blob/6f9f0b0eaea5e3718c860bc0cbaa651a554aad21/.travis.yml>
