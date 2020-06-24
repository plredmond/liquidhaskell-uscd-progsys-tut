# How to use

* develop in `nix-shell` with `cabal v2-*` commands
    * do not run `cabal v2-*` commands outside of `nix-shell`, because that will do separate dependency resolution which may be distinct from nix and may break
    * do not run `nix-build` inside `nix-shell` because that won't produce a `result` containing your project
* release with `nix-build`

this pattern, along with the contents of `versions.json`, `default.nix` etc is borrowed from [vaibhavsagar](http://github.com/vaibhavsagar) who writes about an earlier version of this in [Quick and Easy Nixpkgs Pinning](https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/)

# Doctest sharp edges

tests are implemented with doctest, which currently requires `.ghc.environment*` files to be present..
as of cabal 3, `.ghc.environment*` files aren't written by default and you'll need to add a snippet to either `~/.cabal/config` or `cabal.project.local`: `write-ghc-environment-files: always`..
to test, you'll need to run cabal once to write out the `.ghc.environment*` file, during which the tests will fail, and a second time to actually run the tests..

however, the presence of the `ghc.environment*` files can break `nix-build` and so we use `nix-gitignore` to generate the list passed to `callCabal2nix` in `default.nix`..

# Git hook

you might also want a pre-commit hook to run tests

`.git/hooks/pre-commit`
```sh
#!/usr/bin/env bash
set -e -u -o pipefail
set -x
if [ -n "${IN_NIX_SHELL:-}" ]; then
    # v2-build must run first to write out a .ghc.environment* file for doctest to use
    cabal v2-build
    cabal v2-test
else
    # if a .ghc.environment* file is around, it must be ignored or it will break the isolated nix-build
    nix-build
fi
```
