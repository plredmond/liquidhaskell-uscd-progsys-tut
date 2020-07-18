* On this branch there are two modules, `A` and `B`.
* `A` uses `{-# LANGUAGE PackageImports #-}` to `import "liquid-base" Prelude`.
* `B` contains an invalid annotation (a value 3 is annotated as not being 3).

EXPECTED:

* LiquidHaskell should check both modules and find the incorrect annotation in
  `B`.
* The build should fail.

ACTUAL:

* `nix-build`: About 75% of the time LiquidHaskell reports SAFE and "0 constraints checked" and the build succeeds. The rest of the time it fails, as expected.
* `nix-shell --run 'cabal v2-build'`: The build fails, as expected.
