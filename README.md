FIXED:

* The original problem was present at commit: d021a33
* The problem (described below) was fixed by upgrading LH in `default.nix`

```diff
-    rev = "3bc467162ba285bf2c1529dafce21a20bb9aab8e";
+    rev = "26cad4f05171669949fd92fa5a5f584a4950ca7b";
```

---

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
