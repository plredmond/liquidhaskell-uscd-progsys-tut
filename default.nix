{ config ? { allowBroken = true; }, ... }:
let
  # fetch pinned version of nixpkgs
  nixpkgs = import (
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs-channels/archive/674ab2dffa58dab8b6a97a26a0156133dbd90378.tar.gz";
      sha256 = "1vh0npx1xkr1lwgdmg7zlai7mhl0w40h888qrfv5a1qcyp8x8d6p";
    }
  ) { inherit config; };
  # fetch pinned version of liquidhaskell
  lh = nixpkgs.fetchFromGitHub {
    owner = "ucsd-progsys";
    repo = "liquidhaskell";
    rev = "3bc467162ba285bf2c1529dafce21a20bb9aab8e";
    sha256 = "0qcmadfpanf9ivbf25kwrpggq515dq90alhnm7zm5qn8yg2qw7xg";
    fetchSubmodules = true; # liquid-fixpoint is a submodule
  };
  # make sure a haskell package has z3 at test-time and at run-time
  withZ3 = pkg: nixpkgs.haskell.lib.overrideCabal pkg (old: { buildTools = old.buildTools or [] ++ [ nixpkgs.z3 ]; });
  # override haskell compiler version and dependencies in nixpkgs
  haskellPackages = nixpkgs.haskell.packages."ghc8101".override (
    old: {
      all-cabal-hashes = nixpkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/0bd3bb7eb8c998872a4b1a0ee83d8c619228ac2a.tar.gz";
        sha256 = "00x1830xqlpadc8zyn90yw6h19ir55l8nz20i36j0aky0c1dgkfm";
      };
      overrides = self: super: with nixpkgs.haskell.lib; rec {
        # parts of liquidhaskell are not yet on hackage, and the hackage version is old, so here we build all needed components from source
        liquid-base = withZ3 (self.callCabal2nix "liquid-base" (lh + "/liquid-base") { inherit liquid-ghc-prim; inherit liquidhaskell; });
        liquid-ghc-prim = withZ3 (self.callCabal2nix "liquid-ghc-prim" (lh + "/liquid-ghc-prim") { inherit liquidhaskell; });
        liquidhaskell = dontCheck (self.callCabal2nix "liquidhaskell" lh { inherit liquid-fixpoint; });
        liquid-fixpoint = self.callCabal2nix "liquid-fixpoint" (lh + "/liquid-fixpoint") {};
        # some dependencies of liquidhaskell had problems with version ranges or tests
        ChasingBottoms = doJailbreak super.ChasingBottoms;
        Diff = dontCheck super.Diff;
        hashable = self.callHackage "hashable" "1.3.0.0" {}; # ouch
        inspection-testing = self.callHackage "inspection-testing" "0.4.2.4" {};
        optics = self.callHackage "optics" "0.3" {};
        optics-core = self.callHackage "optics-core" "0.3" {};
        optics-extra = self.callHackage "optics-extra" "0.3" {};
        optics-th = self.callHackage "optics-th" "0.3" {};
        tasty-rerun = doJailbreak super.tasty-rerun;
        text-format = doJailbreak super.text-format;
        # also fix doctest
        #doctest =
        #  (self.callHackageDirect { pkg = "doctest"; ver = "0.16.3"; sha256 = "11py87v0w70x60l2a9grv2vm2kfacczdxhn0rkyvisa4fsan936j"; } {});
      };
    }
  );
  # function to bring in devtools
  devtools = old: { nativeBuildInputs = with nixpkgs.pkgs; old.nativeBuildInputs ++ [ cabal-install ghcid ]; }; # ghc and hpack are automatically included
  # ignore files specified by gitignore
  source = nixpkgs.nix-gitignore.gitignoreSource [] ./.;
  # use overridden-haskellPackages to call gitignored-source and produce either package or env
  drv = withZ3 (haskellPackages.callCabal2nix "abcexample" source {});
in
if nixpkgs.lib.inNixShell then drv.env.overrideAttrs devtools else drv
