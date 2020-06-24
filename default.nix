let
  # vnixpinupdater versions.json nixpkgs nixos-20.03
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) {};
  drv = nixpkgs.haskellPackages.callCabal2nix "abcexample" ./. {};
in if nixpkgs.lib.inNixShell then drv.env else drv