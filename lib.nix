{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, overlays ? []
}:

let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix {
    sourcesOverride = sources;
  };
  haskellNix = import sources."haskell.nix";
  args = haskellNix // {
    inherit system crossSystem;
    overlays = (haskellNix.overlays or []) ++ overlays;
    config = (haskellNix.config or {}) // config;
  };
  nixpkgs = import sources.nixpkgs;
  pkgs = nixpkgs args;
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs;
    src = ./.;
  };
  lib = pkgs.lib;
  niv = (import sources.niv {}).niv;
  isCardanoProxy = with lib; package:
    (package.isHaskell or false) &&
      ((hasPrefix "cardano-byron-proxy" package.identifier.name) ||
       (elem package.identifier.name [ "text-class" "bech32" ]));
  filterCardanoPackages = pkgs.lib.filterAttrs (_: package: isCardanoProxy package);
  getPackageChecks = pkgs.lib.mapAttrs (_: package: package.checks);
in lib // iohkNix.cardanoLib // {
  inherit (pkgs.haskell-nix.haskellLib) collectComponents;
  inherit
    niv
    sources
    haskellPackages
    pkgs
    iohkNix
    isCardanoProxy
    getPackageChecks
    filterCardanoPackages;
}
