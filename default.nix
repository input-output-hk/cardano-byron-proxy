{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, commonLib ? import ./lib.nix { inherit system crossSystem config; }
, pkgs ? commonLib.pkgs
, customConfig ? {}
, interactive ? false
, gitrev ? commonLib.iohkNix.commitIdFromGitRepoOrZero ./.git
, withHoogle ? true
}:

let
  lib = commonLib.pkgs.lib;
  inherit (commonLib) environments haskellPackages;
  cardano-byron-proxy = haskellPackages.cardano-byron-proxy.components.exes.cardano-byron-proxy;

  # scripts contains startup scripts for proxy
  scripts = import ./nix/scripts.nix {
    inherit commonLib customConfig;
  };
  # NixOS tests run a proxy and validate it listens
  nixosTests = import ./nix/nixos/tests { inherit (commonLib) pkgs; };

  recRecurseIntoAttrs = with pkgs; pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
  projectPkgSet = recRecurseIntoAttrs (x: with pkgs; lib.isAttrs x && !lib.isDerivation x)
    # we are only intersted in listing the project packages
    (pkgs.lib.filterAttrs (n: p: p != null && (pkgs.haskell-nix.haskellLib.selectProjectPackages p))
      # from our project which is based on a cabal project.
      haskellPackages);

  self = with commonLib; {
    inherit scripts nixosTests environments cardano-byron-proxy;
    haskellPackages = projectPkgSet;
    inherit (iohkNix) check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isCardanoNode haskellPackages;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (filterCardanoPackages haskellPackages));
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isCardanoNode haskellPackages;

    shell = haskellPackages.shellFor {

      # Builds a Hoogle documentation index of all dependencies,
      # and provides a "hoogle" command to search the index.
      inherit withHoogle;

      # You might want some extra tools in the shell (optional).
      buildInputs = (with haskellPackages; [
        #weeder.components.exes.weeder
        #hlint.components.exes.hlint
        #cabal-install.components.exes.cabal
        #ghcid.components.exes.ghcid
      ]) ++ (with pkgs; [
        pkgconfig
        sqlite-interactive
        tmux
        commonLib.cabalProjectRegenerate
      ]);

      # Prevents cabal from choosing alternate plans, so that
      # *all* dependencies are provided by Nix.
      exactDeps = true;
    };

  };

in self
