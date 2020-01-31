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
  self = with commonLib; {
    inherit scripts nixosTests environments cardano-byron-proxy haskellPackages;
    inherit (iohkNix) check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isCardanoProxy haskellPackages;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (filterCardanoPackages haskellPackages));
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isCardanoProxy haskellPackages;

    shell = haskellPackages.shellFor {

      packages = ps: with ps; [
        haskellPackages.cardano-byron-proxy
      ];

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
