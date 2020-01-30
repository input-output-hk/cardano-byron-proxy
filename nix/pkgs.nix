{ pkgs
, src
, haskellCompiler ? "ghc865"
}:
let

  haskell = pkgs.haskell-nix;

  # TODO: move to iohk-nix
  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = dir:  with pkgs.lib; let
      isFiltered = src ? _isLibCleanSourceWith;
      origSrc = if isFiltered then src.origSrc else src;
    in cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        hasPrefix (toString origSrc + toString dir) path;
    } + dir;

 in pkgs.haskell-nix.cabalProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit { inherit src; };
          ghc = pkgs.haskell-nix.compiler.${haskellCompiler};

    modules = [
      {
        doHaddock = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      }
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.components.library.doExactConfig = true;
        packages.typed-protocols.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim.configureFlags = [ "--ghc-option=-Werror" ];
        packages.io-sim-classes.configureFlags = [ "--ghc-option=-Werror" ];
        packages.prometheus.components.library.doExactConfig = true;
      }
    ];
  }
