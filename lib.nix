let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix { sourcesOverride = sources; };
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
in lib // iohkNix.cardanoLib // iohkNix // { inherit sources; }
