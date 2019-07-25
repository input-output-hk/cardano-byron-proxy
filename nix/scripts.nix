{
  commonLib,
  customConfig,
  cardanoConfig,
  environments,
  nixTools
}:

let
  byronProxy = nixTools.nix-tools.exes.cardano-byron-proxy;

in {
  byron = {
    proxy = import ./byron-proxy-scripts.nix {
      inherit commonLib environments byronProxy customConfig cardanoConfig;
    };
    validator = import ./byron-validator-scripts.nix {
      inherit commonLib byronProxy environments customConfig cardanoConfig;
    };
  };
}
