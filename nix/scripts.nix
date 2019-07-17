{
  commonLib,
  customConfig,
  nixTools
}:

let
  cardanoRev = (builtins.fromJSON (builtins.readFile ./old-cardano-sl-src.json)).rev;
  oldCardanoSrc = import ./old-cardano.nix {
    inherit commonLib;
  };
  cardanoConfig = (import oldCardanoSrc { gitrev = cardanoRev;}).cardanoConfig;
  byronProxy = nixTools.nix-tools.exes.cardano-byron-proxy;

in {
  byron = {
    proxy = import ./byron-proxy-scripts.nix {
      inherit commonLib oldCardanoSrc byronProxy customConfig cardanoConfig;
    };
    validator = import ./byron-validator-scripts.nix {
      inherit commonLib byronProxy oldCardanoSrc customConfig cardanoConfig;
    };
  };
}
