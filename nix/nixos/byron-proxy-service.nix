{ config
, lib
, ... }:

with lib;
let
  cfg = config.services.byron-proxy;
in {

  options = {
    services.byron-proxy = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable byron-proxy, a node proxying cardano-sl to byron-proxy
        '';
      };
    };
  };

  config = mkIf cfg.enable ( let stateDirBase = "/var/lib/"; in {
    systemd.services.byron-proxy = {
      description   = "byron-proxy node service";
      after         = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      script = cfg.script;
      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
        WorkingDirectory = cfg.stateDir;
        # This assumes /var/lib/ is a prefix of cfg.stateDir.
        # This is checked as an assertion below.
        StateDirectory =  lib.removePrefix stateDirBase cfg.stateDir;
      };
    };
    assertions = [{
      assertion = lib.hasPrefix stateDirBase cfg.stateDir;
      message =
        "The option services.byron-proxy.stateDir should have ${stateDirBase} as a prefix!";
    }];
  });
}
