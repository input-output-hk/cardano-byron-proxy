{ config, pkgs, lib, options, ... }:
with lib;
let
  cfg = config.services.byron-proxy;
  name = "byron-proxy";
  stateDir = "/var/lib/byron-proxy";

in {
  options.services.byron-proxy = {
    enable = mkEnableOption name;
    environment = mkOption {
      type = types.enum (builtins.attrNames (import ../.. {}).environments);
      default = "mainnet";
    };
    proxyPort = mkOption {
      type = types.int;
      default = 7777;
    };
    proxyHost = mkOption {
      type = types.string;
      default = "127.0.0.1";
    };
  };

  config = mkIf cfg.enable {
    users = {
      users.byron-proxy = {
        description     = "byron-proxy";
        group           = "byron-proxy";
        home            = stateDir;
        createHome      = true;
      };
      groups.byron-proxy = {};
    };

    networking.firewall = {
      allowedTCPPorts = lib.optional (cfg.proxyHost != "127.0.0.1") cfg.proxyPort;
    };

    systemd.services.byron-proxy = {
      description   = "byron proxy service";
      after         = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      script = let
        customConfig = {
          proxyHost = cfg.proxyHost;
          proxyPort = cfg.proxyPort;
        };
        byronScripts = (import ../.. { inherit customConfig; }).scripts.byron;
      in ''${byronScripts.proxy.${cfg.environment}}'';
      serviceConfig = {
        User = "byron-proxy";
        Group = "byron-proxy";
        Restart = "always";
        RestartSec = 30;
        StartLimitInterval = 200;
        StartLimitBurst = 5;
        KillSignal = "SIGINT";
        WorkingDirectory = stateDir;
        PrivateTmp = true;
      };
    };
  };
}
