{ config
, pkgs
, ... }:

with import ../../lib.nix;
let
  cfg = config.services.byron-proxy;
  envConfig = cfg.cardanoLib.environments.${cfg.environment};
in {
  options = {
    services.byron-proxy = {
      script = mkOption {
        type = types.str;
        default = let
        in ''
          exec ${cfg.package}/bin/cardano-byron-proxy \
            +RTS -T -RTS \
            --database-path ${cfg.stateDir}/db \
            --index-path ${cfg.stateDir}/index \
            --configuration-file ${cfg.cardanoLib.cardanoConfig}/configuration.yaml \
            --configuration-key ${envConfig.confKey} \
            --topology ${cfg.topologyFile} \
            --logger-config ${cfg.logger.configFile} \
            --local-addr [${cfg.proxyHost}]:${toString cfg.proxyPort} \
            ${optionalString (cfg.pbftThreshold != null) "--pbft-threshold ${cfg.pbftThreshold}"} \
            ${optionalString (cfg.nodeId != null) "--node-id ${cfg.nodeId}"} \
            ${optionalString (cfg.listen != null) "--listen ${cfg.listen}"} \
            ${optionalString (cfg.address != null) "--address ${cfg.address}"} \
        '';
      };

      package = mkOption {
        type = types.package;
        default = (import ../nix-tools.nix {}).nix-tools.exes.cardano-byron-proxy;
        defaultText = "cardano-byron-proxy";
        description = ''
          The byron-proxy package that should be used
        '';
      };

      cardanoLib =  mkOption {
        type = types.attrs;
        default = {
          inherit cardanoConfig environments;
        };
        defaultText = "cardano-lib (from iohk-nix)";
        description = ''
         Cardano-lib to use for configuration.
        '';
      };

      environment = mkOption {
        type = types.enum (builtins.attrNames cfg.cardanoLib.environments);
        default = "testnet";
        description = ''
          environment proxy will connect to
        '';
      };

      pbftThreshold = mkOption {
        type = types.nullOr types.str;
        default = envConfig.pbftThreshold or null;
        description = ''
          PBFT Threshold
        '';
      };

      stateDir = mkOption {
        type = types.str;
        default = "/var/lib/byron-proxy/${cfg.environment}";
        description = ''
          Directory to store blockchain data.
        '';
      };

      proxyPort = mkOption {
        type = types.int;
        default = 7777;
      };

      proxyHost = mkOption {
        type = types.str;
        default = "127.0.0.1";
      };

      nodeId = mkOption {
        type = types.nullOr types.str;
        default = null;
      };

      listen = mkOption {
        type = types.nullOr types.str;
        default = null;
      };

      address = mkOption {
        type = types.nullOr types.str;
        default = null;
      };

      topologyFile = mkOption {
        type = types.path;
        default = mkProxyTopology envConfig.relays;
      };

      logger.configFile = mkOption {
        type = types.path;
        default = ../../cfg/logging.yaml;
        description = ''
          Logger configuration file
        '';
      };
    };
  };
}
