{
  commonLib,
  customConfig,
}:

with commonLib.pkgs.lib;

let
  pkgs = commonLib.pkgs;
  pkgsModule = {
    config._module.args.pkgs = mkDefault pkgs;
  };
  mkProxyScript = envConfig: let
    defaultConfig = {
      stateDir = "state-proxy-${envConfig.name}";
      environment = envConfig.name;
      loggingConfig = ../cfg/logging.yaml;
      pbftThreshold = null;
      # defaults to proxy if env has no relays
      proxyHost = "127.0.0.1";
      proxyPort = 7777;
      nodeId = null;
    };
    config = defaultConfig // envConfig // customConfig;
    serviceConfig = {
      inherit (config)
        stateDir
        environment
        pbftThreshold
        proxyHost
        proxyPort;
      logger.configFile = config.loggingConfig;
    };
    proxyConf = { config.services.byron-proxy = serviceConfig; };
    proxyScript = (modules.evalModules {
      modules = [
        ./nixos/byron-proxy-options.nix
        proxyConf
        pkgsModule
      ];
    }).config.services.byron-proxy.script;
  in pkgs.writeScript "byron-proxy-${envConfig.name}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${proxyScript} $@
  '';
  scripts = commonLib.forEnvironments (environment:
  {
    proxy = mkProxyScript environment;
  });
in scripts

