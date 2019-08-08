{ commonLib
, byronProxy
, environments
, customConfig
, cardanoConfig
}:

let
  mkTopology = relay: pkgs.writeText "topology-file" ''
    wallet:
      relays: [[{ host: ${relay} }]]
  '';
  pkgs = commonLib.pkgs;
  mkProxyScript = environment: let
    envConfig = environments.${environment};
    config = {
      proxyHost = "127.0.0.1";
      proxyPort = 7777;
      topologyFile = mkTopology envConfig.relays;
      loggingConfig = ../cfg/logging.yaml;
    } // customConfig;
  in with config; pkgs.writeScript "byron-proxy-${environment}" ''
    exec ${byronProxy}/bin/cardano-byron-proxy +RTS -T -RTS --database-path db-byron-proxy-${environment} --index-path index-byron-proxy-${environment} --configuration-file ${configuration}/lib/configuration.yaml --configuration-key ${envConfig.confKey} --topology ${topologyFile} --logger-config ${loggingConfig} --local-addr [${proxyHost}]:${toString proxyPort}
  '';
  configuration = cardanoConfig;

in with builtins; listToAttrs (map
  (e: {name = e; value = mkProxyScript e;})
  (attrNames environments)
)
