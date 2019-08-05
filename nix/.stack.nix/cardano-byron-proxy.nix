{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-byron-proxy"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "(c) 2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-byron-proxy";
      url = "";
      synopsis = "Adapter for the Byron net";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-chain)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-db)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cborg)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.contra-tracer)
          (hsPkgs.cryptonite)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.io-sim-classes)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.network-mux)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.ouroboros-network)
          (hsPkgs.resourcet)
          (hsPkgs.sqlite-simple)
          (hsPkgs.serialise)
          (hsPkgs.stm)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.typed-protocols)
          (hsPkgs.unliftio-core)
          (hsPkgs.unordered-containers)
          ];
        };
      exes = {
        "cardano-byron-proxy" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.cardano-byron-proxy)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-sl)
            (hsPkgs.cardano-sl-binary)
            (hsPkgs.cardano-sl-chain)
            (hsPkgs.cardano-sl-core)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.cardano-sl-infra)
            (hsPkgs.cardano-sl-util)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.contra-tracer)
            (hsPkgs.cryptonite)
            (hsPkgs.directory)
            (hsPkgs.exceptions)
            (hsPkgs.filepath)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.io-sim-classes)
            (hsPkgs.lens)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.ouroboros-network)
            (hsPkgs.random)
            (hsPkgs.reflection)
            (hsPkgs.resourcet)
            (hsPkgs.serialise)
            (hsPkgs.stm)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.typed-protocols)
            (hsPkgs.unordered-containers)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././.; }