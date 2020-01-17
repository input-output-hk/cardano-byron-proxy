{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-crypto-test"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - arbitrary instances for cardano-sl-crypto";
      description = "This package contains arbitrary instances for the cryptography primitives used in Cardano SL.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-binary-test)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.cryptonite)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.memory)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.universum)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "f778004a2d9940567732528ab9d0229d0d4984e2";
      sha256 = "1n1idcqc6w8r744fgqq1mxxbcc5a869d94mw98d6j2n4wzm2da7y";
      });
    postUnpack = "sourceRoot+=/crypto/test; echo source root reset to \$sourceRoot";
    }