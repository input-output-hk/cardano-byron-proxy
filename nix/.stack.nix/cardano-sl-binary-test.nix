{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-binary-test"; version = "3.2.0"; };
      license = "Apache-2.0";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - binary serializarion (tests)";
      description = "This package contains test helpers for cardano-sl-binary.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.cborg)
          (hsPkgs.cereal)
          (hsPkgs.containers)
          (hsPkgs.formatting)
          (hsPkgs.half)
          (hsPkgs.hedgehog)
          (hsPkgs.hspec)
          (hsPkgs.mtl)
          (hsPkgs.pretty-show)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.safecopy)
          (hsPkgs.serokell-util)
          (hsPkgs.text)
          (hsPkgs.universum)
          ];
        build-tools = [
          (hsPkgs.buildPackages.cpphs or (pkgs.buildPackages.cpphs))
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "f778004a2d9940567732528ab9d0229d0d4984e2";
      sha256 = "1n1idcqc6w8r744fgqq1mxxbcc5a869d94mw98d6j2n4wzm2da7y";
      });
    postUnpack = "sourceRoot+=/binary/test; echo source root reset to \$sourceRoot";
    }