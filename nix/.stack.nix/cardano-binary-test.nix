{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-binary-test"; version = "1.3.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-binary exposed to other packages";
      description = "Test helpers from cardano-binary exposed to other packages";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-prelude-test)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.formatting)
          (hsPkgs.hedgehog)
          (hsPkgs.hspec)
          (hsPkgs.pretty-show)
          (hsPkgs.QuickCheck)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.text)
          (hsPkgs.vector)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "ace6b96d8ff13198d938a69c99cb949017c27d18";
      sha256 = "10z8s42n66h2af1l5glf4xn62j5xyxcr3iafjmwgld7cijxq47yp";
      });
    postUnpack = "sourceRoot+=/binary/test; echo source root reset to \$sourceRoot";
    }