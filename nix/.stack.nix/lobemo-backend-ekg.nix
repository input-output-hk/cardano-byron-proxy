{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "lobemo-backend-ekg"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand";
      homepage = "https://github.com/input-output-hk/iohk-monitoring-framework";
      url = "";
      synopsis = "provides a backend implementation to EKG";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.ekg)
          (hsPkgs.ekg-core)
          (hsPkgs.safe-exceptions)
          (hsPkgs.snap-core)
          (hsPkgs.snap-server)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.unordered-containers)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "dd30455144e11efb435619383ba84ce02aee720d";
      sha256 = "1g08bg99fvss99kg27l7pmxm7lh60573xln8l8x2rzvwfvfgk2i5";
      });
    postUnpack = "sourceRoot+=/plugins/backend-ekg; echo source root reset to \$sourceRoot";
    }