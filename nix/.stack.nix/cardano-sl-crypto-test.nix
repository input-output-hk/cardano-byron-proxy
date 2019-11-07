let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-sl-crypto-test"; version = "3.1.0"; };
      license = "Apache-2.0";
      copyright = "2016 IOHK";
      maintainer = "hi@serokell.io";
      author = "Serokell";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - arbitrary instances for cardano-sl-crypto";
      description = "This package contains arbitrary instances for the cryptography primitives used in Cardano SL.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
          (hsPkgs."cardano-sl-binary" or (buildDepError "cardano-sl-binary"))
          (hsPkgs."cardano-sl-binary-test" or (buildDepError "cardano-sl-binary-test"))
          (hsPkgs."cardano-sl-crypto" or (buildDepError "cardano-sl-crypto"))
          (hsPkgs."cardano-sl-util" or (buildDepError "cardano-sl-util"))
          (hsPkgs."cardano-sl-util-test" or (buildDepError "cardano-sl-util-test"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."generic-arbitrary" or (buildDepError "generic-arbitrary"))
          (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
          (hsPkgs."universum" or (buildDepError "universum"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "35871b5f912f5dd09be6e0cfa89e372046d65bbc";
      sha256 = "1pk1ss3gzs4lvs9g0fi7xhnz18alcv97s7i1cnysk3v7zimwzlis";
      });
    postUnpack = "sourceRoot+=/crypto/test; echo source root reset to \$sourceRoot";
    }