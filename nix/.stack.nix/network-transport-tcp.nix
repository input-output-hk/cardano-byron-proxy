{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { use-mock-network = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "network-transport-tcp"; version = "0.7.0"; };
      license = "BSD-3-Clause";
      copyright = "Well-Typed LLP, Tweag I/O Limited";
      maintainer = "Facundo Domínguez <facundo.dominguez@tweag.io>";
      author = "Duncan Coutts, Nicolas Wu, Edsko de Vries";
      homepage = "http://haskell-distributed.github.com";
      url = "";
      synopsis = "TCP instantiation of Network.Transport";
      description = "TCP instantiation of Network.Transport";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.network-transport)
          (hsPkgs.data-accessor)
          (hsPkgs.containers)
          (hsPkgs.bytestring)
          (hsPkgs.network)
          (hsPkgs.uuid)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/avieth/network-transport-tcp";
      rev = "2024d562d693518d983943331838b1857c2bf063";
      sha256 = "0mc0ki6xk9gwrxxmy67xish6h3pqvq9ixshgbmva8v879pmikjjj";
      });
    }