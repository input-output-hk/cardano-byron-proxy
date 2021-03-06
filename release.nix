############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-byron-proxy ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-byron-proxy.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Import IOHK common nix lib
, commonLib ? import ./lib.nix {}

}:

with (import commonLib.iohkNix.release-lib) {
  inherit (commonLib) pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-byron-proxy;
  gitrev = cardano-byron-proxy.rev;
};

with pkgs.lib;

let
  nixosTests = (import ./. {}).nixosTests;
  getArchDefault = system: let
    table = {
      x86_64-linux = import ./. { system = "x86_64-linux"; };
      x86_64-darwin = import ./. { system = "x86_64-darwin"; };
      x86_64-windows = import ./. { system = "x86_64-linux"; crossSystem = "x86_64-windows"; };
    };
  in table.${system};
  default = getArchDefault builtins.currentSystem;
  makeScripts = cluster: let
    getScript = name: {
      x86_64-linux = (getArchDefault "x86_64-linux").scripts.${cluster}.${name};
      x86_64-darwin = (getArchDefault "x86_64-darwin").scripts.${cluster}.${name};
    };
  in {
    proxy = getScript "proxy";
  };
  # TODO: add docker images
  #wrapDockerImage = cluster: let
  #  images = (getArchDefault "x86_64-linux").dockerImages;
  #  wrapImage = image: commonLib.pkgs.runCommand "${image.name}-hydra" {} ''
  #    mkdir -pv $out/nix-support/
  #    cat <<EOF > $out/nix-support/hydra-build-products
  #    file dockerimage ${image}
  #    EOF
  #  '';
  makeRelease = cluster: {
    name = cluster;
    value = {
      scripts = makeScripts cluster;
      #dockerImage = wrapDockerImage cluster;
    };
  };
  extraBuilds = let
    # only build nixos tests for linux
    default = getArchDefault "x86_64-linux";
  in {
    inherit nixosTests;
  } // (builtins.listToAttrs (map makeRelease [
    "mainnet"
    "staging"
    "shelley_staging_short"
    "shelley_staging"
    "testnet"
  ]));

  testsSupportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all test derivations in a list.
  collectTests' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTests = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectTests' package)
    ) ds);

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross project);
    # TODO: fix broken evals
    #musl64 = mapTestOnCross musl64 (packagePlatformsCross project);
  } // extraBuilds // (mkRequiredJob (
      collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++ [
      jobs.native.cardano-byron-proxy.x86_64-darwin
      jobs.native.cardano-byron-proxy.x86_64-linux
      (map (cluster: jobs.${cluster}.scripts.proxy.x86_64-linux) [ "mainnet" "testnet" "staging" ])
      # windows cross compilation targets
      #jobs.x86_64-pc-mingw32.cardano-byron-proxy.x86_64-linux
    ]));

in jobs
