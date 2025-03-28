# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

# Each flake variant defined in your project.nix project will yield a separate
# shell. If no flake variants are defined, then cabalProject is the original 
# project.
{ repoRoot, inputs, pkgs, lib, system }: cabalProject:
let
  extraPackages = {
    cardano-address = (pkgs.callPackage
      (import ./packages/cardano-address.nix inputs.cardano-address-bin) { });
    kupo = (pkgs.callPackage (import ./packages/kupo.nix inputs.kupo-bin) { });
    ogmios =
      (pkgs.callPackage (import ./packages/ogmios.nix inputs.ogmios-bin) { });

  };
  mk-wallet = pkgs.stdenv.mkDerivation {
    pname = "mk-wallet";
    version = "0.1.0";
    src = ./scripts/mk-wallet.py;
    propagatedBuildInputs = [ pkgs.python3 ];
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/mk-wallet
      chmod +x $out/bin/mk-wallet
    '';
  };
in {
  name = "cardano-lightning";

  packages = [
    inputs.cardano-node.packages.cardano-node
    inputs.cardano-node.packages.cardano-cli
    inputs.process-compose.packages.process-compose
    inputs.bech32.packages.${system}."bech32:exe:bech32"
    pkgs.nixpkgs-fmt

    extraPackages.cardano-address
    extraPackages.kupo
    extraPackages.ogmios

    mk-wallet
  ];

  # tools = {
  #   haskell-language-server = hlsProject.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  # };

  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    fourmolu.enable = true;
    hlint.enable = false;
    shellcheck.enable = false;
    prettier.enable = true;
    nixpkgs-fmt.enable = false; # not sure how to debug this
  };

  scripts = {
    deploy-local-testnet = {
      description = "Start and run an ephemeral local testnet";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        ROOT="$(realpath --no-symlinks "''${1-testnet/example}")"

        cd $(git rev-parse --show-toplevel)/testnet
        [ -f "$ROOT/configuration.yaml" ] || scripts/babbage/mkfiles.sh "$ROOT"
        cd "$ROOT"
        ./run/all.sh
      '';
    };

    purge-local-testnet = {
      description = "Cleanup the local testnet directory";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        rm -rf "''${1-testnet/example}"
      '';
    };
  };

  env = {
    # This is the testnet ID for the local testnet
    CARDANO_NODE_NETWORK_ID = "42";
    PC_PORT_NUM="8200";
    PC_CONFIG_FILES=repoRoot.nix.process-compose-yaml;
  };

  # This is where the SPO 1 socket is located for the local testnet
  shellHook = ''
    export CARDANO_NODE_SOCKET_PATH="$(git rev-parse --show-toplevel)/.run/testnet/node-spo1/node.sock"
  '';
}
