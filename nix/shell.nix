# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

# Each flake variant defined in your project.nix project will yield a separate
  # shell. If no flake variants are defined, then cabalProject is the original 
  # project.
{ repoRoot, inputs, pkgs, lib, system }: cabalProject: {
  name = "cardano-lightning";

  packages = [
    inputs.cardano-node.packages.cardano-node
    pkgs.process-compose
    pkgs.nixpkgs-fmt

    # inputs.nixpkgsUnstable.legacyPackages.bun
    # pkgs.retry
    # pkgs.magic-wormhole
    # pkgs.scriv
    # pkgs.jq
    # pkgs.fq
    # pkgs.nodejs
    # pkgs.nix-index
    # inputs.cardano-addresses.packages."cardano-addresses-cli:exe:cardano-address"
    # inputs.bech32.packages."bech32:exe:bech32"
    # inputs.cardano-node.packages.cardano-cli
    # inputs.self.packages.create-testing-datums
    # inputs.self.packages.new-environment
    # inputs.cardano-signer.packages.cardano-signer
    # hlsProject.hsPkgs.hie-bios.components.exes.hie-bios
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
  };

  # This is where the SPO 1 socket is located for the local testnet
  shellHook = ''
    export CARDANO_NODE_SOCKET_PATH="$(git rev-parse --show-toplevel)/.run/testnet/node-spo1/node.sock"
  '';
  # When we reintroduce process-compose we will need this:
  # export PC_CONFIG_FILES=${repoRoot.nix.process-compose}
}
