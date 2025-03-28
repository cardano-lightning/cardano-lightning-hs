# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#flakenix
{
  description = "Cardano Lightning Transaction Builder";

  inputs = {
    cardano-node.url = "github:IntersectMBO/cardano-node/9.1.0";
    # NOTE: Please update the version manually in the ./nix/packages/cardano-address.nix
    cardano-address-bin = {
      url =
        "https://github.com/IntersectMBO/cardano-addresses/releases/download/4.0.0/cardano-address-4.0.0-linux.tar.gz";
      flake = false;
    };
    # NOTE: Please update the version manually in the ./nix/packages/kupo.nix
    kupo-bin = {
      url =
        "https://github.com/CardanoSolutions/kupo/releases/download/v2.10/kupo-v2.10.0-x86_64-linux.zip";
      flake = false;
    };
    # NOTE: Please update the version manually in the ./nix/packages/ogmios.nix
    ogmios-bin = {
      url =
        "https://github.com/CardanoSolutions/ogmios/releases/download/v6.11.2/ogmios-v6.11.2-x86_64-linux.zip";
      flake = false;
    };
    iogx = {
      url = "github:input-output-hk/iogx";
      # Not sure how this is related to the cabal.project?
      # inputs.nixpkgs.follows = "nixpkgs";
      #inputs.hackage.follows = "hackage";
      # inputs.CHaP.follows = "CHaP";
      # inputs.haskell-nix.follows = "haskell-nix";
      # inputs.hackage.follows = "cardano-node/hackageNix";
      # inputs.CHaP.follows = "cardano-node/CHaP";
      # inputs.haskell-nix.follows = "cardano-node/haskellNix";
      # inputs.nixpkgs.follows = "cardano-node/nixpkgs";
    };

    # nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    # hackage = {
    #   url = "github:input-output-hk/hackage.nix";
    #   flake = false;
    # };
    # haskell-nix = {
    #   url = "github:input-output-hk/haskell.nix";
    #   inputs.hackage.follows = "hackage";
    # };
    # CHaP = {
    #   url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
    #   flake = false;
    # };

    # cardano-signer.url = "github:jhbertra/cardano-signer";
    # nixpkgsUnstable.url = "github:NixOS/nixpkgs/nixos-unstable";


    # cardano-addresses.url = "github:IntersectMBO/cardano-addresses?ref=03aace86f4a7b4bdb4af4ae8da98929d89514b9f";
    bech32.url = "github:IntersectMBO/bech32";
    # flake-parts.url = "github:hercules-ci/flake-parts";
    # nix-github-actions.url = "github:nix-community/nix-github-actions";
    process-compose.url = "github:F1bonacc1/process-compose";
  };

  # Docs for mkFlake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkflake
  outputs = inputs: inputs.iogx.lib.mkFlake {

    inherit inputs;

    repoRoot = ./.;

    outputs = import ./nix/outputs.nix;

    systems = [ "x86_64-linux" ]; # "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];

    nixpkgsArgs = {
      config = { };
      overlays = [ ];
    };

    # debug = false;
    # flake = { repoRoot, inputs }: {
    #   nixosModules.default = repoRoot.nix.nixosModule;
    #   nixpkgs = inputs.nixpkgs;
    # };
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}

