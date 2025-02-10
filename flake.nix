# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#flakenix
{
  description = "Cardano Lightning Transaction Builder";

  # nixConfig = {
  #   bash-prompt = ''\[\033[1;32m\][cl-sig:\w]\$\[\033[0m\] '';
  # };

  inputs = {
    cardano-node.url = "github:IntersectMBO/cardano-node/10.2";
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
    # bech32.url = "github:IntersectMBO/bech32";
    # process-compose-flake.url = "github:Platonic-systems/process-compose-flake";
    # flake-parts.url = "github:hercules-ci/flake-parts";
    # nix-github-actions.url = "github:nix-community/nix-github-actions";
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

