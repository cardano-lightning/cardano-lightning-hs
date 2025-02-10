{ repoRoot, inputs, pkgs, system, lib }:

let

  cabalProject' = pkgs.haskell-nix.cabalProject' ({ pkgs, config, ... }:
    let
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      # WARNING You must use the `pkgs` coming from cabalProject' for `isCross` to work.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      src = ../.;

      shell.withHoogle = false;

      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.iogx.inputs.CHaP;
      };

      name = "cardano-lightning";

      compiler-nix-name = lib.mkDefault "ghc96";
      # flake.variants.profiled = {
      # };
      # flake.variants = {
      #   ghc96 = { }; # Alias for the default project
      #   # ghc96-profiled.modules = [{
      #   #   enableProfiling = true;
      #   #   enableLibraryProfiling = true;
      #   # }];
      #   ghc810.compiler-nix-name = "ghc810";
      #   # ghc98.compiler-nix-name = "ghc98";
      #   # ghc910.compiler-nix-name = "ghc910";
      # };

      # flake.variants.profiled = {
      #   modules = [{
      #     enableProfiling = true;
      #     enableLibraryProfiling = true;
      #   }];
      # };

      modules =
        [
          {
            dontStrip = false;
            packages = { };
          }
        ];
    });


  cabalProject = cabalProject'.appendOverlays [ ];


  # Docs for mkHaskellProject: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellproject
  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;

    shellArgs = repoRoot.nix.shell;

    includeProfiledHydraJobs = true;

    includeMingwW64HydraJobs = false;

    readTheDocs = {
      enable = false;
      siteFolder = "doc";
      sphinxToolchain = null;
    };

    combinedHaddock = {
      enable = true;
      prologue = "";
      packages = [ ];
    };
  };

in

project
