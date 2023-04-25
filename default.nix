# default.nix
let
  pkgs = import (builtins.fetchTarball {
    name = "nixpkgs-unstable";
    url = "https://github.com/nixos/nixpkgs/archive/4a5817c44a62c415ce3ec24ff832d12108805012.tar.gz";
    sha256 = "1s0dgaywwk0rlcj7n2a32w0izm91wi3arsxq2ymxr4fsb9asf1yi";
  })
  { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    # modifier = drv:
    #   pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
    #     [ cabal-install
    #       ghcid
    #     ]);
  }
