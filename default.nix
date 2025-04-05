{ reflex-platform ? import ../reflex-platform { } }:
let
  pkgs = reflex-platform.nixpkgs;
in
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    hello-world = ./.;
  };

  shells = {
    ghc = [ "hello-world" ];
    ghcjs = [ "hello-world" ];
  };

  shellToolOverrides = self: super: {
    # Development tools
    haskell-language-server = pkgs.haskell.packages.ghc8107.haskell-language-server;
    implicit-hie = pkgs.haskell.packages.ghc8107.implicit-hie;
  };
})
