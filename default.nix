{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    brassica = ./.;
    brassica-web = ./gui/brassica-web;
  };

  shells = {
    ghc = ["brassica" "brassica-web"];
    ghcjs = ["brassica" "brassica-web"];
  };

  shellToolOverrides = self: super: {
    inherit (pkgs.haskell.packages.ghc865) haskell-language-server;
  };

  useWarp = true;
})
