let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  buildInputs = [
    pkgs.git
    pkgs.nixfmt
    pkgs.nodejs-18_x
    pkgs.yarn
    pkgs.esbuild
    pkgs.purescript
    pkgs.spago
    pkgs.nodePackages.purescript-language-server
    pkgs.nodePackages.purs-tidy
  ];
}
