{
  description = "Ambiente Haskell con Chart e Cairo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          
          myGhc = pkgs.haskellPackages.ghcWithPackages (p: with p; [
            Chart
            Chart-cairo
          ]);
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              myGhc
              pkgs.haskell-language-server
              pkgs.hlint
              pkgs.git
              pkgs.cairo
              pkgs.pango
              pkgs.pkg-config
              pkgs.tree
            ];
          };
        });
    };
}
