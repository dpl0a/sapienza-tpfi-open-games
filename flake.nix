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
          
          # Definiamo GHC con le librerie richieste
          myGhc = pkgs.haskellPackages.ghcWithPackages (p: with p; [
            Chart
            Chart-cairo
          ]);
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              myGhc                    # GHC con Chart e Chart-cairo inclusi
              pkgs.haskell-language-server
              pkgs.hlint
              pkgs.git
              # Cairo ha bisogno di dipendenze di sistema per compilare/girare
              pkgs.cairo
              pkgs.pango
              pkgs.pkg-config
            ];

            shellHook = ''
              echo "Haskell environment con Chart & Cairo caricato!"
            '';
          };
        });
    };
}
