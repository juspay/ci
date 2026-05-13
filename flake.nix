{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = { self, nixpkgs, haskell-flake, ... }:
    let
      eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;

      perSystem = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          project = (haskell-flake.lib { inherit pkgs; }).evalHaskellProject {
            projectRoot = self;
            modules = [{
              devShell.mkShellArgs.nativeBuildInputs = [ pkgs.just pkgs.process-compose pkgs.gh pkgs.git ];
              settings.ci.extraBuildTools = [ pkgs.just pkgs.process-compose pkgs.gh pkgs.git ];
            }];
          };
        in
        {
          packages.default = project.packages.ci.package;
          devShells.default = project.devShell;
        };

      systemOutputs = eachSystem perSystem;
    in
    {
      packages = nixpkgs.lib.mapAttrs (_: s: s.packages) systemOutputs;
      devShells = nixpkgs.lib.mapAttrs (_: s: s.devShells) systemOutputs;
    };
}
