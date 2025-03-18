{
  description = "stackage-server";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          package = pkgs.callPackage ./package.nix {};
        in
        {
          packages.default = package.app;
          devShells.default = package.shell;

          checks = {
            # I used to put these into $out/lib, but justStaticExecutables
            # removes that directory. Now I feel like I'm just getting lucky. So
            # let's double check the files are there.
            file-check = pkgs.runCommand "check-runtime-files" {} ''
              if [ -e ${self.packages.${system}.default}/run/config/settings.yml ]; then
                touch $out
              else
                2>&1 echo "Runtime files are missing"
                exit 1
              fi
            '';
          };
        }
      );
}
