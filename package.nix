{ pkgs }:

let
  hlib = pkgs.haskell.lib;
  hpkgs = pkgs.haskellPackages.override {
    overrides = self: super: {

      stackage-server = hlib.overrideCabal (self.callPackage nix/stackage-server.nix { }) (old: {
        preConfigure = ''
          ${pkgs.hpack}/bin/hpack .
        '';
        # During build, static files are generated into the source tree's
        # static/ dir. Plus, config/ is needed at runtime.
        postInstall = ''
          mkdir -p $out/run
          cp -a {static,config} $out/run
        '';
        src = pkgs.lib.cleanSource old.src;
      });

      # patched, see gen-package-nix.sh
      amazonka-core = self.callPackage nix/amazonka-core.nix { };

      # We have this old dependency for unexplored reasons.
      # Tests fail from attempted network access.
      pantry = pkgs.lib.pipe (self.callPackage nix/pantry.nix { }) [hlib.dontCheck hlib.doJailbreak];

      # Changing this has operational impacts.
      hoogle = self.callPackage nix/hoogle.nix { };

      # Outdated breakage? (TODO: upstream)
      barrier = hlib.markUnbroken super.barrier;

      # Tests fail from attempted network access (TODO: upstream)
      yesod-gitrev = hlib.markUnbroken (hlib.dontCheck super.yesod-gitrev);
    };
  };
in
hlib.justStaticExecutables hpkgs.stackage-server
