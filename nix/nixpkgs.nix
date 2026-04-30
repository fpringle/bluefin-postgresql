let
  # set the GHC version for the entire project.
  ghc-version = "ghc910";

  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { overlays = [ overlay ]; };
  packages = import ./packages.nix;

  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];

  overlay = final: prev:
    let
      inherit (prev.haskell.lib)
        doJailbreak
        doCheck
        ;

      haskell-overrides = hfinal: hprev:
        let
          bluefin = sources."bluefin-0.5.1.0";
          # bluefin = sources."bluefin-0.2.7.0";

          packageOverrides = {
            bluefin-internal = hfinal.callCabal2nixWithOptions "bluefin-internal" bluefin "--subpath bluefin-internal" { };
            bluefin = hfinal.callCabal2nixWithOptions "bluefin" bluefin "--subpath bluefin" { };
            postgresql-libpq = doJailbreak hprev.postgresql-libpq;
            hlint = doJailbreak hprev.hlint;
            postgresql-operation-counting = hfinal.callCabal2nix "postgresql-operation-counting" "${sources.postgresql-operation-counting}" { };
          };

          makePackage = name: path:
            let pkg = hfinal.callCabal2nix name (gitignore path) { };
            in doCheck pkg;
        in
        builtins.mapAttrs makePackage packages // packageOverrides;
    in
    {
      # This will become the main package set for the project. In a `nix-shell`,
      # this is what we'll have access to.
      haskellPackages = prev.haskell.packages.${ghc-version}.override {
        overrides = haskell-overrides;
      };
    };
in
nixpkgs
