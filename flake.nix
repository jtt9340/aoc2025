{
  description = "Advent of Code 2025";
  # Have to use this jank version of nixpkgs which contains a version of rio,
  # a dependency of Stack, that does not have this bug: https://github.com/commercialhaskell/rio/issues/264
  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=pull/466258/head";

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages = pkgs.haskell.packages.ghc9103;
      # Adapted from https://docs.haskellstack.org/en/v3.7.1/topics/nix_integration/#supporting-both-nix-and-non-nix-developers
      stack-wrapped = pkgs.symlinkJoin {
        name = "stack";
        paths = [ haskellPackages.stack ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = with haskellPackages; [ ghc stack-wrapped ];
      };
    };
}
