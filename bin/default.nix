with import ../nixpkgs {};

let
  nixops = pkgs.nixops.overrideDerivation (
    old: {
      patchPhase = ''
        substituteInPlace nix/eval-machine-info.nix \
            --replace 'system.nixosVersion' 'system.nixos.version'
      '';
    }
  );
in

runCommand "rb" {
  buildInputs = [ makeWrapper ];
} ''
  mkdir -p $out/bin
  substitute ${./rb} $out/bin/rb \
    --subst-var-by nixops ${nixops}/bin/nixops

  chmod +x $out/bin/rb
''
