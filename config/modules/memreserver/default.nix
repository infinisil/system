# Workaround for an AMD/systemd suspension bug, see
# https://github.com/NixOS/nixpkgs/issues/223690#issuecomment-1492970990

{ config, lib, pkgs, ... }:

let
  memreserver = pkgs.stdenv.mkDerivation rec {
    pname = "memreserver";
    version = "unstable-2023-04-01";

    src = pkgs.fetchFromGitLab {
      domain = "git.dolansoft.org";
      owner = "lorenz";
      repo = pname;
      rev = "480253e565dab935df1d1c4e615ebc8a8dc81ba4";
      hash = "sha256-HjcrH98hH2zKdsHolYCFugL39sT1VjroVhRf8a8dpIA=";
    };

    nativeBuildInputs = with pkgs; [ meson ninja pkg-config ];
    buildInputs = with pkgs; [ libdrm ];

    meta = with lib; {
      homepage = "https://git.dolansoft.org/lorenz/memreserver";
      description = "Sleep hook which frees up RAM needed to evacuate GPU VRAM into";
      license = licenses.mit;
      maintainers = with maintainers; [ lorenz ];
    };
  };

in

{
  options = {
    hardware.memreserver = {
      enable = lib.mkOption {
        default = false;
        type = lib.types.bool;
        description = lib.mdDoc ''
          Enable memreserver sleep hook.
          Makes sure that GPUs with dedicated VRAM can suspend correctly.
        '';
      };
    };
  };

  config = lib.mkIf config.hardware.memreserver.enable {
    systemd.services.memreserver = {
      description = "Sleep hook which frees up RAM needed to evacuate GPU VRAM into";
      before = [ "sleep.target" ];
      wantedBy =  [ "sleep.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${memreserver}/bin/memreserver";
      };
    };
  };
}

