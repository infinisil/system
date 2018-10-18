{ lib, config, nodes, pkgs, ... }: {

  imports = [
    ../../hardware/pc
    ./hardware-configuration.nix
  ];

  nixpkgs = {
    overlays = [
      (self: super: {
        linuxPackages_latest = super.linuxPackages_latest.extend (linuxSelf: linuxSuper: {
          nvidia_x11 = linuxSelf.nvidia_x11_beta;
        });
      })
    ];
  };
  hardware.opengl.driSupport32Bit = true;

  mine.sshMounts = lib.mapAttrs (name: value: {
    host = "infinisil@${value}:/home/infinisil";
    identity = "/home/infinisil/.ssh/id_rsa";
  }) {
    inf = "infinisil.com";
    emmaLocal = "192.168.1.19";
  };

  mine.profiles.desktop.enable = true;

  mine.compton.highend = true;
  services.ipfs = {
    enable = true;
    autostart = true;
  };

  mine.shinas = true;

  mine.eth.sem6.enable = true;

  mine.dev.rust.enable = true;

  mine.server-sync = {
    enable = true;
    dataDir = "server/data";
    uploadDir = "server/upload";
    server = "infinisil.com";
  };

  services.znapzend = {
    enable = true;
    pure = true;
    zetup.main.plan = "1hour=>5min,1day=>1hour,1week=>1day,1month=>1week";
  };
  mine.deluged.enable = true;
  mine.openvpn.client = {
    enable = true;
    server = nodes.new;
  };

  # hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  boot = {
    zfs.enableUnstable = true;
    loader = {
      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
      };
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages_latest;
  };

  mine.rpf.client = {
    server = nodes.new;
    webport = 8022;
    sshport = 2022;
    subdomain = "pc";
  };

  environment.systemPackages = with pkgs; [
    (writeScriptBin "projector" ''
      #!${stdenv.shell}
      xrandr --output HDMI-0 --mode 1920x1080 --output DP-4 --off
    '')
    (writeScriptBin "monitor" ''
      xrandr --output HDMI-0 --off --output DP-4 --mode 2560x1440
    '')
    discord
  ];

  mine.gaming.enable = true;

  services.nginx.virtualHosts.localhost = {
    basicAuth.infinisil = config.private.passwords."pc.infinisil.com";
    locations."/betty/" = {
      root = "/betty";
      extraConfig = "autoindex on;";
    };
  };

  networking = {
    hostName = "paul";
    hostId = "56236562";
    extraHosts = ''
      192.168.1.1 swisscom.mobile
    '';
  };
}
