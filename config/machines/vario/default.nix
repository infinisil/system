{ lib, config, pkgs, ... }: {

  imports = [
    ../../hardware/pc
    ./hardware-configuration.nix
  ];

  hardware.opengl.driSupport32Bit = true;

  mine.sshMounts = lib.mapAttrs (name: value: {
    host = "infinisil@${value}:/home/infinisil";
    identity = "/home/infinisil/.ssh/id_rsa";
  }) config.networking.connections;

  mine.profiles.desktop.enable = true;

  shivacam.viewer.host = "ninur-w";

  services.xserver.xrandrHeads = [
    {
      output = "HDMI-0";
      monitorConfig = ''
        Option "Enable" "false"
      '';
    }
  ];

  mine.compton.highend = true;
  services.ipfs = {
    enable = true;
    autostart = true;
  };

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
    server = config.networking.connections.protos;
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

  services.nginx = {
    enable = true;
    virtualHosts.localhost = {
      basicAuth.infinisil = config.private.passwords."pc.infinisil.com";
      locations."/".root = "/webroot";
      locations."/betty/" = {
        root = "/betty";
        extraConfig = "autoindex on;";
      };
    };
  };

  networking = {
    hostName = "vario";
    hostId = "56236562";
    firewall.allowedTCPPorts = [ 80 ];
  };
}
