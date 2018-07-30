{ lib, config, nodes, pkgs, ... }: {

  imports = [
    ../../hardware/pc
    ./hardware-configuration.nix
  ];

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
    server = nodes.yuri;
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"
    KERNEL=="uinput", MODE="0660", GROUP="users", OPTIONS+="static_node=uinput"
  '';
  hardware.opengl.driSupport32Bit = true;
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
  };

  mine.rpf.client = {
    server = nodes.yuri;
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
