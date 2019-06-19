{ lib, config, pkgs, ... }: {

  imports = [
    ./hardware-configuration.nix
  ];

  mine.enableUser = true;

  mine.saveSpace = true;

  mine.hueadm.controls = true;

  mine.hardware = {
    swap = true;
    cpuCount = 8;
    audio = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  mine.compton.nvidia = true;

  hardware.opengl.driSupport32Bit = true;

  mine.sshMounts = lib.mapAttrs (name: value: {
    host = "infinisil@${value}:/home/infinisil";
    identity = "/home/infinisil/.ssh/id_rsa";
  }) config.networking.connections;

  mine.profiles.desktop.enable = true;

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
  services.deluge = {
    declarative = true;
    config = {
      move_completed_path = "/betty/Torrent";
      queue_new_to_top = true;
      max_active_limit = -1;
      max_active_downloading = -1;
      max_active_seeding = -1;
      allow_remote = true;
      max_half_open_connections = -1;
      download_location = "/var/lib/deluge/part";
      max_upload_speed = 500.0;
      max_connections_per_second = -1;
      dont_count_slow_torrents = true;
      torrentfiles_location = "/var/lib/deluge/torrent";
      enabled_plugins = [ "YaRSS2" ];
      max_connections_global = 500;
      listen_ports = [ 6881 6891 ];
      max_upload_speed_per_torrent = 250.0;
      copy_torrent_file = true;
      move_completed = true;
    };
  };

  mine.openvpn.client = {
    enable = true;
    server = config.networking.connections.protos;
  };

  # hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  boot = {
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
