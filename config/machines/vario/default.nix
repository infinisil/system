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

  services.znapzend = {
    enable = true;
    pure = true;
    autoCreation = true;
    zetup."main/root/data" = {
      plan = "1h=>5min,1d=>1h,1w=>1d";
      recursive = true;
      destinations.backup = {
        host = config.networking.connections.orakel;
        dataset = "tank/backup/vario";
        plan = "1w=>1d,1m=>1w,6m=>1m";
      };
    };
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

  services.openvpn.servers.protos = {
    autoStart = false;
    mine.type = "client";
    mine.client.serverIp = config.networking.connections.protos;
  };

  services.openvpn.servers.orakel = {
    mine.type = "client";
    mine.client.serverIp = config.networking.connections.orakel;
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

  mine.binalias.projector = "xrandr --output HDMI-0 --mode 1920x1080 --output DP-2 --off";
  mine.binalias.monitor = "xrandr --output HDMI-0 --off --output DP-2 --mode 2560x1440";

  environment.systemPackages = with pkgs; [
    guvcview
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
