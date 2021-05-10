{ lib, config, pkgs, ... }:
let
  pot = pkgs.writeShellScriptBin "pot" ''
    i=0
    currentName=$(pacmd dump | sed -n 's/set-default-sink \(.*\)/\1/p')
    current=
    declare -a sinks

    while IFS=$'\t' read -r id name type format state; do
      if [[ "$type" == module-null-sink.c ]]; then
        continue
      fi
      if [[ "$name" == "$currentName" ]]; then
        current="$i"
      fi
      sinks+=("$id")
      i=$(( i + 1 ))
    done < <(pactl list short sinks)

    count=''${#sinks[@]}
    toActivate=$(( (current + 1) % count ))

    pactl set-default-sink "''${sinks[$toActivate]}"
  '';

  yt = pkgs.writeShellScriptBin "yt" ''
    url=$1

    formats=$(youtube-dl -F "$url" 2>/dev/null | rg 'format code.*extension.*resolution.*note' -A 100000 | tail -n +2)

    audioid=$(echo "$formats" | rg "audio only" | cut -d' ' -f1 | sort | tail -1)

    videoid=$(echo "$formats" | rg "video only" | fzf --tac | cut -d' ' -f1)

    echo $audioid
    echo $videoid

    mpv "$url" --ytdl-format="$videoid+$audioid"
  '';

  projector = pkgs.writeShellScriptBin "projector" ''
    xrandr --output HDMI-0 --mode 1920x1080 --output DP-2 --off
    pactl set-default-sink alsa_output.usb-Kingston_HyperX_7.1_Audio_00000000-00.analog-stereo
  '';
  monitor = pkgs.writeShellScriptBin "monitor" ''
    xrandr --output HDMI-0 --off --output DP-2 --mode 2560x1440
    pactl set-default-sink alsa_output.pci-0000_00_1b.0.analog-stereo
  '';
in {

  imports = [
    ./hardware-configuration.nix
  ];

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
      allowed-uris = https://github.com/
    '';

    buildMachines = [{
      hostName = "192.168.178.51";
      maxJobs = 4;
      sshKey = "/home/infinisil/.ssh/id_ed25519";
      sshUser = "silvan";
      system = "x86_64-darwin";
    }];
  };

  environment.autoUpdate.packages.youtube-dl = {
    attrPath = [ "youtube-dl" ];
    url = "channel:nixpkgs-unstable";
    period = "hourly";
  };

  services.vault.enable = true;

  systemd.services.zfs-import-main.before = lib.mkForce [ "betty.mount" ];
  systemd.targets.zfs-import.after = lib.mkForce [];
  fileSystems."/betty".options = [ "nofail" ];
  systemd.services.systemd-udev-settle.serviceConfig.ExecStart = [ "" "${pkgs.coreutils}/bin/true" ];

  services.lorri.enable = true;

  mine.enableUser = true;

  mine.saveSpace = true;

  mine.hardware = {
    swap = true;
    cpuCount = 8;
    audio = true;
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
  };

  users.users.infinisil.extraGroups = [ "docker" "transmission" "plugdev" ];
  users.groups.transmission.gid = 70;
  users.groups.plugdev = {};

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.bluetooth.enable = true;

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

  #services.ipfs = {
  #  enable = true;
  #  autostart = true;
  #};

  #mine.dev.rust.enable = true;

  services.znapzend = {
    enable = true;
    pure = true;
    autoCreation = true;
    zetup."tank2/root/data" = {
      plan = "1d=>1h,1w=>1d";
      recursive = true;
      destinations.backup = {
        host = config.networking.connections.orakel;
        dataset = "tank/backup/vario";
        plan = "1w=>1d,1m=>1w,6m=>1m";
      };
    };
  };
  #mine.deluged.enable = true;
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

  # hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  boot = {
    loader = {
      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        gfxmodeEfi = "2560x1440";
      };
      efi.canTouchEfiVariables = true;
    };
    # https://discourse.nixos.org/t/browsers-unbearably-slow-after-update/9414/31
    kernelParams = [ "intel_pstate=active" ];
  };

  mine.binalias.rate = "mpc sendmessage rating";

  programs.zsh.shellAliases.yt = "noglob yt";

  environment.systemPackages = with pkgs; [
    guvcview
    slack-dark
    zoom-us
    pot
    yt
    projector
    monitor
    syncplay
  ];

  mine.gaming.enable = true;

  services.nginx = {
    enable = true;
    virtualHosts.localhost = {
      #basicAuth.infinisil = config.private.passwords."pc.infinisil.com";
      locations."/".root = "/webroot";
      locations."/betty/" = {
        root = "/betty";
        extraConfig = "autoindex on;";
      };
    };
  };

  services.udev.extraRules = ''
    # Rule for all ZSA keyboards
    SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
    # Rule for the Ergodox EZ
    SUBSYSTEM=="usb", ATTR{idVendor}=="feed", ATTR{idProduct}=="1307", GROUP="plugdev"

    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"
  '';

  networking = {
    hostName = "vario";
    hostId = "56236562";
    firewall.allowedTCPPorts = [ 80 ];
    useDHCP = false;
    defaultGateway.address = "192.168.178.1";
    interfaces.eno1.ipv4.addresses = [ {
      address = "192.168.178.53";
      prefixLength = 24;
    }];
  };
}
