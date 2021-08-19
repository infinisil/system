{ nodes, lib, config, pkgs, ... }:
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

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.kernelModules = [ "v4l2loopback" ];

  mine.userConfig.programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "60";
    matchBlocks =
      lib.mapAttrs (name: value: { hostname = value.networking.public.ipv4; })
      (lib.filterAttrs (name: value: value.networking.public.hasIpv4) nodes);
  };

  services.zrepl = {
    enable = true;
    settings = {
      jobs = [
        {
          type = "snap";
          name = "data-snaps";
          filesystems."tank2/root/data<" = true;
          snapshotting = {
            type = "periodic";
            interval = "5m";
            prefix = "local_";
          };
          pruning.keep = [
            {
              type = "regex";
              regex = ".*";
            }
          ];
        }
        {
          type = "push";
          name = "data-push";
          filesystems."tank2/root/data<" = true;
          snapshotting = {
            type = "periodic";
            interval = "1h";
            prefix = "repl_";
          };
          connect = {
            type = "tcp";
            address = "10.99.2.1:8888";
          };
          send.raw = true;
          pruning = {
            keep_sender = [
              {
                type = "grid";
                regex = ".*";
                # Keep all snapshots in the last hour
                # In the hour before that, keep one every 15 minutes
                # The day before that, keep one every hour
                # In the 10 days before that, keep one every day
                grid = "12x5m(keep=all) | 4x15m | 24x1h | 10x1d";
              }
            ];
            keep_receiver = [
              {
                type = "grid";
                regex = ".*";
                grid = "1x1d(keep=all) | 7x1d | 5x7d | 12x31d";
              }
            ];
          };
        }
        {
          type = "pull";
          name = "current-torrents";
          connect = {
            type = "tcp";
            address = "10.99.2.1:8889";
          };
          root_fs = "main/current-torrents";
          interval = "1h";
          pruning = {
            keep_sender = [
              {
                type = "last_n";
                regex = ".*";
                count = 1;
              }
            ];
            keep_receiver = [
              {
                type = "last_n";
                regex = ".*";
                count = 1;
              }
            ];
          };
        }
        {
          type = "pull";
          name = "orakel-backup";
          connect = {
            type = "tcp";
            address = "10.99.2.1:8890";
          };
          root_fs = "main/backup/orakel";
          interval = "1h";
          pruning = {
            keep_sender = [
              {
                type = "grid";
                regex = ".*";
                grid = "1x1d(keep=all) | 7x1d";
              }
            ];
            keep_receiver = [
              {
                type = "grid";
                regex = ".*";
                grid = "1x1d(keep=all)";
              }
            ];
          };
        }
        {
          type = "pull";
          name = "protos-backup";
          connect = {
            type = "tcp";
            address = "10.99.3.1:8888";
          };
          root_fs = "main/backup/protos";
          interval = "1h";
          pruning = {
            keep_sender = [
              {
                type = "grid";
                regex = ".*";
                grid = "1x1d(keep=all)";
              }
            ];
            keep_receiver = [
              {
                type = "grid";
                regex = ".*";
                grid = "1x1d(keep=all) | 10x1d | 10x7d | 6x30d";
              }
            ];
          };
        }
      ];
    };
  };

  # Only really for env vars
  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = [
      pkgs.fcitx5-mozc
    ];
  };

  # Would normally set this to fcitx, but kitty only supports ibus, and fcitx
  # provides an ibus interface. Can't use ibus for e.g. QT_IM_MODULE though,
  # because that at least breaks mumble
  environment.variables.GLFW_IM_MODULE = "ibus";

  nix = {
    extraOptions = ''
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

  environment.autoUpdate.presets.youtube-dl = true;

  services.vault.enable = true;

  # Remove fs-before.target
  systemd.services.zfs-import-main.before = lib.mkForce [
    "betty.mount"
    "home-infinisil-music.mount"
    "home-infinisil-torrent.mount"
  ];
  systemd.targets.zfs-import.after = lib.mkForce [];
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
    kernelPackages = pkgs.linuxPackages_5_12;
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
    anki-bin
    element-desktop
    htop
    obs-studio
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
    interfaces.eno1.useDHCP = true;
  };
}
