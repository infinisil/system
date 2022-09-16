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

  hippo = pkgs.writeShellScriptBin "hippo" ''
    ${pkgs.xorg.xrandr}/bin/xrandr \
      --output HDMI-0 --mode 3840x2160 --rate 30
  '';
  projector = pkgs.writeShellScriptBin "projector" ''
    ${pkgs.xorg.xrandr}/bin/xrandr \
      --output HDMI-0 --mode 1920x1080 --rate 119.88
  '';
in {

  imports = [
    ./hardware-configuration.nix
    ../../personal/key-layout.nix
  ];

  mine.terminal.enable = true;
  mine.cdSupport.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  mine.localMusic.enable = true;

  nix.settings.experimental-features = [ "flakes" "nix-command" "ca-derivations" ];

  nix.settings.trusted-public-keys = [
    "tweag-haskell-fido2.cachix.org-1:bB+jy70CksEq3o6LKAJgZP1Fr5Moi7fsWzXBg3aZVxE="
  ];
  nix.settings.substituters = [
    "https://tweag-haskell-fido2.cachix.org"
  ];

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.kernelModules = [
    "v4l2loopback"
    # Needed for disks..
    "sg"
  ];

  mine.userConfig.programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "60";
    matchBlocks =
      lib.mapAttrs (name: value: { hostname = value.networking.public.ipv4; })
      (lib.filterAttrs (name: value: value.networking ? public.hasIpv4 && value.networking.public.hasIpv4) nodes);
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

  mine.japaneseInput = true;

  nix = {
    buildMachines = [{
      hostName = "192.168.178.51";
      maxJobs = 4;
      sshKey = "/home/infinisil/.ssh/id_ed25519";
      sshUser = "silvan";
      system = "x86_64-darwin";
    }];
  };

  environment.autoUpdate.enable = true;
  environment.autoUpdate.presets.yt-dlp = true;

  environment.autoUpdate.packages.discord.spec = {
    attrPath = [ "discord" ];
    config.allowUnfree = true;
  };

  # Remove fs-before.target
  systemd.services.zfs-import-main.before = lib.mkForce [
    "betty.mount"
    "home-infinisil-music.mount"
    "home-infinisil-torrent.mount"
  ];
  systemd.targets.zfs-import.after = lib.mkForce [];
  systemd.services.systemd-udev-settle.serviceConfig.ExecStart = [ "" "${pkgs.coreutils}/bin/true" ];

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
  services.blueman.enable = true;

  mine.profiles.default.enable = true;
  mine.profiles.desktop.enable = true;

  mine.xmonad = {
    enable = true;
    users = [ "infinisil" ];
  };

  services.xserver.displayManager.lightdm.extraSeatDefaults = ''
    display-setup-script=${projector}/bin/projector
  '';
  services.xserver.serverFlagsSection = ''
    Option "StandbyTime" "120"
    Option "SuspendTime" "120"
    Option "OffTime" "120"
    Option "BlankTime" "120"
  '';

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

  nixpkgs.overlays = [ (self: super: {
    inherit pot;
  }) ];

  environment.systemPackages = with pkgs; [
    guvcview
    pot
    projector
    hippo
    syncplay
    anki-bin
    element-desktop
    htop
    obs-studio
    xournal
    audacity
    chromium
    libreoffice
    moreutils
    jless
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

  systemd.services.nix-daemon.serviceConfig.LimitNOFILE = lib.mkForce 40960;

  networking = {
    hostName = "vario";
    hostId = "56236562";
    firewall.allowedTCPPorts = [ 80 ];
    useDHCP = false;
    interfaces.eno1.useDHCP = true;
  };
}
