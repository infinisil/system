# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, sources, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../new-modules
      ../../modules
      ../../personal/user.nix
      ../../personal/key-layout.nix
      (sources.nixos-hardware + "/framework/13-inch/11th-gen-intel")
    ];

  obswatch.enable = true;

  services.netdata.enable = true;

  hardware.opengl.extraPackages = [
    pkgs.vpl-gpu-rt
  ];

  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.addNetworkInterface = false;

  services.transmission.enable = true;

  mine.japaneseInput = true;

  boot.zfs.allowHibernation = true;
  boot.zfs.forceImportRoot = false;

  services.logind.lidSwitch = "suspend-then-hibernate";

  # The behavior of suspend-then-hibernate changed in systemd 252, see https://github.com/systemd/systemd/issues/25269
  systemd.sleep.extraConfig = ''
    HibernateDelaySec=${toString (60 * 60)}
  '';

  nix.package = pkgs.nixVersions.latest;
  nix.buildMachines = [
    # tweag remote builders
    {
      hostName = "build01.tweag.io";
      maxJobs = 24;
      sshUser = "nix";
      sshKey = "/root/.ssh/id-tweag-builder";
      system = "x86_64-linux";
      supportedFeatures = [ "big-parallel" "kvm" "nixos-test" ];
    }
    {
      hostName = "build02.tweag.io";
      maxJobs = 24;
      sshUser = "nix";
      sshKey = "/root/.ssh/id-tweag-builder";
      systems = ["aarch64-darwin" "x86_64-darwin"];
      supportedFeatures = [ "big-parallel" ];
    }
  ];

  nix.daemonCPUSchedPolicy = "idle";

  nix.settings.builders-use-substitutes = true;

  nix.settings.experimental-features = [ "flakes" "nix-command" ];
  nix.settings.trusted-users = [ "infinisil" "tweagysil" "ncasil" ];

  nix.settings.trusted-public-keys = [
    "tweag-webauthn.cachix.org-1:FnOU/CHnxuFf7DGSRu82EJzQZ9UknNxgYl/BcHaPDEI="
  ];
  nix.settings.substituters = [
    "https://tweag-webauthn.cachix.org"
  ];

  mine.dunst.enable = true;
  mine.firefox.enable = true;

  networking.extraHosts = ''
    206.81.23.189 protos
    10.99.3.2 vario-via-protos
    192.168.0.12 vario-local
  '';

  users.mutableUsers = false;

  users.users.tweagysil = {
    uid = 1001;
    description = "Silvan Mosberger @ Tweag";
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
      "pipewire"
    ];
    packages = with pkgs; [
      slack
      zoom-us
      tmate
      nixfmt
      obs-studio
      shellcheck
    ];
  };

  users.users.ncasil = {
    uid = 1002;
    description = "Silvan Mosberger @ NCA";
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
      "pipewire"
    ];
    packages = with pkgs; [
      zoom-us
      tmate
      nixfmt
      shellcheck
    ];
  };

  home-manager.users.tweagysil = {
    programs.git = {
      userEmail = "silvan.mosberger@tweag.io";
      lfs.enable = true;
    };
  };

  home-manager.users.ncasil = {
    programs.git = {
      userEmail = "nca@infinisil.com";
      #lfs.enable = true;
    };
  };

  users.users.infinisil = {
    packages = with pkgs; [
      mumble
    ];
    extraGroups = [ "transmission" ];
  };

  services.pipewire.systemWide = true;

  mine.mainUsers = [ "tweagysil" "ncasil" ];

  hardware.bluetooth.enable = true;

  mine.userConfig = {
    services.gpg-agent = {
      enable = true;
      extraConfig = ''
        pinentry-program ${pkgs.pinentry.qt}/bin/pinentry
      '';
    };
  };


  mine.xUserConfig = {
    services.random-background = {
      enable = true;
      imageDirectory = "%h/background";
      interval = "120";
    };
  };

  services.clight = {
    enable = true;
    settings = {
      inhibit.disabled = true;
      dpms.disabled = true;
      dimmer.disabled = true;
      screen.disabled = true;
      backlight.no_auto_calibration = true;
    };
  };

  location.latitude = 47.4;
  location.longitude = 9.2;

  mine.sound.enable = true;
  mine.vim.enable = true;

  boot.initrd.availableKernelModules = [
    # https://wiki.archlinux.org/title/Kernel_mode_setting#Early_KMS_start
    "i915"
  ];

  boot.kernelParams = [
    "drm.edid_firmware=DP-1:edid/rogswift.bin"
    "mem_sleep_default=deep"
  ];

  hardware.firmware = let
    rogswiftEdid = pkgs.runCommand "rogswift.bin" {
      # EDID data for ASUS PG278Q ROG monitor
      # From https://bbs.archlinux.org/viewtopic.php?pid=2014292#p2014292
      hex = ''
        00ffffffffffff000469b127758201002b180104a53c2278064ce1a55850
        a0230b505400000001010101010101010101010101010101565e00a0a0a0
        29503020350056502100001a000000ff002341534e536230494c30655064
        000000fd001e961ed236010a202020202020000000fc00524f4720504732
        3738510a2020015002030a01654b040001015a8700a0a0a03b5030203500
        56502100001a5aa000a0a0a046503020350056502100001a6fc200a0a0a0
        55503020350056502100001a74d20016a0a009500410110056502100001e
        1c2500a0a0a011503020350056502100001a000000000000000000000000
        000000000000000000000000000000af'';
      passAsFile = [ "hex" ];
      nativeBuildInputs = [ pkgs.xxd ];
    } ''
      mkdir -p $out/lib/firmware/edid
      xxd -r -p <"$hexPath" >"$out/lib/firmware/edid/rogswift.bin"
    '';
  in [
    rogswiftEdid
  ];

  services.fwupd.enable = true;
  services.fwupd.extraRemotes = [ "lvfs-testing" ];

  # https://github.com/NixOS/nixpkgs/pull/266598
  users.users.fwupd-refresh.isSystemUser = true;
  users.users.fwupd-refresh.group = "fwupd-refresh";
  users.groups.fwupd-refresh = {};
  systemd.services.fwupd-refresh.serviceConfig = {
    DynamicUser = lib.mkForce false;
    StandardError = "inherit";
  };

  nixpkgs.config.allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) [
    "helvetica-neue-lt-std"
    "slack"
    "zoom"
    "discord"
  ];

  networking.iphoneUsbTethering.enable = false;

  #services.udev.extraHwdb = ''
  #  evdev:atkbd:dmi:*
  #   KEYBOARD_KEY_db=leftalt
  #   KEYBOARD_KEY_38=leftmeta
  #'';

  mine.hardware.battery = true;

  mine.enableUser = true;

  mine.console.enable = true;

  i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

  boot.loader = {
    timeout = 1;
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
      extraEntries = ''
        menuentry "LibreElec" {
          search --set -f /KERNEL
          # Turns off the builtin display, and forces a specific resolution for the projector (otherwise it only detects very low resolutions for some reason)
          linux /KERNEL boot=UUID=E0D2-41B5 disk=UUID=a61696ac-07d1-4c07-a763-ab4618bb0996 quiet video=DP-4:3840x2160@60 video=eDP-1:d drm.edid_firmware=edid/edid.bin
          initrd /edid.cpio
        }
      '';
    };
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/efi";
    };
  };

  networking.hostName = "zion";
  networking.hostId = "e585b53a";

  services.fprintd.enable = true;

  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  time.timeZone = "Europe/Zurich";

  mine.terminal.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver = {
    exportConfiguration = true;
    displayManager = {
      lightdm = {
        enable = true;
      };

      #sessionCommands = ''
      #  # Set GTK_DATA_PREFIX so that GTK+ can find the themes
      #  export GTK_DATA_PREFIX=${config.system.path}

      #  # find theme engines
      #  export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
      #'';
    };
  };

  mine.xmonad = {
    enable = true;
    locker = true;
    users = [ "infinisil" "tweagysil" "ncasil" ];
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    flameshot
    nix-output-monitor
    vim
    htop
    git
    element-desktop
    feh
    arandr
    mpv
    xdotool
    xorg.xwininfo
    simplescreenrecorder
    ffmpeg
    thunderbird
    guvcview
    moreutils
    evince
    zulip
    discord
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  services.libinput.enable = true;
  services.libinput.touchpad.naturalScrolling = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      (nerdfonts.override {
        fonts = [
          "Iosevka"
          "FiraMono"
          "FantasqueSansMono"
          "NerdFontsSymbolsOnly"
        ];
      })
      roboto
      roboto-mono
    ];
  };



  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}

