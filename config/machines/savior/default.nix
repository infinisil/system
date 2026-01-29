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
      ../../personal/key-layout.nix
      ../../personal/user.nix
      (sources.nixos-hardware + "/lenovo/thinkpad/x1/11th-gen")
    ];

  #virtualisation.docker.enable = true;

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
  nix.settings.trusted-users = [ "tweagysil" ];

  nix.settings.trusted-public-keys = [
    "tweag-webauthn.cachix.org-1:FnOU/CHnxuFf7DGSRu82EJzQZ9UknNxgYl/BcHaPDEI="
  ];
  nix.settings.substituters = [
    "https://tweag-webauthn.cachix.org"
  ];

  mine.dunst.enable = true;
  mine.firefox.enable = true;

  #networking.firewall.trustedInterfaces = [ "docker0" ];

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
      "docker"
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

  users.users.nixcon = {
    uid = 1002;
    description = "Silvan Mosberger @ Tweag";
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
      "pipewire"
      "docker"
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

  users.users."silchan" = {
    uid = 1003;
    description = "モスバーガーシルヴァン";
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
      "pipewire"
    ];

    packages = with pkgs; [
      libreoffice
      tagainijisho
      anki-bin
    ];
  };


  home-manager.users.tweagysil = {
    programs.git = {
      userEmail = "silvan.mosberger@moduscreate.com";
      lfs.enable = true;
    };
  };

  services.pipewire.systemWide = true;

  mine.mainUsers = [ "tweagysil" "silchan" ];
  mine.console.users = [ "tweagysil" "root" "silchan" ];

  hardware.bluetooth.enable = true;

  services.gnome.gnome-keyring.enable = true;

  mine.userConfig = {
    services.gpg-agent = {
      enable = true;
      extraConfig = ''
        pinentry-program ${pkgs.pinentry-gnome3}/bin/pinentry
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

  #boot.initrd.availableKernelModules = [
  #  # https://wiki.archlinux.org/title/Kernel_mode_setting#Early_KMS_start
  #  "i915"
  #];

  #boot.kernelParams = [
  #  "drm.edid_firmware=DP-1:edid/rogswift.bin"
  #  "mem_sleep_default=deep"
  #];

  #hardware.firmware = let
  #  rogswiftEdid = pkgs.runCommand "rogswift.bin" {
  #    # EDID data for ASUS PG278Q ROG monitor
  #    # From https://bbs.archlinux.org/viewtopic.php?pid=2014292#p2014292
  #    hex = ''
  #      00ffffffffffff000469b127758201002b180104a53c2278064ce1a55850
  #      a0230b505400000001010101010101010101010101010101565e00a0a0a0
  #      29503020350056502100001a000000ff002341534e536230494c30655064
  #      000000fd001e961ed236010a202020202020000000fc00524f4720504732
  #      3738510a2020015002030a01654b040001015a8700a0a0a03b5030203500
  #      56502100001a5aa000a0a0a046503020350056502100001a6fc200a0a0a0
  #      55503020350056502100001a74d20016a0a009500410110056502100001e
  #      1c2500a0a0a011503020350056502100001a000000000000000000000000
  #      000000000000000000000000000000af'';
  #    passAsFile = [ "hex" ];
  #    nativeBuildInputs = [ pkgs.xxd ];
  #  } ''
  #    mkdir -p $out/lib/firmware/edid
  #    xxd -r -p <"$hexPath" >"$out/lib/firmware/edid/rogswift.bin"
  #  '';
  #in [
  #  rogswiftEdid
  #];

  services.fwupd.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) [
    "helvetica-neue-lt-std"
    "slack"
    "zoom"
    "discord"
  ];

  networking.iphoneUsbTethering.enable = false;

  mine.hardware.battery = true;

  mine.console.enable = true;

  i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.timeout = 1;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/efi";

  networking.hostName = "savior";
  networking.hostId = "1bfc1bb0";

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
    };
  };

  mine.xmonad = {
    enable = true;
    locker = true;
    users = [ "tweagysil" "silchan" ];
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
    # guvcview
    moreutils
    evince
    zulip
    discord
    #cinny-desktop
    signal-desktop
    inkscape
    libreoffice
    jless
    xournalpp
    chromium
    acpi
    diffoscope
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
      nerd-fonts.iosevka
      nerd-fonts.fira-mono
      nerd-fonts.fantasque-sans-mono
      nerd-fonts.symbols-only
      roboto
      roboto-mono
      fira-sans
      mplus-outline-fonts.osdnRelease
    ];
  };

  mine.japaneseInput = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.05"; # Did you read the comment?
  home-manager.sharedModules = [
    {
      home.stateVersion = "25.05"; # Did you read the comment?
    }
  ];
}


