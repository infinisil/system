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
      (sources.nixos-hardware + "/lenovo/thinkpad/x1/13th-gen")
    ];

  services.transmission.enable = true;

  mine.japaneseInput = true;

  #boot.zfs.allowHibernation = true;
  #boot.zfs.forceImportRoot = false;

  #services.logind.lidSwitch = "suspend-then-hibernate";

  # The behavior of suspend-then-hibernate changed in systemd 252, see https://github.com/systemd/systemd/issues/25269
  #systemd.sleep.extraConfig = ''
  #  HibernateDelaySec=${toString (60 * 60)}
  #'';

  services.xserver.serverFlagsSection = ''
    Option "StandbyTime" "120"
    Option "SuspendTime" "120"
    Option "OffTime" "120"
    Option "BlankTime" "120"
  '';

  nix.package = pkgs.nixVersions.latest;

  nix.daemonCPUSchedPolicy = "idle";

  nix.settings.experimental-features = [ "flakes" "nix-command" ];
  nix.settings.trusted-users = [ "infinisil" "silchan" ];

  mine.dunst.enable = true;
  mine.firefox.enable = true;

  #networking.extraHosts = ''
  #  206.81.23.189 protos
  #  10.99.3.2 vario-via-protos
  #  192.168.0.12 vario-local
  #'';

  users.mutableUsers = false;

  users.users.infinisil = {
    uid = 1000;
    description = "Silvan Mosberger";
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
      "pipewire"
      "transmission"
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

  services.pipewire.systemWide = true;

  mine.mainUsers = [ "infinisil" "silchan" ];
  mine.console.users = [ "infinisil" "silchan" "root" ];

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

  nixpkgs.config.allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) [
    "helvetica-neue-lt-std"
  ];

  #networking.iphoneUsbTethering.enable = false;

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
    };
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/efi";
    };
  };

  networking.hostName = "void";
  networking.hostId = "26bfeffd";

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
    users = [ "infinisil" "silchan" ];
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = let
    reset = pkgs.writeShellScriptBin "reset" ''
      xrandr --output eDP-1 --mode 1920x1200
    '';
  in with pkgs; [
    reset
    flameshot
    nix-output-monitor
    chromium
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
    #discord
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
      mplus-outline-fonts.osdnRelease
    ];
  };



  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?
}
