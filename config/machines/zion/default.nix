# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../../new-modules
      ../../assets
      ../../modules
      ../../personal/user.nix
      ((import ../../../nix/sources.nix {}).nixos-hardware + "/framework")
      ../../personal/bins.nix
    ];

  users.mutableUsers = false;

  hardware.bluetooth.enable = true;

  mine.userConfig = {
    services.gpg-agent = {
      enable = true;
      extraConfig = ''
        pinentry-program ${pkgs.pinentry.qt}/bin/pinentry
      '';
    };
  };

  services.clight.enable = true;

  location.latitude = 47.4;
  location.longitude = 9.2;

  mine.sound.enable = true;
  mine.vim.enable = true;

  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  services.fwupd.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) [
    "helvetica-neue-lt-std"
    "slack"
  ];

  networking.iphoneUsbTethering.enable = true;

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

  console = {
    useXkbConfig = true; # use xkbOptions in tty.
  };

  mine.terminal.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver = {
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
    modifier = 1;
    switchWorkspaceKeys = [
      "M-&"
      "M-["
      "M-{"
      "M-}"
      "M-("
      "M-="
      "M-*"
      "M-)"
      "M-+"
      "M-]"
      "M-!"
      "M-#"
    ];
    shiftWorkspaceKeys = [
      "M-S-&"
      "M-S-["
      "M-S-{"
      "M-S-}"
      "M-S-("
      "M-S-="
      "M-S-*"
      "M-S-)"
      "M-S-+"
      "M-S-]"
      "M-S-!"
      "M-S-#"
    ];
  };

  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 25;
  services.xserver.xkbOptions = "caps:backspace";
  # "compose???" ];
  services.xserver.xkbVariant = "dvp";

  mine.userConfig.home.keyboard = null;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    htop
    git
    slack
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.naturalScrolling = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      (nerdfonts.override {
        fonts = [
          "Iosevka"
          "FiraMono"
          "FantasqueSansMono"
        ];
      })
      hanazono
      ipafont
      mplus-outline-fonts.osdnRelease
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts
      wqy_zenhei
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

