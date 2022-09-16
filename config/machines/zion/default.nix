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
      ../../personal/key-layout.nix
      ((import ../../../nix/sources.nix {}).nixos-hardware + "/framework")
      ../../personal/bins.nix
    ];

  nix.settings.experimental-features = [ "flakes" "nix-command" ];

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


  mine.xUserConfig = {
    services.random-background = {
      enable = true;
      imageDirectory = "%h/background";
      interval = "120";
    };
  };

  services.clight.enable = true;

  location.latitude = 47.4;
  location.longitude = 9.2;

  mine.sound.enable = true;
  mine.vim.enable = true;

  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

  boot.initrd.availableKernelModules = [
    # https://wiki.archlinux.org/title/Kernel_mode_setting#Early_KMS_start
    "i915"
  ];

  boot.kernelParams = [
    "drm.edid_firmware=DP-1:edid/rogswift.bin"
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

  services.autorandr = {
    enable = true;
    defaultTarget = "--debug default";
    hooks.postswitch = {
      "restart-xmonad" = ''
        xmonad --restart
        pkill -x xmobar-custom
      '';
    };
    profiles = {
      default = {
        fingerprint = {
          eDP-1 = "00ffffffffffff0009e55f0900000000171d0104a51c137803de50a3544c99260f505400000001010101010101010101010101010101115cd01881e02d50302036001dbe1000001aa749d01881e02d50302036001dbe1000001a000000fe00424f452043510a202020202020000000fe004e4531333546424d2d4e34310a00fb";
        };
        config.eDP-1 = {
          enable = true;
          mode = "2256x1504";
        };
      };
      docked = {
        fingerprint = {
          eDP-1 = "00ffffffffffff0009e55f0900000000171d0104a51c137803de50a3544c99260f505400000001010101010101010101010101010101115cd01881e02d50302036001dbe1000001aa749d01881e02d50302036001dbe1000001a000000fe00424f452043510a202020202020000000fe004e4531333546424d2d4e34310a00fb";
          DP-1 = "00ffffffffffff000469b127758201002b180104a53c2278064ce1a55850a0230b505400000001010101010101010101010101010101565e00a0a0a029503020350056502100001a000000ff002341534e536230494c30655064000000fd001e961ed236010a202020202020000000fc00524f47205047323738510a2020015002030a01654b040001015a8700a0a0a03b503020350056502100001a5aa000a0a0a046503020350056502100001a6fc200a0a0a055503020350056502100001a74d20016a0a009500410110056502100001e1c2500a0a0a011503020350056502100001a000000000000000000000000000000000000000000000000000000af";
        };
        config = {
          eDP-1 = {
            enable = true;
            mode = "2256x1504";
            position = "0x0";
          };
          DP-1 = {
            enable = true;
            mode = "2560x1440";
            primary = true;
            position = "2256x0";
            rate = "120";
            # Tries to be smart with gamma, but misses the mark, no custom gamma is needed
            gamma = "1.0:1.0:1.0";
          };
        };
      };
    };
  };

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

  mine.terminal.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver = {
    displayManager = {
      lightdm = {
        enable = true;
        extraSeatDefaults = ''
          display-setup-script=${pkgs.writeShellScript "autorandr-test" ''
            /run/current-system/sw/bin/autorandr -c &>> /autorandr-output
          ''}
        '';
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
    users = [ "infinisil" "tweagysil" ];
  };

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

