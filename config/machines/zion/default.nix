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
    ];

  mine.sound.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) [
    "helvetica-neue-lt-std"
  ];

  mine.enableUser = true;

  mine.console.enable = false;

  i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "zion";
  networking.hostId = "e585b53a";
  boot.zfs.devNodes = "/dev";
  boot.loader.efi.efiSysMountPoint = "/efi";

  services.fprintd.enable = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Zurich";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  mine.terminal.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver = {
    displayManager = {
      lightdm = {
        enable = true;
        #background = toString config.mine.assets.blurred;
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

  #mine.xUserConfig = {
  #  xsession.enable = true;
  #  xsession.windowManager.xmonad = {
  #    enable = true;
  #    extraPackages = self: [ self.fuzzy ];
  #    enableContribAndExtras = true;
  #    config = pkgs.runCommand "xmonad.hs" config.scripts "substituteAll ${../../new-modules/x/wm/xmonad.hs} $out";
  #  };
  #};

  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 25;
  services.xserver.xkbOptions = "caps:backspace";
  # "compose???" ];
  services.xserver.xkbVariant = "dvp";

  mine.userConfig.home.keyboard = null;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  #   packages = with pkgs; [
  #     firefox
  #     thunderbird
  #   ];
  # };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
    htop
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  services.xserver.libinput.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHjY4cuUk4IWgBgnEJSULkIHO+njUmIFP+WSWy7IobBs"
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  #mine.x.enable = true;
  #mine.wm.enable = true;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}

