# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, fetchFromGitHub, ... }:

let

  pkgs2 = import <nixpkgs> { config = {}; };

  systemd-digitalocean = pkgs2.fetchFromGitHub {
    owner = "edef1c";
    repo = "systemd-digitalocean";
    rev = "8e232ce77ae04421a99be6787f1d185e49ebbc63";
    sha256 = "1635zqfj1yip5ixmakxy370ya1q30xg8wxzn9gn3623bba1s28sh";
  };

    davnginx = pkgs.nginxMainline.overrideAttrs (old: {
    modules = [
      (pkgs.fetchFromGitHub {
        owner = "arut";
        repo = "nginx-dav-ext-module";
        rev = "v0.0.3";
        sha256 = "1qck8jclxddncjad8yv911s9z7lrd58bp96jf13m0iqk54xghx91";
      })
    ];
  });

in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      #systemd-digitalocean.outPath
      ../console.nix
      ../users.nix
      ../ssh.nix
    ];

  
  environment.systemPackages = with pkgs; [
    git
    coreutils
    neovim
    tmux
    iperf
    bind
  ];

  security.sudo.wheelNeedsPassword = false;

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";
  boot.zfs.devNodes = "/dev/";

 
  networking = {
    hostId = "ecb69508";
    hostName = "dobby";
    defaultGateway = "207.154.240.1";
    defaultGateway6 = "2a03:b0c0:3:d0::1";
    usePredictableInterfaceNames = false;
    interfaces.eth0 = {
      ipAddress = "207.154.251.58";
      ipv6Address = "2a03:b0c0:3:d0::47dc:3001";
      prefixLength = 20;
    };
  };

  services.openssh.enable = true;  
  services.openssh.passwordAuthentication = false;
  services.fail2ban.enable = true;

  networking.firewall.allowedTCPPorts = [22 53 80 443 5201 2022 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
  services.nginx = {
    enable = true;
    package = davnginx;
    virtualHosts."infinisil.io" = {
      forceSSL = true;
      enableACME = true;
      root = "/webroot";
    };
    virtualHosts."mac.infinisil.io" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://localhost:81";
      };
    };
    virtualHosts."dav.infinisil.io" = {
      enableACME = true;
      forceSSL = true;
      root = "/webroot/dav";
      locations."/" = {
        extraConfig = ''
          dav_methods PUT DELETE MKCOL COPY MOVE;
          dav_ext_methods PROPFIND OPTIONS;
          dav_access user:rw group:r;

          autoindex on;
        '';
      };
    };
  };

  environment.variables.PATH = "/global/nixpkgs/result/bin";

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  # environment.systemPackages = with pkgs; [
  #   wget
  # ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

}
