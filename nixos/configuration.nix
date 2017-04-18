# Edit this configuration file to define what should be installed on your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual 
# (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  fileSystems."/home/shared" =
    { device = "main/home/shared";
      fsType = "zfs";
    };

  system.autoUpgrade.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.loader.grub.device = "/dev/sda";

  nixpkgs.config.permittedInsecurePackages = [
    "libplist-1.12"
  ];
  
 
  networking.hostId = "34cc680d";
  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;

  nix.useSandbox = true;

  time.timeZone = "Europe/Zurich";
  # List packages installed in system profile. To search by name, run: $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    emacs
    git
    (haskellPackages.ghcWithPackages (pkgs: [
      pkgs.xmonad
      pkgs.xmonad-contrib
      pkgs.xmonad-extras
    ]))
    #haskellPackages.haskellPlatform
    #haskellPackages.xmonad
    #haskellPackages.xmonad-contrib
    #haskellPackages.xmonad-extras
    vivaldi
    pass
    gnupg
    taskwarrior
    beets
    ponysay
    fortune
    cowsay
    cmatrix
    asciinema
    terminator
    neofetch
    neovim
    thunderbird
    sakura
    tldr
    youtube-dl
    perl
    python
    nix-repl
    libplist
    buku
    franz
    mpd
    xbindkeys
    xbindkeys-config
    xlibs.xev
  ];

  hardware.pulseaudio.enable = true;
  
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  services.openssh.enable = true;
  programs.ssh.startAgent = true;

  services.mpd = {
    enable = true;
    musicDirectory = "/home/shared/beets";
    dataDir = "/home/shared/mpd";
    dbFile = "/home/shared/mpd/mpd.db";
  };

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvp";
    
    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "infinisil";
    
    desktopManager.xfce.enable = true;    
    #desktopManager.xfce.enableXfwm = false;
    desktopManager.default = "xfce";

    #windowManager.default = "xmonad";
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    #windowManager.xmonad.extraPackages = self: [ self.xmonad-contrib ];
    # from github.com/bernerdschaefer/dotfiles/blob/bs-nixos/nixos/configuration.nix
    multitouch = {
      enable = true;
      invertScroll = true;
    };
    

    synaptics = {
      enable = true;
      buttonsMap = [ 1 3 2 ]; # 1: left, 2: right, 3: middle
      twoFingerScroll = true;
      horizTwoFingerScroll = true;
      horizontalScroll = true;
      palmDetect = true;
      tapButtons = true;
    };
  };

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "opengl-swc";
    shadow = true;

    extraOptions =
    ''
      glx-swap-method = "3";
      no-dock-shadow = true;
      paint-on-overlay = true;
      sw-opti = true;
    '';
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      source-code-pro
      powerline-fonts
      nerdfonts
    ];
  };

  programs.zsh.enable = true;
  
  users.extraUsers.infinisil = {
    isNormalUser = true;
    home = "/home/infinisil";
    description = "Silvan Mosberger";
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.zsh;
  };


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}

