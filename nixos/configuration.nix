# Edit this configuration file to define what should be installed on your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual 
# (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  system.autoUpgrade.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.loader.grub.device = "/dev/sda";
  
 
  networking.hostId = "34cc680d";
  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  nixpkgs.config.allowUnfree = true;


  time.timeZone = "Europe/Zurich";
  # List packages installed in system profile. To search by name, run: $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    emacs
    git
    ghc
    stack
    ghc
    xlibs.xmessage
    haskellPackages.ghc
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    vivaldi
    pass
    gnupg
    taskwarrior
    beets
    #buku Does not work right now, needs to add some python dependencies
  ];

  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  services.openssh.enable = true;
  programs.ssh.startAgent = true;

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
    #windowManager.xmonad.enable = true;
    #windowManager.xmonad.enableContribAndExtras = true;
    #windowManager.xmonad.extraPackages = self: [ self.xmonad-contrib ];
    # from github.com/bernerdschaefer/dotfiles/blob/bs-nixos/nixos/configuration.nix
    multitouch = {
      enable = true;
      invertScroll = true;
    };
    

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      horizontalScroll = true;
      palmDetect = true;
      tapButtons = true;
    };
  };

  services.compton.enable = true;

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      source-code-pro
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

