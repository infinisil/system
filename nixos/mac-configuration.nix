# Edit this configuration file to define what should be installed on your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual 
# (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  pkgs-unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {
    config = {};
  };

  sink = "bluez_sink.04_52_C7_33_59_A8.headset_head_unit";
in
{

  imports =
    [ # Include the results of the hardware scan.
      hardware/mac.nix
    ];

  fileSystems."/home/shared" =
    { device = "main/home/shared";
      fsType = "zfs";
    };

  virtualisation.docker.enable = false;

  system.autoUpgrade.enable = true;

  nixpkgs.config.permittedInsecurePackages = [
    "libplist-1.12"
  ];
  nixpkgs.config.packageOverrides = pkgs: {
    bluez = pkgs.bluez5;
  };


  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.loader.grub.device = "/dev/sda";

  networking.hostId = "34cc680d";
  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;

  nix.useSandbox = true;
  nix.buildCores = 4;

  time.timeZone = "Europe/Zurich";
  # List packages installed in system profile. To search by name, run: $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    (haskellPackages.ghcWithPackages (pkgs: [
      pkgs.xmobar
      pkgs.xmonad
      pkgs.xmonad-contrib
      pkgs.xmonad-extras
    ]))
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
    neofetch
    neovim
    thunderbird
    sakura
    tldr
    youtube-dl
    perl
    python
    nix-repl
    pkgs-unstable.buku
    franz
    mpd
    xbindkeys
    xbindkeys-config
    xlibs.xev
    pkgs-unstable.albert
    irssi
    (pkgs.wrapFirefox (firefox-unwrapped.override {
      enableOfficialBranding = true;
    }) {} )
    libimobiledevice
    tilda
    feh # Sets wallpaper
    sonata # mpd GUI client
    texlive.combined.scheme-medium
    termite
    bluez
    blueman
    flat-plat
  ];

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
      load-module module-switch-on-connect
    '';
  };
  hardware.bluetooth.enable = true;

  programs.ssh.startAgent = true;

  services.mpd = { 
    enable = true;
    musicDirectory = "/home/shared/beets";
    dataDir = "/home/shared/mpd";
    dbFile = "/home/shared/mpd/mpd.db";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "MPD PulseAudio Output"
        server "127.0.0.1"
      }
    '';
  };

  services.urxvtd.enable = true;
  services.emacs.enable = true;
  services.illum.enable = true;
  services.openssh.enable = true;
  services.znapzend.enable = true;
  #services.unclutter-xfixes.enable = true; # Doesn't seem to be doing anything

  #services.ipfs.enable = true; # Needs to turn off when on battery
  #services.ipfs.dataDir = "/ipfs";

  services.zfs.autoSnapshot.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvp";
    xkbOptions = "caps:backspace";

    autoRepeatDelay = 250;
    autoRepeatInterval = 30;

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "infinisil";

    desktopManager.default = "none";

    windowManager.default = "xmonad";
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    #windowManager.xmonad.extraPackages = self: [ self.xmonad-contrib ];
    # from github.com/bernerdschaefer/dotfiles/blob/bs-nixos/nixos/configuration.nix
    multitouch = {
      enable = true;
      invertScroll = true;
      buttonsMap = [1 3 2];
    };
    synaptics = {
      #enable = true; # Only applies when multitouch is disabled
      #buttonsMap = [ 1 3 2 ];
      tapButtons = true;
      twoFingerScroll = true;
      horizTwoFingerScroll = true;
      scrollDelta = 10;
      minSpeed = "0.7";
      maxSpeed = "1.7";
      palmDetect = true;
      additionalOptions = ''
        Option "FingerHigh" "50"
        Option "FingerLow" "30"
        Option "TapAndDragGesture" "off"
        Option "TapButton1" "1"
        Option "TapButton2" "3"
        Option "TapButton3" "2"
        Option "VertScrollDelta" "-500"
        Option "HorizScrollDelta" "-500"
      '';
    };
  };

  services.compton = {
    enable = true;
    backend = "glx";
    vSync = "opengl-swc";
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
    extraGroups = [ "wheel" "networkmanager" "mpd" ];
    shell = pkgs.zsh;
  };

  security.sudo.wheelNeedsPassword = false;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}

