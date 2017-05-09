# Edit this configuration file to define what should be installed on your system.  Help is available in the configuration.nix(5) man page and in the NixOS manual 
# (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  pkgs-unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {
    config = {};
  };
in
{

  imports =
    [ # Include the results of the hardware scan.
      hardware/mac.nix
      ./audio.nix
    ];

  virtualisation.docker.enable = false;

  system.autoUpgrade.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
    permittedInsecurePackages = [
      "libplist-1.12"
    ];
    packageOverrides = pkgs: {
      bluez = pkgs.bluez5;
    };
  };
  nix.useSandbox = true;
  nix.buildCores = 4;


  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.loader.grub.device = "/dev/sda";

  networking.hostId = "34cc680d";
  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

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
    irssi
    (pkgs.wrapFirefox (firefox-unwrapped.override {
        enableOfficialBranding = true;
    }) {} )
    libimobiledevice
    tilda
    feh # Sets wallpaper
    texlive.combined.scheme-medium
    termite
    opera
    shotcut # Video editor
    unison
  ];

  #environment.variables = { # Certainly takes effect after reboot, don't know how else
  #  HELLO = "Hi theer";
  #};

  programs.ssh.startAgent = true;

  services.urxvtd.enable = true;
  services.emacs.enable = true;
  services.illum.enable = true;
  services.openssh.enable = true;
  services.znapzend.enable = true;

  services.rsyncd = {
    #enable = true;
  };  
  #services.unclutter-xfixes.enable = true; # Doesn't seem to be doing anything

  services.ipfs.enable = true; # Needs to turn off when on battery
  services.ipfs.dataDir = "/home/shared/ipfs";

  #services.zfs.autoSnapshot.enable = true;
  systemd.timers.sync = {
    partOf = [ "sync.service" ];
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "*:*:0/10";
  };
  systemd.services.sync = {
    path = with pkgs; [ rsync openssh];
    script = ''
      rsync -avuz infinisil@infinisil.io:/global/ /global
      rsync -avuz --delete /global/ infinisil@infinisil.io:/global
    '';
  };

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
    extraGroups = [ "wheel" "networkmanager" "ipfs" ];
    shell = pkgs.zsh;
  };

  users.extraGroups.audio = {};

  security.sudo.wheelNeedsPassword = false;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}

