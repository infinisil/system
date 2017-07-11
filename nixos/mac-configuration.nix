
{ config, pkgs, ... }:

let
  
  unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {
      config = {};
  };

in
{
  imports =
    [ # Include the results of the hardware scan.
      hardware/mac.nix
      ./audio.nix
      ./say.nix
      ./mozilla.nix
      ./console.nix
      ./users.nix
      ./ssh.nix
    ];

  hardware.cpu.intel.updateMicrocode = true;

  system.extraSystemBuilderCmds = "ln -sv ${./.}";
  system.autoUpgrade.enable = true;

  i18n.consoleUseXkbConfig = true;

  #virtualisation.docker.enable = true;
  #virtualisation.virtualbox.host.enable = true;


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

  networking = {
    #nameservers = [
      #  "207.154.251.58"
      #];
    hostId = "34cc680d";
    hostName = "nixos";
    wireless.enable = true;
    firewall = {
      allowedTCPPorts = [ 139 445 ];
      allowedUDPPorts = [ 137 138 ];
    };
  };

  services.autossh.sessions = [
    {
      extraArguments = "-o \"ServerAliveInterval 30\" -o \"ServerAliveCountMax 3\" -N -R 81:localhost:8080 root@infinisil.io";
      name = "localserver";
      user = "root";
    }
  ];


  services.nginx = {
    enable = true;
    virtualHosts."mac.infinisil.io" = {
      root = "/webroot";
      port = 8080;
    };
  };


  boot = {
    loader.systemd-boot.enable = true;
    loader.grub = {
      efiSupport = true;
      #enable = true;
      device = "/dev/sda";
      configurationLimit = 10;
    };
    cleanTmpDir = true;
    supportedFilesystems = [ "zfs" ];
  };
      

  time.timeZone = "Europe/Zurich";
  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithPackages (self: [ self.xmobar ]))
    wget
    git
    pass
    gnupg
    taskwarrior
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
    #unstable.buku
    franz
    mpd
    xbindkeys
    xbindkeys-config
    xlibs.xev
    irssi
    #firefox
    tilda
    #unstable.feh # Sets wallpaper
    texlive.combined.scheme-full # full needed for emacs pdf config
    #shotcut # Video editor
    zulu
    cacert
    #(wine.override { wineBuild = "wineWow"; })
    acpi
    tmux
    lm_sensors
    efivar
    hardinfo
    ipfs
    libnotify
    twmn
    dunst
    jq
    arandr
    cava
    autossh
    vlc
    arc-theme
    gtk_engines
    gtk-engine-murrine

    ripgrep
    gnome3.gnome_terminal
    samba
    compton
  ];

  environment.variables = {
    PATH = "/global/nixpkgs/result/bin:/global/system/bin";
  };


  services.urxvtd.enable = true;
  services.emacs.enable = true;
  services.illum.enable = true;
  services.znapzend.enable = true;
  services.samba = {
    enable = true;
    syncPasswordsByPam = true;
    shares = {
      root.path = "/";
    };
  };
  

  services.ipfs = {
    #enable = true;
    dataDir = "/ipfs";
  };

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvp";
    xkbOptions = "caps:backspace";
    wacom.enable = true;

    #useXFS = true Is this needed?

    autoRepeatDelay = 250;
    autoRepeatInterval = 30;

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "infinisil";
    displayManager.sessionCommands = ''
        # Set GTK_DATA_PREFIX so that GTK+ can find the themes
        export GTK_DATA_PREFIX=${config.system.path}

        # find theme engines
        export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
      '';

    desktopManager.default = "none";

    windowManager.default = "xmonad";
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.xmonad.extraPackages = self: [ self.xmobar ];
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
    enable = false;
    backend = "glx";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      nerdfonts
    ];
  };

  users.extraGroups.audio = {};

  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "16.09";
}
