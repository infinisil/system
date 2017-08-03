{ config, pkgs, ... }:
{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./audio.nix
    ./say.nix
    ./base.nix
    ./console.nix
    ./localserver.nix
    ./x.nix
  ];

  hardware.cpu.intel.updateMicrocode = true;

  security.sudo.wheelNeedsPassword = false;

  zramSwap.enable = true;

  system.extraSystemBuilderCmds = "ln -sv ${./.}";
  system.autoUpgrade.enable = true;

  virtualisation = {
    docker.enable = false;
    virtualbox = {
      host.enable = true;
      guest.enable = true;
    };
  };

  nix = {
    buildCores = 0; # Makes it use all CPUs
    autoOptimiseStore = true;
    package = pkgs.nixUnstable;
  };

  networking = {
    hostName = "nixos";
    hostId = "34cc680d";
    nameservers = [
      "207.154.251.58"
    ];
    wireless.enable = true;
    firewall = {
      allowedTCPPorts = [ 139 445 ];
      allowedUDPPorts = [ 137 138 ];
    };
    extraHosts = ''
      192.168.1.1 swisscom.mobile
    '';
  };

  boot = {
    loader.systemd-boot.enable = true;
    cleanTmpDir = true;
    supportedFilesystems = [ "zfs" ];
  };
      

  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithPackages (self: [ self.xmobar ]))
    pass
    gnupg
    taskwarrior
    asciinema
    neofetch
    wget
    neovim
    thunderbird
    sakura
    tldr
    youtube-dl
    perl
    python
    nix-repl
    franz
    mpd
    xbindkeys
    xbindkeys-config
    xlibs.xev
    #firefox
    tilda
    #unstable.feh # Sets wallpaper
    #texlive.combined.scheme-full # full needed for emacs pdf config
    #shotcut # Video editor
    zulu
    cacert
    #(wine.override { wineBuild = "wineWow"; })
    acpi
    tmux
    lm_sensors
    efivar
    hardinfo
    libnotify
    twmn
    dunst
    jq
    arandr
    cava
    autossh
    vlc
    arc-theme
    numix-gtk-theme
    gtk_engines
    gtk-engine-murrine
    gnome3.gnome_terminal
    samba
    compton
  ];

  environment.variables = {
    PATH = "/global/system/bin";
  };

  services = {
    emacs.enable = true;

    znapzend.enable = true;
    
    ipfs.enable = false;

    samba = {
      enable = true;
      shares = {
        root.path = "/";
      };
    };
  };

  programs.command-not-found.enable = true;

  system.stateVersion = "16.09";
}
