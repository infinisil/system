{ config, pkgs, options, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./audio.nix
    ./say.nix
    ./base.nix
    ./console.nix
    ./localserver.nix
    ./x.nix
    #./ss.nix
    ./extraUsers.nix
    ./touchpad.nix
  ];
  programs.oblogout.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  security.sudo.wheelNeedsPassword = false;

  system.extraSystemBuilderCmds = "ln -sv ${./.}";
  system.autoUpgrade.enable = true;

  users.extraUsers.infinisil.extraGroups = [ "systemd-journal" "networkmanager" ];

  virtualisation = {
    docker.enable = false;
    virtualbox = {
      host.enable = false;
    };
  };

  nix = {
    useSandbox = true;
    trustedUsers = [ "root" "@wheel" ];
    buildCores = 0; # Makes it use all CPUs
    autoOptimiseStore = true;
    #package = pkgs.nixUnstable;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    nixPath = [
      "nixpkgs=/root/nixpkgs"
      "nixos-config=/cfg/system/nixos/mac.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  networking = {
    hostName = "nixos";
    hostId = "34cc680d";
    nameservers = [
      "139.59.149.43"
    ];
    wireless.enable = true;
    firewall = {
      # For samba
      allowedTCPPorts = [ 139 445 ];
      allowedUDPPorts = [ 137 138 ];
    };
    extraHosts = ''
      192.168.1.1 swisscom.mobile
    '';
  };

  boot = {
    initrd.luks.devices = {
      key = {
        device = "/dev/disk/by-partlabel/key";
      };
      root = {
        device = "/dev/disk/by-uuid/bcf2dd01-ef96-412a-a458-0bd0437cd83a";
        keyFile = "/dev/mapper/key";
      };
      swap = {
        device = "/dev/disk/by-uuid/59515706-b6b7-4823-95e3-b1d930aca2f8";
        keyFile = "/dev/mapper/key";
      };
    };

    loader.grub = {
      enable = true;
      splashImage = null;
      mirroredBoots = [
        {
          devices = [ "nodev" ];
          path = "/boot/1";
        }
        {
          devices = [ "nodev" ];
          path = "/boot/2";
        }
        {
          devices = [ "nodev" ];
          path = "/boot/3";
        }
      ];
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
    zfs.devNodes = "/dev/mapper";
    cleanTmpDir = true;
    supportedFilesystems = [ "zfs" ];
  };

  environment.systemPackages = with pkgs; [
    haskellPackages.xmobar
    (haskellPackages.ghcWithPackages (p: [ p.fuzzy ]))
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
    franz
    mpd
    deluge
    xbindkeys
    xbindkeys-config
    dmenu
    xlibs.xev
    firefox
    tilda
    #unstable.feh # Sets wallpaper
    texlive.combined.scheme-full # full needed for emacs pdf config
    #shotcut # Video editor
    zulu
    cacert
    #(wine.override { wineBuild = "wineWow"; })
    acpi
    tmux
    mpc_cli
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
    #arc-theme
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

    znapzend = {
      enable = true;
      autoCreation = true;
    };
    
    ipfs.enable = false;

    samba = {
      enable = true;
      shares = {
        root = {
          path = "/";
          "read only" = false;
        };
        betty = {
          path = "/betty";
          "read only" = false;
        };
      };
    };
    
    physlock.enable = true;
  };

  programs.command-not-found.enable = true;

  system.stateVersion = "16.09";
}
