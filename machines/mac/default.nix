{ config, pkgs, lib, options, ... }:
{
  imports = [
    ./boot.nix
    <modules/audio.nix>
    <modules/base.nix>
    <modules/console.nix>
    <modules/localserver.nix>
    <modules/x.nix>
    <modules/touchpad.nix>
    <modules/wlan.nix>
    #<modules/mpdServer.nix>
    #<modules/cuberite.nix>
  ];

  mpd.local = true;

  hardware.cpu.intel.updateMicrocode = true;

  system.autoUpgrade.enable = true;

  users.extraUsers.infinisil.extraGroups = [ "networkmanager" ];

  users.extraUsers.testuser = {
    description = "Test User";
    isNormalUser = true;
  };
  #fileSystems."/betty" = lib.mkForce {
  #  device = "betty";
  #  fsType = "zfs";
  #  options = [
  #    "x-systemd.automount"
  #    "x-systemd.device-timeout=10"
  #  ];
  #};

  virtualisation = {
    docker.enable = false;
    virtualbox = {
      host.enable = true;
    };
  };

  nix = {
    useSandbox = true;
    buildCores = 0; # Makes it use all CPUs
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    nixPath = [
      "nixpkgs=/root/nixpkgs"
      "nixos-config=/cfg/machines/mac"
      "modules=/cfg/modules"
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



  environment.systemPackages = with pkgs; [
    efibootmgr
    pass
    gnupg
    taskwarrior
    neofetch
    openssl
    tldr
    youtube-dl
    mtr
    nix-repl
    iftop
    atool
    nox
    tilda
    texlive.combined.scheme-full # full needed for emacs pdf config
    #(wine.override { wineBuild = "wineWow"; })
    acpi
    tmux
    mpc_cli
    lm_sensors
    libnotify
    twmn
    dunst
    mpc_cli
    jq
    samba
  ];

  environment.variables = {
    PATH = "/global/system/bin";
  };

  services = {
    znapzend = {
      enable = true;
      autoCreation = true;
    };

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

    dbus.socketActivated = true;
    physlock.enable = true;

    ipfs = {
      enable = true;
      dataDir = "/var/lib/ipfs";
    };
  };

  systemd.services.ipfs.wantedBy = lib.mkForce [];

  system.stateVersion = "16.09";
}
