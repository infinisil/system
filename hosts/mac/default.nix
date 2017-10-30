{ config, pkgs, lib, options, ... }:

{
  imports = [
    ./boot.nix
    <cfg/modules/audio.nix>
    <cfg/modules/base.nix>
    <cfg/modules/console.nix>
    <cfg/modules/localserver.nix>
    <cfg/modules/x.nix>
    <cfg/modules/touchpad.nix>
    <cfg/modules/wlan.nix>
  ];

  mpd.local = true;

  hardware.cpu.intel.updateMicrocode = true;

  system.autoUpgrade.enable = true;

  users.extraUsers.infinisil.extraGroups = [ "networkmanager" ];

  users.extraUsers.testuser = {
    description = "Test User";
    isNormalUser = true;
  };

  users.users.root.openssh.authorizedKeys.keys = [
    (import <cfg/modules/keys.nix>).mac.nixos.root
    (import <cfg/modules/keys.nix>).nixops.mac
  ];
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
      "cfg=/cfg"
      "nixpkgs=/cfg/nixpkgs"
      "nixos-config=/cfg/hosts/mac"
    ];
  };

  networking = {
    hostName = "nixos";
    hostId = "34cc680d";
    nameservers = [
      (import ../.).dobby.networking.interfaces.eth0.ipAddress
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
    nixops
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
    PATH = "/cfg/bin";
  };

  services = {

    usbmuxd.enable = true;

    znapzend = {
      enable = true;
      autoCreation = true;
    };

    zfs.autoScrub.enable = true;

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
