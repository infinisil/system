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
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      #systemd-digitalocean.outPath
      ../console.nix
      ../users.nix
      ../ssh.nix
      ./radicale.nix
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
    nameservers = [ "8.8.8.8" ];
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

  services.bind = {
    enable = true;
    cacheNetworks = [
      "127.0.0.0/24"
      "178.197.227.134/32"
    ];
    blockedNetworks = [
      "45.34.184.76/32"
      "162.211.182.144/32"
      "122.114.194.226/32"
      "202.101.42.220/32"
      "182.110.69.53/32"
      "192.126.114.237/32"
    ];
    zones = [
      {
        file = "/var/dns/infinisil.io";
        master = true;
        name = "infinisil.io";
      }
    ];
  };

  networking.firewall.allowedTCPPorts = [22 53 80 443 5201 2022 5232 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
  services.nginx = {
    enable = true;
    #package = davnginx;
    virtualHosts."infinisil.io" = {
      #forceSSL = true;
     # enableACME = true;
      root = "/webroot";
    };
    virtualHosts."mac.infinisil.io" = {
      #forceSSL = true;
      #enableACME = true;
      locations."/" = {
        proxyPass = "http://localhost:81";
      };
    };
  };

  environment.variables.PATH = "/global/nixpkgs/result/bin";

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
