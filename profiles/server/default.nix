{ config, lib, pkgs, ... }:

{

  imports = [
    ../../modules/web.nix
    ../../modules/ssh.nix
    ../../modules/radicale.nix
    ../../modules/bind.nix
    ../../modules/console.nix
    ../../modules/mpdServer.nix
    ../../modules/namecoin.nix
    ../../modules/youtube.nix
    ../../modules/info.nix
    ../../modules/chatserver
    ../../modules/ipfs.nix
    ../../modules/znc.nix
    ../../private/server.nix
    /home/infinisil/eth/DS/CardGame/Server/module.nix
  ];

  boot.loader.timeout = 60;

  # minimalization, taken from <nixpkgs/nixos/modules/profiles>
  sound.enable = false;
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];
  systemd.enableEmergencyMode = false;
  fonts.fontconfig.enable = false;
  programs.ssh.setXAuthLocation = false;
  security.pam.services.su.forwardXAuth = lib.mkForce false;
  #environment.noXlibs = true;
  i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];
  services.nixosManual.enable = false;
  programs.info.enable = false;

  environment.systemPackages = with pkgs; [
    youtube-dl
    iperf
  ];

  networking.firewall.allowedTCPPorts = [
    5001 # iperf
  ];

  networking.firewall.logRefusedConnections = true;

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  services.ipfs.autostart = true;

  nix = {
    autoOptimiseStore = true;
    nixPath = [
      # Ruin the config so we don't accidentally run
      # nixos-rebuild switch on the host (thanks grahamc!)
      "nixos-config=${pkgs.writeText "throw-configuration.nix" ''
        throw "Hey dummy, you're on your server! Use NixOps!"
      ''}"
      "nixpkgs=/run/current-system/nixpkgs"
    ];
  };

  system.extraSystemBuilderCmds = ''
    ln -sv ${lib.cleanSource pkgs.path} $out/nixpkgs
  '';

  services.nginx = {
    virtualHosts."mac.${config.networking.domain}" = {
      forceSSL = true;
      enableACME = true;
      root = "/webroot/mac";
      locations."/".proxyPass = "http://localhost:1808";
    };
  };

  networking.subdomains = [ "test" ];

  services.nginx.virtualHosts."test.${config.networking.domain}" = {
    forceSSL = true;
    enableACME = true;
    root = "/webroot/test";
    locations."/".extraConfig = "autoindex on;";
  };

}
