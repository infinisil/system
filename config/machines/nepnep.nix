{ config, nodes, lib, pkgs, ... }: {

  imports = [
    ../hardware/pc
  ];

  mine.profiles.desktop.enable = true;

  mine.compton.highend = true;

  mine.shinas = true;

  mine.eth.sem6.enable = true;

  mine.dev.rust.enable = true;

  mine.server-sync = {
    enable = true;
    dataDir = "server/data";
    uploadDir = "server/upload";
    server = "infinisil.com";
  };

  mine.rpf.client = {
    server = nodes.yuri;
    webport = 8022;
    sshport = 2022;
    subdomain = "pc";
  };

  environment.systemPackages = with pkgs; [
    (writeScriptBin "projector" ''
      #!${stdenv.shell}
      xrandr --output HDMI-0 --mode 1920x1080 --output DP-4 --off
    '')
    (writeScriptBin "monitor" ''
      xrandr --output HDMI-0 --off --output DP-4 --mode 2560x1440
    '')
  ];

  mine.gaming.enable = true;

  services.nginx.virtualHosts.localhost = {
    basicAuth.infinisil = config.private.passwords."pc.infinisil.com";
    locations."/betty/" = {
      root = "/betty";
      extraConfig = "autoindex on;";
    };
  };

  #services.openvpn.servers.server = {
  #  up = ''
  #    ip route append 10.149.76.3 protocol static src 192.168.1.25 \
  #      nexthop dev eno1 || true
  #  '';
  #  down = ''
  #    ip route del 10.149.76.3 || true
  #  '';
  #};

  networking = {
    hostName = "paul";
    hostId = "56236562";
    extraHosts = ''
      192.168.1.1 swisscom.mobile
    '';
  };
}
