{ nodes, lib, pkgs, ... }: {

  imports = [
    ../hardware/pc
  ];

  mine.profiles.desktop.enable = true;

  mine.shinas = true;

  mine.rpf.client = {
    server = nodes.yuri;
    webport = 8022;
    sshport = 2022;
    subdomain = "pc";
  };

  environment.systemPackages = with pkgs; [
    steam
  ];

  nix.nixPath = [
    "$HOME/.nix"
    "nixpkgs=${lib.cleanSource <nixpkgs>}"
  ];

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
  };
}
