{ config, ... }:
let

  port = 2362;

in {

  networking.firewall.allowedTCPPorts = [ port ];

}
