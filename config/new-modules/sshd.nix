{ config, lib, ... }:

with lib;

{

  options.mine.sshd.enable = mkOption {
    type = types.bool;
    default = true;
    description = "sshd config";
  };

  config = mkIf config.mine.sshd.enable {

    services.openssh = {
      enable = true;
      gatewayPorts = "clientspecified";
      extraConfig = ''
        ClientAliveInterval 15
      '';
    };

    networking.firewall.allowedTCPPorts = [ 22 ];

    users.users = mkMerge (map (user: {
      ${user}.openssh.authorizedKeys.keys = config.mine.allsshkeys;
    }) config.mine.mainUsers);

  };


}
