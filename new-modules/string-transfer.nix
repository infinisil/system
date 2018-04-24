{ lib, config, ... }:

with lib;

let

  port = 3464;

in

{

  options.mine.string-transfer.enable = mkEnableOption "string transfer aliases";

  config = mkIf config.mine.string-transfer.enable {

    networking.firewall.allowedTCPPorts = [ port ];

    mine.userConfig.programs.zsh.initExtra = ''
      function send-string() {
        ncat -l ${toString port} -k -c "echo $*" --send-only
      }

      function recv-string() {
        ncat $1 ${toString port} --recv-only
      }
    '';
  };

}
