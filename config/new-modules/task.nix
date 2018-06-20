{ config, lib, pkgs, ... }:

with lib;

{
  options.mine.taskclient.enable = mkEnableOption "taskwarrior client config";

  config = mkMerge [
    (mkIf config.mine.taskclient.enable {

      environment.systemPackages = with pkgs; [
        taskwarrior
        tasknc
      ];

      mine.userConfig = {
        home.file.".taskrc".text = let
          dataDir = "~/.local/share/task";
            in ''
          data.location=${dataDir}
          include ${pkgs.taskwarrior}/share/doc/task/rc/solarized-dark-256.theme

          taskd.certificate=${dataDir}/keys/public.cert
          taskd.key=${dataDir}/keys/private.key
          taskd.ca=${dataDir}/keys/ca.cert
          taskd.server=infinisil.com:53589
          taskd.credentials=private\/infinisil\/1d355ed7-6310-4ff9-adf6-b6e6046f5a42
        '';
      };
    })
    {

      services.taskserver = {
        fqdn = config.networking.domain;
        listenHost = "::";
        organisations.private.users = config.mine.mainUsers;
      };

    }
  ];
}
