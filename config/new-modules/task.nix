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
          taskd.credentials=private\/infinisil\/ed52a0a7-73e6-4fc8-9fad-009f6a2d87af
          confirmation=no
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
