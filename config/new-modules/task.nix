{ config, lib, pkgs, ... }:

with lib;

{

  options.mine.taskclient.enable = mkEnableOption "tasknc client config";

  config = mkIf config.mine.taskclient.enable {

    environment.systemPackages = with pkgs; [
      taskwarrior
      tasknc
    ];

    mine.userConfig = {

      home.file.".taskrc".text = ''
        # For more documentation, see http://taskwarrior.org or try 'man task', 'man task-color',
        # 'man task-sync' or 'man taskrc'

        # Here is an example of entries that use the default, override and blank values
        #   variable=foo   -- By specifying a value, this overrides the default
        #   variable=      -- By specifying no value, this means no default
        #   #variable=foo  -- By commenting out the line, or deleting it, this uses the default

        # Use the command 'task show' to see all defaults and overrides

        # Files
        data.location=~/.local/share/task

        include ${pkgs.taskwarrior}/share/doc/task/rc/solarized-dark-256.theme
      '';

    };

  };
}
