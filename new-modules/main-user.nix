{ config, lib, ... }:

with lib;

{

  # TODO: Have an option to set which user needs X stuff (root doesn't)

  options.mine = {

    mainUsers = mkOption {
      type = types.listOf types.str;
      example = [ "paul" "john" ];
      default = [];
      description = "Main users for this system";
    };

    userConfig = mkOption {
      # Should be home-manager submodule
      type = types.unspecified;
      description = "Home-manager configuration to be used for all main users";
    };

  };

  config = {

    home-manager.users = mkMerge (map (user: {

      "${user}" = config.mine.userConfig;

    }) config.mine.mainUsers);

    assertions = map (user: {
      assertion = builtins.hasAttr user config.users.users;
      message = "The main user ${user} has to exist";
    }) config.mine.mainUsers;

  };

}
