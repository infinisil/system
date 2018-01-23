{ config, lib, ... }:

with lib;

{

  options.mine.mainUser = mkOption {
    type = types.nullOr types.str;
    example = "paul";
    description = "Main user for this system";
  };

  config.assertions = [{
    assertion = config.mine.mainUser != null -> builtins.hasAttr config.mine.mainUser config.users.users;
    message = "The main user ${config.mine.mainUser} has to exist";
  }];

}
