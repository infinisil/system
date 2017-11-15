{ config, ... }:

{

  imports = [

  ];

  users.extraUsers.testuser = {
    description = "Test User";
    isNormalUser = true;
  };

}
