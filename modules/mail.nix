{ config, pkgs, ... }:
{
  imports = [
    /home/infinisil/src/nixos-mailserver
  ];

  mailserver = {
    enable = false;
    domain = "infinisil.io";
    loginAccounts = [
      {
        name = "contact";
        hashedPassword = "$6$XUfpGpld$a1uW9TvvHpVWgTYj0JSzQSmc2r88pjCDLvXHc4D6WRAaPtAMVx1VuwnVQ2NkjZiHeioQWnDd93Blo2ipOoBNb1";
      }
    ];
    virtualAliases = {
      admin = "contact";
    };
  };
}

