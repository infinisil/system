{ config, pkgs, ... }:
{
  imports = [
    (builtins.fetchTarball "github.com/Infinisil/nixos-mailserver/archive/master.tar.gz")
  ];

  mailserver = {
    enable = true;
    domain = "infinisil.io";
    login_accounts = [
      {
        name = "contact";
        hashedPassword = "$6$1KkG2bzCymm2$xvy9FtswaF9sTHiTCJhc0pOBVTF3mT2/W5Mcuz29GpbZ.Swq53zhy9NNYjqKZEphQVfZOjKnY2631JfRmVH.1/";
      }
    ];
    valiases = {
      admin = "contact";
    };
  };
}

