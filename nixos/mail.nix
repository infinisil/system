{ config, pkgs, ... }:
{
  imports = [
    (import pkgs.fetchFromGitHub {
      owner = "Infinisil";
      repo = "nixos-mailserver";
      rev = "master";
      sha256 = "1j0shf10da967w0hf2ldr6bdaafn023xjld00g0ahmrmgdfbh2ci";
    })
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

