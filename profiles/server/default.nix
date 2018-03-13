{ nodes, config, lib, pkgs, ... }:

{

  imports = [
    ../../modules/radicale.nix
    ../../modules/remote.nix
    ../../modules/git-host.nix
    ../../private/server.nix
    #/home/infinisil/eth/DS/CardGame/Server/module.nix
    #/home/infinisil/prj/nixbot/module.nix
  ];

}
