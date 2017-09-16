{
  imports = [
    /home/infinisil/src/nixpkgs/nixos/modules/services/games/cuberite.nix
  ];

  nixpkgs.overlays = [
    (self: super: {
      cuberite = super.callPackage /home/infinisil/src/nixpkgs/pkgs/games/cuberite { };
    })
  ];

  services.cuberite = {
    enable = true;
    openFirewall = true;
    webadmin = {
      enable = true;
      port = 8091;
      users.foo = "foo";
    };
  };
}
