{ lib, config, pkgs, ... }: {

  options.mine.cdSupport = {
    enable = lib.mkEnableOption "cd support";
  };

  config = lib.mkIf config.mine.cdSupport.enable {
    nixpkgs.overlays = [(self: super: {
      mpv-unwrapped = super.mpv-unwrapped.override {
        cddaSupport = true;
      };
    })];

    environment.systemPackages = with pkgs; [
      mpv
    ];
  };
}
