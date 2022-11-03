{ lib, config, pkgs, ... }: {

  options.mine.diskSupport = {
    enable = lib.mkEnableOption "disk support";
  };

  config = lib.mkIf config.mine.diskSupport.enable {
    nixpkgs.overlays = [(self: super: {
      mpv-unwrapped = super.mpv-unwrapped.override {
        # Support for playing CDs with `mpv cdda:///dev/sr0`
        cddaSupport = true;
      };
    })];

    # With makemkv, support for decoding blurays with `mpv bd:////dev/sr0`
    environment.sessionVariables = {
      LIBBDPLUS_PATH = "/run/current-system/sw/lib/libmmbd";
      LIBAACS_PATH = "/run/current-system/sw/lib/libmmbd";
      MAKEMKVCON = "/run/current-system/sw/bin/makemkvcon";
      MMBD_TRACE = toString 1;
    };

    environment.systemPackages = with pkgs; [
      makemkv
      mpv
      vlc
    ];
  };
}
