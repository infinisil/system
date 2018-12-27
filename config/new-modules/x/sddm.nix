{ pkgs, lib, config, ... }:

with lib;

let

  pkgs' = import <nixpkgs> {};

  mkTheme = { src, name ? src.name, config ? null, userConfig ? null, extraCommands ? "", overlay ? null}:
    let
      themeConf = pkgs'.writeTextDir "theme.conf" config;
      themeConfUser = pkgs'.writeTextDir "theme.conf.user" userConfig;
    in pkgs'.runCommand name {
      passthru = optionalAttrs (overlay != null) {
        inherit overlay;
      };
    } ''
      mkdir -p $out/share/sddm/themes
      cp -r ${src} $out/share/sddm/themes/${name}
      cd $out/share/sddm/themes/${name}
      chmod +w -R .

      ${optionalString (config != null) "install ${themeConf}/* ."}
      ${optionalString (userConfig != null) "install ${themeConfUser}/* ."}

      ${extraCommands}
    '';

  themes = {
    clairvoyance = mkTheme {
      name = "clairvoyance";
      src = (import ../../sources).sddm-theme-clairvoyance;
      config = ''
        [General]
        background=${config.mine.assets.display-manager}
        autoFocusPassword=true
        enableHDPI=false
      '';
      overlay = self: super: {
        sddm = super.sddm.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ singleton self.qt5.qtquickcontrols;
        });
      };
    };
    aerial = mkTheme {
      name = "aerial";
      src = (import ../../sources).aerial-sddm-theme;
      config = ''
        [General]
        background=background.jpg
        background_day=playlist_day.m3u
        background_night=playlist_night.m3u
        displayFont="DejaVu Sans"
        showLoginButton=true
        passwordLeftMargin=15
        usernameLeftMargin=15
        relativePositionX=0.3
        relativePositionY=0.7
      '';
      #userConfig = ''
      #  [General]
      #  displayFont="Misc Fixed"
      #  passwordLeftMargin=15
      #  relativePositionX=0.5
      #  relativePositionY=0.75
      #  showLoginButton=false
      #  type=color
      #  usernameLeftMargin=15
      #'';
      extraCommands = ''
        rm theme.conf.user
      '';
      overlay = self: super: {
        sddm = super.sddm.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [
            (self.qt5.qtmultimedia.overrideAttrs (old: {
              buildInputs = old.buildInputs ++ [
                self.gst_all_1.gst-libav
                self.gst_all_1.gst-plugins-good
                self.libsForQt5.phonon
                self.libsForQt5.phonon-backend-gstreamer
              ];
            }))
            self.libsForQt5.phonon
            self.libsForQt5.phonon-backend-gstreamer
          ];
        });
      };
    };


  };

  theme = themes.clairvoyance;

in

{

  options.mine.sddm.enable = mkEnableOption "sddm";

  config = mkIf config.mine.sddm.enable {

    nixpkgs.overlays = optional (theme ? overlay) theme.overlay;

    environment.systemPackages = [ theme ];

    environment.pathsToLink = [ "/share/sddm" ];

    services.xserver.displayManager.sddm.theme = theme.name;

  };

}


