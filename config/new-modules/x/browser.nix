{ lib, config, pkgs, sources, ... }:
with lib;
let
  extensions = (import sources.nur { pkgs = pkgs; nurpkgs = pkgs; }).repos.rycee.firefox-addons;
in
{

  options.mine.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf config.mine.firefox.enable {
    mine.userConfig = {
      programs.firefox = {
        enable = true;
        package = pkgs.firefox.override {
          cfg.enableBrowserpass = true;
        };
        #extensions = with extensions; [
        #  tree-style-tab
        #  browserpass
        #];
        # profiles.default = {
        #   settings = {
        #     # Always restore tabs and windows when starting
        #     "browser.startup.page" = 3;
        #     # Dark theme
        #     "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
        #     "browser.theme.content-theme" = 0;
        #     "browser.theme.toolbar-theme" = 0;
        #     # Don't confirm when quitting
        #     "browser.warnOnQuitShortcut" = false;
        #   };

        #   userChrome = ''
        #     #main-window[tabsintitlebar="true"]:not([extradragspace="true"]) #TabsToolbar > .toolbar-items {
        #       opacity: 0;
        #       pointer-events: none;
        #     }
        #     #main-window:not([tabsintitlebar="true"]) #TabsToolbar {
        #         visibility: collapse !important;
        #     }
        #   '';
        # };
      };
    };
  };

}
