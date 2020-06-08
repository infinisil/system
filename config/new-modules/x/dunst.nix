{ config, lib, pkgs, ... }:

with lib;

{

  options.mine.dunst.enable = mkEnableOption "dunst config";

  config.mine.xUserConfig = mkIf config.mine.dunst.enable {

    services.dunst = {
      enable = true;
      settings = {
        global = {
          monitor = 0;
          follow = "mouse";
          geometry = "300x5-30+20";
          indicate_hidden = true;
          shrink = false;
          transparency = 20;
          notification_height = 0;
          separator_height = 2;
          padding = 8;
          horizontal_padding = 8;
          frame_width = 3;
          frame_color = "#aaaaaa";
          separator_color = "frame";
          sort = true;
          idle_threshold = 120;
          font = "Helvetica Neue LT Std,HelveticaNeueLT Std Lt Cn:style=47 Light Condensed,Regular";
          line_height = 0;
          markup = "full";
          format = "<b>%s</b>\\n%b";
          alignment = "center";
          show_age_threshold = 60;
          word_wrap = true;
          ignore_newline = false;
          stack_duplicates = true;
          hide_duplicate_count = false;
          show_indicators = true;
          icon_position = "left";
          max_icon_size = 75;
          sticky_history = true;
          history_length = 20;
          dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst";
          browser = "${config.mine.firefox}/bin/firefox --new-tab";
          always_run_script = true;
          title = "Dunst";
          class = "Dunst";
          startup_notification = false;
          force_xinerama = false;
        };
        experimental = {
          per_monitor_dpi = false;
        };
        shortcuts = {
          close = "ctrl+space";
          close_all = "ctrl+shift+space";
          history = "ctrl+grave";
          context = "ctrl+shift+period";
        };
        urgency_low = {
          background = "#222222";
          foreground = "#888888";
          timeout = 10;
        };
        urgency_normal = {
          background = "#285577";
          foreground = "#ffffff";
          timeout = 10;
        };
        urgency_critical = {
          background = "#900000";
          foreground = "#ffffff";
          frame_color = "#ff0000";
          timeout = 10;
        };
      };
    };
  };
}
