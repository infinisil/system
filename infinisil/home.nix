with (import <nixpkgs> {}).lib;

{ pkgs, ... }:

let
  musicDir = "/home/infinisil/Music";
in

{
  manual.manpages.enable = true;



  home.packages = with pkgs; [
    fortune
    cmatrix
  ];

  programs.ssh = {
    enable = true;
    controlMaster = "yes";
    matchBlocks = [
      {
        host = "inf";
        hostname = "infinisil.io";
      }
      {
        host = "git";
        hostname = "infinisil.io";
        user = "git";
      }
    ];
  };

  
  programs.beets.settings = {
    directory = "${musicDir}/data";
    library = "${musicDir}/beets.db";
    plugins = [ "smartplaylist" "mpdupdate" ];

    mpd = {
      host = "127.0.0.1";
      port = "6600";
    };

    smartplaylist = let
      playlists = {
        all = "";
        spacegirl = "Space";
        five = "5";
      }; in {
      playlist_dir = "${musicDir}/mpd/playlists";
      relative_to = "${musicDir}/data";
      playlists = mapAttrsToList (name: query: {
        name = "${name}.m3u";
        inherit query;
      }) playlists;
    };
  };

  programs.git = {
    enable = true;
    userName = "Silvan Mosberger";
    userEmail = "infinisil@icloud.com";
    aliases = {
      lg = "lg = log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
    };
    signing = {
      key = "5B2CFBD8E6AD7FC113D675A89424360B4B85C9E7";
      signByDefault = true;
    };
  };

  programs.browserpass = {
    enable = true;
    browsers = [ "firefox" ];
  };

  programs.htop = {
    enable = true;
    fields = [
      "PID" "USER" "PRIORITY"
      "PERCENT_CPU" "M_RESIDENT" "PERCENT_MEM"
      "TIME" "COMM"
    ];
    colorScheme = 6;
    hideThreads = true;
    hideUserlandThreads = true;
    showProgramPath = false;
    highlightBaseName = true;
    meters = {
      left = [
        "Memory"
        "CPU"
        "LeftCPUs2"
        "RightCPUs2"
        { kind = "CPU"; mode = 3; }
      ];
      right = [
        { kind = "Clock"; mode = 4; }
        "Uptime"
        "Tasks"
        "LoadAverage"
        { kind = "Battery"; mode = 1; }
      ];
    };
  };

  services.random-background = {
    enable = true;
    imageDirectory = "%h/Pics";
    interval = "10min";
  };
}


