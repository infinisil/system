let
  keys.mba.nixos = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7zf2O8yBXxh2tX9v/3ZztXtYeV4W9vTY2iSrm92HSErjz5KcIY/AAKaqbWXHZgsZk2pehBqNbQMOwn0WWdLvil2+Ah97cvl7d9b9XdCkfOPhNB6FKcTzPmMp5Rivi/IodVMhT2xO9S1zO0Y2Q7dsYgk5leKyiD10pkcw23p6MPMKhKV2DPgY6BiszrTEVmtyOHpGkji9rE1iB9MyOINY9eC4etmnNINXMlwttV0GjbJI9WXXEQN2mRaPPp1PBWaPOgoP3ufKi9MR1hEhAantyrfBm2SeqjUvXG5JN1RyooohIWIHWXNJlYFldFPsCD/C1HnE5ylJeLBbZEw0TPb6x infinisil@NixOS";
  keys.mba.macos = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsMBRhTAKrXSL3rkZ4//WpG8cOOUTMLB7yrWdEnfYuzNf7vm0+cDh7GIjCRrq2dIA6I4b+oWowI0zD4l5SjJyuYq7lr0W6MudcosUPxJ7ixmV5iKP3io2tvah2Jyw8O8wZ3iPNfUg0u4zvmgKdVlZBqdUBtl1KflOnGh+gDZ1RzlpfzDB4bWUCwiRCPzMElamMAJjhcIWi8kO9zDEjto8MMFtHJjhuEXrOavoo/51bgFustWWUBN7mQH4eezL6RyzLAJ5a1dRg5sm9wJIMFyrERJVis8wdbVXwAI+MPIQvE1EmhX5k2UlBSUj08HC/oNCEeJPtdh/4nBLLGsKQtkkj";
  keys.eth = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDprwBEjAuaYpVyXAy5BEe+OK0xBDtvJuUd1LYbJq6ji9QbXArbG9ch7UbK5TAxQ07149NRo0UpmsV4/IlrlD9+g+Lo3Q12pDlRLGkDNLMKfWr5skLeLtfZztUQZXKX9CHjfUsmksENEQPu7DCHifzDxO4jmMSLZ74OiW3yMUyc+hHCweIWSGpKDUUjkMxhn4CaUHnvtPAEivFZq5QpGCzKKaKjONPkiX/d1Bq7ZCCn5rmvdAh/zrJn3vjsO9al+NYXAlWlHOHZrc722TGHZVZ4zRn2p3DgLag0CJFfk1cT5tBCh46k0M8bKWHcDj0E+KL+oLQx7jOHgBIjy5L1N603 msilvan@slab1vrt.ethz.ch";

  keys.all = [ keys.mba.nixos keys.mba.macos keys.eth ];
in { pkgs, ... }: {
  imports = [
    hardware/digitalocean.nix
    ./networking.nix # generated at runtime by nixos-infect
  ];

  boot.cleanTmpDir = true;
  networking.hostId = 
  networking.hostName = "dobby";
  networking.firewall.allowPing = true;
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
  };
  
  users.extraUsers.infinisil = {
    isNormalUser = true;
    home = "/home/infinisil";
    description = "Silvan Mosberger";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = keys.all;
  };

  environment.systemPackages = with pkgs; [
    git
    fortune
    neovim
  ];

  users.extraUsers.git = {
    isNormalUser = true;
    home = "/git";
    description = "User for git";
    openssh.authorizedKeys.keys = keys.all;
  };

  programs.zsh.enable = true;

  users.defaultUserShell = pkgs.zsh;

  services.fail2ban.enable = true;
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  services.nginx = {
    enable = true;
    virtualHosts."infinisil.io" = {
      forceSSL = true;
      enableACME = true; # Automatically refreshes certificate with Let's Encrypt
      root = "/webroot";
    };
  };

  services.firefox.syncserver = {
    #enable = true;
    listen.address = "0.0.0.0";
  };


  services.nginx.appendConfig = ''
    error_log /var/log/nginx/error.log debug;
  '';
  
  # https://www.williamjbowman.com/blog/2015/07/24/setting-up-webdav-caldav-and-carddav-servers/
  #services.nginx.virtualHosts."dav.infinisil.io" = {
  #  enableACME = true;
  #  root = "/webroot/radicale";
  #  forceSSL = true;
  #  locations."/" = {
  #    proxyPass = "http://127.0.0.1:5234";
  #  };
  #};

  #services.radicale.enable = true;
  services.radicale.config = ''
[server]
hosts = 0.0.0.0:5234, [::]:5234
pid = "/run/radicale.pid"
base_prefix = /
ssl = False

[encoding]
request = utf-8
stock = utf-8

[auth]
type = None

[rights]
type = None

[storage]
filesystem_folder = "/webroot/radicale/collections"

[logging]
config = /etc/radicale/logging
debug = True
full_environment = False
  '';
}
