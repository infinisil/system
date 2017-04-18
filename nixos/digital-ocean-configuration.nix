let
  keys.mba.nixos = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7zf2O8yBXxh2tX9v/3ZztXtYeV4W9vTY2iSrm92HSErjz5KcIY/AAKaqbWXHZgsZk2pehBqNbQMOwn0WWdLvil2+Ah97cvl7d9b9XdCkfOPhNB6FKcTzPmMp5Rivi/IodVMhT2xO9S1zO0Y2Q7dsYgk5leKyiD10pkcw23p6MPMKhKV2DPgY6BiszrTEVmtyOHpGkji9rE1iB9MyOINY9eC4etmnNINXMlwttV0GjbJI9WXXEQN2mRaPPp1PBWaPOgoP3ufKi9MR1hEhAantyrfBm2SeqjUvXG5JN1RyooohIWIHWXNJlYFldFPsCD/C1HnE5ylJeLBbZEw0TPb6x infinisil@NixOS";
  keys.mba.macos = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsMBRhTAKrXSL3rkZ4//WpG8cOOUTMLB7yrWdEnfYuzNf7vm0+cDh7GIjCRrq2dIA6I4b+oWowI0zD4l5SjJyuYq7lr0W6MudcosUPxJ7ixmV5iKP3io2tvah2Jyw8O8wZ3iPNfUg0u4zvmgKdVlZBqdUBtl1KflOnGh+gDZ1RzlpfzDB4bWUCwiRCPzMElamMAJjhcIWi8kO9zDEjto8MMFtHJjhuEXrOavoo/51bgFustWWUBN7mQH4eezL6RyzLAJ5a1dRg5sm9wJIMFyrERJVis8wdbVXwAI+MPIQvE1EmhX5k2UlBSUj08HC/oNCEeJPtdh/4nBLLGsKQtkkj";

  

in { pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect
  ];

  boot.cleanTmpDir = true;
  networking.hostName = "main";
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [ keys.mba.nixos keys.mba.macos ];
  
  environment.systemPackages = with pkgs; [
    git
  ];

  users.extraUsers.git = {
    isNormalUser = true;
    home = "/git";
    description = "User for git";
    openssh.authorizedKeys.keys = [ keys.mba.nixos keys.mba.macos ];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  services.nginx = {
    enable = true;
    virtualHosts."infinisil.io" = {
      forceSSL = true;
      enableACME = true; # Automatically refreshes certificate with Let's Encrypt
      root = "/webroot";
    };
  };

  
  # Radicale CalDAV and CardDAV configuration, enable when dav.infinisil.io can be looked up. Thanks to https://www.williamjbowman.com/blog/2015/07/24/setting-up-webdav-caldav-and-carddav-servers/
  #services.nginx.virtualHosts."dav.infinisil.io" = {
  #  enableACME = true;
  #  proxyPass = "http://127.0.0.1:5232";
  #};
  #services.radicale.enable = true
  #services.radicale.config = ''
  #  [server]
  #  hosts = 0.0.0.0:5232, [::]:5232
  #  pid = "/run/radicale.pid"
  #
  #  [storage]
  #  filesystem_folder = "/webroot/radicale/collections"
  #''
}
