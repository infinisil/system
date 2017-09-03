{ pkgs, config, ... }:
{
  networking.firewall.allowedTCPPorts = [ 5232 ];
  networking.firewall.allowedUDPPorts = [ 5232 ];

  services.nginx = {
    enable = true;
    virtualHosts."dav.infinisil.io" = {
      #enableACME = true;
      #forceSSL = true;
      root = "/webroot/dav/";
      locations."/" = {
        proxyPass = "http://localhost:5232/";
        extraConfig = ''
          proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_pass_header Authorization;
          auth_basic "Radicale - Password Required";
          auth_basic_user_file /var/radicale/users;
        '';
      };
    };
  };

  services.radicale = {
    enable = true;
    config = ''
      [auth]
      type = htpasswd
      htpasswd_filename = /var/radicale/users
      htpasswd_encryption = md5

      [server]

      [storage]
      filesystem_folder = /var/radicale/collections

      [logging]
      debug = True
    '';
    # Hangs radicale:
    # storage.hook = ${pkgs.git}/bin/git add -A && (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m 'Changes by infinisil')
  };
}
