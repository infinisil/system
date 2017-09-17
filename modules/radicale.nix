{ pkgs, config, ... }:
{
  environment.systemPackages = with pkgs; [
    apacheHttpd
  ];
  
  networking.firewall.allowedTCPPorts = [ 5232 ];
  networking.firewall.allowedUDPPorts = [ 5232 ];

  services.nginx = {
    enable = true;
    virtualHosts."dav.infinisil.io" = {
      enableACME = true;
      forceSSL = true;
      root = "/webroot/dav/";
      locations."/" = {
        proxyPass = "http://localhost:5232/";
        extraConfig = ''
          proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_pass_header Authorization;
        '';
      };
    };
  };

  services.radicale = {
    enable = true;
    config = ''
      [auth]
      type = htpasswd
      htpasswd_filename = /var/lib/radicale/users
      htpasswd_encryption = md5

      [server]

      [storage]
      filesystem_folder = /var/lib/radicale/collections

      [logging]
      debug = True
    '';
    # Hangs radicale:
    # storage.hook = ${pkgs.git}/bin/git add -A && (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m 'Changes by infinisil')
  };
}
