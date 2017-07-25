{ pkgs, config, ... }:

let
  davnginx = pkgs.nginxMainline.overrideAttrs (old: {
    modules = [
      (pkgs.fetchFromGitHub {
        owner = "arut";
        repo = "nginx-dav-ext-module";
        rev = "v0.0.3";
        sha256 = "1qck8jclxddncjad8yv911s9z7lrd58bp96jf13m0iqk54xghx91";
      })
    ];
  });
in {
  networking.firewall.allowedTCPPorts = [ 5232 ];
  networking.firewall.allowedUDPPorts = [ 5232 ];
  services.nginx = {
    enable = true;
    #package = davnginx;
    virtualHosts."dav.infinisil.io" = {
      #enableACME = true;
      #forceSSL = true;
      root = "/webroot/dav";
      locations."/" = {
        extraConfig = ''
          dav_methods PUT DELETE MKCOL COPY MOVE;
          dav_ext_methods PROPFIND OPTIONS;
          dav_access user:rw group:r;

          autoindex on;
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
      htpasswd_encryption = bcrypt

      [server]
      hosts = 0.0.0.0:5232

      [storage]
      filesystem_folder = /var/radicale/collections
    '';
  };
}
