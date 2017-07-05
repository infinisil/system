{
  network.description = "Web server";

  webserver =
    { config, pkgs, ... }:
    {
      services.httpd = {
        enable = true;
        adminAddr = "infinisil@icloud.com";
        documentRoot = "${pkgs.valgrind.doc}/share/doc/valgrind/html";
      };
      networking.firewall.allowedTCPPorts = [ 80 ];
    };
}
