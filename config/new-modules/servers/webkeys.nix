{ config, pkgs, lib, ... }:

let

  attrToDerivation = { name, value, strings ? true }: let
    # Maps an attrset/path to a build script that recursively creates the equivalent directory/file structure
    valToPath = name: value: if lib.isAttrs value then ''
      mkdir ${name} && pushd ${name}
      ${lib.concatMapStringsSep "\n" (name: valToPath (lib.escapeShellArg name) value.${name}) (lib.attrNames value)}
      popd
    '' else ''
      ln -s ${lib.escapeShellArg (if strings then pkgs.writeText "file" value else value)} ${name}
    '';
  in pkgs.runCommand name {
    preferLocalBuild = true;
  } (valToPath "$out" value);

in

{

  options.mine.web.keys.enable = lib.mkEnableOption "Online SSH keys";

  config = lib.mkIf config.mine.web.keys.enable {
    mine.subdomains = [ "keys" ];

    services.nginx = {
      enable = true;
      virtualHosts."keys.${config.networking.domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webserver"; # Needed for ACME
        locations."/" = {
          root = attrToDerivation {
            name = "keys";
            value = config.mine.sshkeys;
          };
          extraConfig = "autoindex on;";
        };
      };
    };
  };

}
