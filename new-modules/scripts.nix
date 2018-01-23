{ lib, config, pkgs, ... }:

with lib;

let

  scripts = builtins.attrValues (mapAttrs (name: path: {
    inherit name path;
  }) config.scripts);

  scriptDrv = pkgs.linkFarm "scripts" scripts;

in

{

  options.scripts = mkOption {
    type = types.attrsOf types.str;
    default = {};
    apply = mapAttrs (name: script: pkgs.writeScript name ''
      #!${pkgs.stdenv.shell}
      ${script}
    '');
    example = literalExample ''
      hello = '''
        #!''${pkgs.bash}/bin/bash
        ''${pkgs.hello}/bin/hello
      ''';
    '';
    description = ''
      Scripts to be written to /run/current-system/<name>
    '';
  };

  config = {

    system.extraSystemBuilderCmds = ''
      ln -s ${scriptDrv} $out/scripts
    '';

  };

}
