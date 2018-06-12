{ pkgs }:
{ lib, config, dag, ... }:

with lib;

let

  initEntry = types.submodule {
    options = {
      data = mkOption {
        type = types.lines;
        description = "emacs code to execute";
      };

      before = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "To be done before the given targets";
      };

      after = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "To be done after the given targets";
      };
    };
  };

  dag = import ../lib/dag.nix { inherit lib; };

  # TODO: Handle dag cycle error gracefully
  initFile = pkgs.writeText "init.el" (concatMapStringsSep "\n\n" ({ name, data }:
    "; Section ${name}\n${data}"
  ) (dag.dagTopoSort config.init).result);

  overrides = super: self: {
    nix-mode = super.nix-mode.overrideAttrs (old: {
      src = builtins.fetchTarball {
        url = "https://github.com/NixOS/nix-mode/archive/5a8b334c75f94dc22ff3c30e3886b1a198118b8a.tar.gz";
        sha256 = "1dvmhjf4v22jsvv9hwsvadicbhgynjrihkmy6r77phvrrd3sh63p";
      };
    });
  };

  epkgs = (pkgs.emacsPackagesNgGen (if config.usePretest
      then pkgs.emacs.overrideAttrs (old: {
        name = "emacs-pretest-26.1";
        src = pkgs.fetchurl {
          url = "ftp://alpha.gnu.org/gnu/emacs/pretest/emacs-26.1-rc1.tar.xz";
          sha256 = "0n2pl1i4piga43p1kbscbb2sgg74gy4qq5jgmmrnxf80vrlfd535";
        };
        patches = [];
      })
      else pkgs.emacs
    )).overrideScope overrides;

  emacs = epkgs.emacsWithPackages (_: lib.unique config.packages);

  exec = pkgs.writeScriptBin "myemacs" ''
    #!${pkgs.stdenv.shell}
    exec ${emacs}/bin/emacs -q -l ${initFile}
  '';

in

{

  imports = map (name: "${./modules}/${name}")
    (builtins.attrNames (builtins.readDir ./modules));

  options = {

    usePretest = mkEnableOption "emacs pretest";

    package = mkOption {
      type = types.package;
      default = pkgs.emacs25;
      description = "Emacs package to use";
    };

    packages = mkOption {
      type = types.listOf types.package;
      default = [];
      description = "Emacs packages. Use pkgs.emacsPackagesNg to refer to them";
    };

    init = mkOption {
      type = with types; attrsOf (coercedTo str dag.entryAnywhere initEntry);
      default = dag.empty;
      description = "Init entries";
    };

    initFile = mkOption {
      type = types.path;
      readOnly = true;
      description = "init file";
    };

    emacs = mkOption {
      type = types.package;
      readOnly = true;
      description = "Emacs";
    };

    exec = mkOption {
      type = types.package;
      readOnly = true;
      description = "All-in-one executable";
    };
  };

  config = {

    _module.args = {
      inherit pkgs dag epkgs;
    };

    inherit initFile emacs exec;

    init.pkgs = dag.entryAnywhere ''
      (package-initialize)
    '';

  };
}
