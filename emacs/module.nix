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

  epkgs = pkgs.emacsPackagesNgGen (if config.usePretest
      then pkgs.emacs.overrideAttrs (old: {
        name = "emacs-pretest-26.1";
        src = pkgs.fetchurl {
          url = "ftp://alpha.gnu.org/gnu/emacs/pretest/emacs-26.1-rc1.tar.xz";
          sha256 = "0n2pl1i4piga43p1kbscbb2sgg74gy4qq5jgmmrnxf80vrlfd535";
        };
        patches = [];
      })
      else pkgs.emacs
    );

  emacs = epkgs.emacsWithPackages (_: config.packages);

in

{

  imports = [
    ./base.nix
    ./evil.nix
    ./haskell.nix
  ];

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
  };

  config = {

    _module.args = {
      inherit dag epkgs;
    };

    inherit initFile emacs;

    init.pkgs = dag.entryAnywhere ''
      (package-initialize)
    '';

  };
}
