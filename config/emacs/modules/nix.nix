{ lib, config, pkgs, epkgs, dag, ... }:

with lib;

let

  schemas = pkgs.writeText "schemas.xml" ''
    <locatingRules xmlns="http://thaiopensource.com/ns/locating-rules/1.0">
      <documentElement localName="section" typeId="DocBook"/>
      <documentElement localName="chapter" typeId="DocBook"/>
      <documentElement localName="article" typeId="DocBook"/>
      <documentElement localName="book" typeId="DocBook"/>
      <typeId id="DocBook" uri="${pkgs.docbook5}/xml/rng/docbook/docbookxi.rnc" />
    </locatingRules>
  '';

  nix-docbook-mode = epkgs.trivialBuild {
    pname = "nix-docbook-mode";
    version = "1970-01-01";
    src = pkgs.writeText "default.el" ''
      (eval-after-load 'rng-loc
        '(add-to-list 'rng-schema-locating-files "${schemas}"))
    '';
  };

in

{

  options.nix = mkOption {
    type = types.bool;
    default = true;
    description = "Nix emacs stuff";
  };

  config = mkIf config.nix {

    packages = [
      nix-docbook-mode
    ] ++ (with epkgs; [
      nix-mode
      company-nixos-options
      nixos-options
      flycheck
    ]);

    init = {
      xml = ''
        (setq flycheck-xml-xmlstarlet-executable "${pkgs.xmlstarlet}/bin/xml")
      '';

      nixflycheck = ''
        (require 'flycheck)
        (add-to-list 'company-backends 'company-nixos-options)
      '';
    };
  };
}
