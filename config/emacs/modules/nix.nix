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
      xml = dag.entryAfter [ "pkgs" ] ''
        (setq flycheck-xml-xmlstarlet-executable "${pkgs.xmlstarlet}/bin/xml")
      '';

      nixflycheck = dag.entryAfter [ "pkgs" ] ''

        (require 'flycheck)
        ; Combination of flychecks nix checker and rebar3 checker's color stripping
        ; Because there is no way to prevent nix-instantiate from outputting colors
        (flycheck-define-checker mynix
          "Nix checker using nix-instantiate.

        See URL `https://nixos.org/nix/manual/#sec-nix-instantiate'."
          :command ("nix-instantiate" "--parse" "-")
          :standard-input t
          :error-patterns
          ((error line-start
                  "error: " (message) " at " (file-name) ":" line ":" column
                  line-end))
          :error-parser
          (lambda (output checker buffer)
            (require 'ansi-color)
            (flycheck-parse-with-patterns
             (and (fboundp 'ansi-color-filter-apply) (ansi-color-filter-apply output))
             checker buffer))
          :error-filter
          (lambda (errors)
            (flycheck-sanitize-errors
             (flycheck-remove-error-file-names "(string)" errors)))
          :modes nix-mode)

        (add-to-list 'flycheck-checkers 'mynix)
      '';
    };
  };
}
