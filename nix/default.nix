with import <nixpkgs> {};
with lib;
let
  config = import ./config.nix;
  cfg = config // { hosts = listify "alias" config.hosts; };
  listify = name: items:
    map (a: mapAttrs (n: x: toString x) (getAttr a items) // { "${name}" = a; }) (attrNames items);
  file = (import .ssh/config.nix) cfg;
  irssi = (import irssi/config.nix) cfg;
in
writeText "config" "${irssi}"

