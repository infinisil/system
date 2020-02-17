{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "kolloch";
  repo = "crate2nix";
  rev = "88835e5fb8c8c0b80434859b37d9fb33074a02c9";
  sha256 = "15yibdsf2hb89dhys0sl9m6545q006w3faw6d8hws0cw1xpsr6zk";
}; in src // {
  meta = src.meta // {
    branch = "0.7.1";
  };
}
