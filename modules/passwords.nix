{ lib, ... }:

with lib;

{
  options.passwords = mkOption {
    type = types.attrs;
    description = "Passwords";
  };

  config = {
    passwords = import ../private/passwords.nix;
  };
}
