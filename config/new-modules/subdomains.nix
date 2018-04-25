{ lib, ... }:

with lib;

{

  options.mine.subdomains = mkOption {
    type = types.listOf types.str;
    default = [];
    description = ''
      List of subdomains to map to <literal>networking.domain</literal>
    '';
  };

}
