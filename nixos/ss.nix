{ pkgs, config, ... }:
{
  services.firefox.syncserver = {
    enable = true;
  };
}
