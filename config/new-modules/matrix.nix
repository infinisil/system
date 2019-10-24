{ pkgs, config, lib, ... }: {

  options.mine.matrix.enable = lib.mkEnableOption "matrix config";

  config = lib.mkIf config.mine.matrix.enable {

  };

}
