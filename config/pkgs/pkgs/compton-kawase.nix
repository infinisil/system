{ lib, fetchFromGitHub }:

{ nvidia ? false }:

let
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "73b135e6b831166dffdefd5bed299c54883b552a";
    sha256 = "0kniph5rnyhqqdhlrffsrqys4s49yp0jikifcms5kdkmz08ggfks";
  };

  pkgs = import nixpkgs { config.allowUnfree = true; };

  # From https://github.com/guibou/nixGL
  nvidiaVersion = "390.77";
  nvidiaLibs = (pkgs.linuxPackages.nvidia_x11.override {
    libsOnly = true;
    kernel = null;
  }).overrideAttrs(oldAttrs: rec {
    name = "nvidia-${nvidiaVersion}";
    src = pkgs.fetchurl {
      url = "http://download.nvidia.com/XFree86/Linux-x86_64/${nvidiaVersion}/NVIDIA-Linux-x86_64-${nvidiaVersion}.run";
      sha256 = "10kjccrkdn360035lh985cadhwy6lk9xrw3wlmww2wqfaa25f775";
    };
    useGLVND = 0;
  });

  compton-kawase = pkgs.compton-git.overrideAttrs (old: {

    src = (import ../../sources).compton-kawase;

    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.makeWrapper ];

    hardeningDisable = [ "format" ];

    postInstall = if nvidia then ''
      wrapProgram $out/bin/compton \
        --set LD_LIBRARY_PATH "${nvidiaLibs}/lib"
    '' else ''
      wrapProgram $out/bin/compton \
        --set LIBGL_DRIVERS_PATH "${pkgs.mesa_drivers}/lib/dri"
    '';
  });

in compton-kawase
