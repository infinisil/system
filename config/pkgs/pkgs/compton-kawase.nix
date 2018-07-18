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
  nvidiaVersion = "390.67";
  nvidiaLibs = (pkgs.linuxPackages.nvidia_x11.override {
    libsOnly = true;
    kernel = null;
  }).overrideAttrs(oldAttrs: rec {
    name = "nvidia-${nvidiaVersion}";
    src = pkgs.fetchurl {
      url = "http://download.nvidia.com/XFree86/Linux-x86_64/${nvidiaVersion}/NVIDIA-Linux-x86_64-${nvidiaVersion}.run";
      sha256 = "0np6xj93fali2hss8xsdlmy5ykjgn4hx6mzjr8dpbdi0fhdcmwkd";
    };
    useGLVND = 0;
  });

  compton-kawase = pkgs.compton-git.overrideAttrs (old: {

    src = fetchFromGitHub {
      owner  = "tryone144";
      repo   = "compton";
      rev    = "241bbc50285e58cbc6a25d45066689eeea913880";
      sha256 = "148s7rkgh5aafzqdvag12fz9nm3fxw2kqwa8vimgq5af0c6ndqh2";
    };

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
