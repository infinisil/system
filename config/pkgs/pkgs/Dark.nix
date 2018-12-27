{ stdenv, fetchFromGitLab }:

stdenv.mkDerivation {
  name = "Dark";
  src = (import ../../sources).Dark;
  makeFlags = [ "PREFIX=$(out)" ];
}

