{ stdenv, fetchFromGitLab, sources }:

stdenv.mkDerivation {
  name = "Dark";
  src = sources.DarK;
  makeFlags = [ "PREFIX=$(out)" ];
}

