{ stdenv, fetchFromGitLab }:

stdenv.mkDerivation {
  name = "Dark";
  src = fetchFromGitLab {
    owner = "sixsixfive";
    repo = "DarK";
    rev = "1a1c2760bfe827c9d8526bccd582ca4b4066491e";
    sha256 = "00qjq7jvx6mkzgscqwb42xm4cv1yaryl7ig1izr9gsd58h5ma0qz";
  };

  makeFlags = [ "PREFIX=$(out)" ];
}

