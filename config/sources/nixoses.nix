{ pkgs }: pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "nixoses";
  rev = "9bdb477b4d0bc8a220a0b3e8eeb262f8aac1dcee";
  sha256 = "1l2yraah9r2yhlzpz11ncf8jdk884raiz37lhd4123yl82r40ald";
  fetchSubmodules = true;
}
