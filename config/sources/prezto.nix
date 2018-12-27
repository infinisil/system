{ pkgs }: pkgs.fetchFromGitHub {
  owner = "sorin-ionescu";
  repo = "prezto";
  rev = "a338cba805f63f770e9078925bc5c46129e28bde";
  sha256 = "07xzw9sli4cdb56zgy36n2f5x13kxyxbyh4ags030b64079br5a0";
  fetchSubmodules = true;
}
