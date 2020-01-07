{ pkgs }: pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "nixoses";
  rev = "0e0c86a3adf2d3604000c326e9c025f6f2716a0a";
  sha256 = "1k7q1bn79wwpnxapqs3z9zgzqgz8i15yq8k6r9i2scg10bdpac32";
  fetchSubmodules = true;
}
