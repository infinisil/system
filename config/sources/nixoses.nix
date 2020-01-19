{ pkgs }: pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "nixoses";
  rev = "44d502abeef314379320fa6f65a08956a03444d5";
  sha256 = "07mvfxsnb400si36hq3kd9rvvcd0296s6rlq8ni6gk7gxgbs067g";
  fetchSubmodules = true;
}
