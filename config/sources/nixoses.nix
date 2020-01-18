{ pkgs }: pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "nixoses";
  rev = "1388bde04a5b37a199f64969c43f260e23fa557e";
  sha256 = "0q4ndg4822h4adaqwpny11jpslyi8nkybcym0sid65bbsza2hf3m";
  fetchSubmodules = true;
}
