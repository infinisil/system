{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "b3b61ab1a95d32284d3f48faef580af5ed664094";
  sha256 = "1vqipl2ys2mycj2s2f44n18sahj6g43j2w3w24kmryxih9sh25dl";
}; in src // {
  meta = src.meta // {
    branch = "1.1";
  };
}
