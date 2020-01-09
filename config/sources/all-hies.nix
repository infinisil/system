{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "4b984030c8080d944372354a7b558c49858057e7";
  sha256 = "0109l0ls7gh2gydrgx45168lg45kgmgwaw9jpjm8mwvwd1brssfk";
}; in src // {
  meta = src.meta // {
    branch = "master";
  };
}
