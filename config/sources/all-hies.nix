{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "4d52f70a28b337a70d1c92f5a4483ebcd7612e03";
  sha256 = "0g7bmzx2ifij87j32kvbmy1gxcyqiawwafra6l027n70kx5hbfva";
}; in src // {
  meta = src.meta // {
    branch = "haskell.nix";
  };
}
