{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "155362dcbe8baf7934b72a8f25d12072504d3c62";
  sha256 = "1qaz9axrm82g3j8g620w2bb3675ph3rgrw83s2y6w7jbmi1bvby2";
}; in src // {
  meta = src.meta // {
    branch = "0.14.0.0";
  };
}
