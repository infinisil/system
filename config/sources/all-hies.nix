{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "971421b6c4f8fbd9f05154581489ea7eebe10de5";
  sha256 = "1zl3zrswic7d3z1hmpjc76zp569bb655jrlkamlgxxr53n0j6n47";
}; in src // {
  #meta = src.meta // {
  #  branch = "0.12.0.0";
  #};
}
