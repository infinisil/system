{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "ce338eea908f189b280cdbfe67b78b0cac7d8b59";
  sha256 = "19spg5xnb1gdnxal4vp402dknfhbva5jj5yq34qyzvksyn16c3dp";
}; in src // {
  meta = src.meta // {
    branch = "0.13.0.0";
  };
}
