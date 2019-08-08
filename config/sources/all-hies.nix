{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "353b7084d6ffad67e7c3caafb9eb3a57c09088c0";
  sha256 = "0gixc4fysbwmmsi3hm1zdyda8c0inkkzgwwzx81synqn7rmbhywq";
}; in src // {
  meta = src.meta // {
    branch = "0.12.0.0";
  };
}
