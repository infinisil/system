{ pkgs }: pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "nixoses";
  rev = "640fa9727e6741a31e2ca23b28e8c95809f52198";
  sha256 = "0xdpzwmqxynpwwqc96549j3q3pmh64h1ibrxmhlwd7qxd8fqxbmn";
  fetchSubmodules = true;
}
