{ pkgs }: pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "nixoses";
  rev = "447eb484bf6966ae09416a875f618a6dc3ee81df";
  sha256 = "0k0gjgvna3s87lg20jyjip5npqgj72gsidy51cbjqxhj5viiv34g";
  fetchSubmodules = true;
}
