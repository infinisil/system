{ pkgs }: pkgs.fetchFromGitHub {
  owner = "sorin-ionescu";
  repo = "prezto";
  rev = "300102897a4710e1559e4435c686f794d126d3c3";
  sha256 = "00f53kx72sbng1k6rdicmz3j04zfgmydaxixm8wp4pq7apffcb34";
  fetchSubmodules = true;
}
