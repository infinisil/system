{ pkgs }:

let

  src = builtins.fetchTarball {
    url = "https://github.com/Infinisil/hashsearch/archive/b2669abfa0ac52e28810a2c0c10312222c668c67.tar.gz";
    sha256 = "11hihyljlvfxgrilfnf7ldvjrbw82rnvq5mpwgqwjvxhhv6x3adf";
  };

  hashsearch = import src { };

in hashsearch
