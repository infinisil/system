{ pkgs }:

let

  src = builtins.fetchTarball {
    url = "https://github.com/Infinisil/hashsearch/archive/6670ecabffc6e7cfe030b92c7f59eb1c9819c922.tar.gz";
    sha256 = "1kfn38kmjxg3scs2ba1vw3gzik25k8ai4vgzdxj5kyp7ihrm3iiv";
  };

  hashsearch = import src { };

in hashsearch.hashsearch
