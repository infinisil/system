{ pkgs, ethzgitlabtoken }: (pkgs.fetchgit {
  url = "https://token:${ethzgitlabtoken}@gitlab.inf.ethz.ch/OU-SINGLA/fi2019_grp6.git/";
  rev = "e4980714ba1fd1848538008cd46b8fc4e2c39c01";
  sha256 = "0n4p59k0g08kwl2p9m89fps1vkpyiqldpkc6kjxri87mhbdnd4fw";
}) // {
  meta.homepage = "https://gitlab.inf.ethz.ch/OU-SINGLA/fi2019_grp6.git/";
}
