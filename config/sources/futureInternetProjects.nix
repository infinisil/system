{ pkgs, ethzgitlabtoken }: (pkgs.fetchgit {
  url = "https://token:${ethzgitlabtoken}@gitlab.inf.ethz.ch/OU-SINGLA/fi2019_grp6.git/";
  rev = "a9e70e2b42befabfd24ce4bead9b36950307289f";
  sha256 = "04129qw2v77b6qvvidd4d86irk6k9llmqpzrihvkq5x6c3j1l12f";
}) // {
  meta.homepage = "https://gitlab.inf.ethz.ch/OU-SINGLA/fi2019_grp6.git/";
}
