{ pkgs, ethzgitlabtoken }: (pkgs.fetchgit {
  url = "https://token:${ethzgitlabtoken}@gitlab.inf.ethz.ch/OU-SINGLA/fi2019_grp6.git/";
  rev = "544d61fc96fe70745d3f7b1f8c4406930458a260";
  sha256 = "1cjjs7pdfb7j46lr8i7krx3id9yn0hyi7wi81b042gkc30pchq6k";
}) // {
  meta.homepage = "https://token:${ethzgitlabtoken}@gitlab.inf.ethz.ch/OU-SINGLA/fi2019_grp6.git/";
}
