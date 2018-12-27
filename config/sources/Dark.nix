{ pkgs, gitlabtoken }: pkgs.fetchgit {
  url = "https://token:${gitlabtoken}@gitlab.com/sixsixfive/DarK.git/";
  rev = "c3c1970570571d1852b11308693dfc79c134c2f6";
  sha256 = "0ijsf688la9jild35jibhadarvywp5xnypf9hgycyk1f7xx6w03q";
} // {
  meta.homepage = "https://gitlab.com/sixsixfive/DarK.git/";
}
