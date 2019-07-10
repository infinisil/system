{ pkgs }: let src = pkgs.fetchFromGitHub {
  owner = "infinisil";
  repo = "all-hies";
  rev = "d8add468a7df53bf60c22a28b873fcb00ef9a438";
  sha256 = "18g55rrmbxcwi1njc1afzqp1pbwlw4ljrq8d8nc9abkqd6f2sgav";
}; in src // {
  meta = src.meta // {
    branch = "0.11.0.0";
  };
}
