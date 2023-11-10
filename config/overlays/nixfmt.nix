self: super: {
  nixfmt = (import self.sources.nixfmt_118).packages.${self.system}.default;
}
