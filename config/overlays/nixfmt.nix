self: super: {
  nixfmt = (import self.sources.nixfmt { system = self.system; }).packages.nixfmt;
}
