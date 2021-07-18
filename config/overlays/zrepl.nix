self: super: {
  zrepl = super.zrepl.overrideAttrs (old: {
    patches = old.patches or [] ++ [
      (self.fetchpatch {
        url = "https://github.com/Infinisil/zrepl/commit/d82b5df6b17bb62ae1628de258d4b9ef1d7ba18f.patch";
        sha256 = "15h2sjmn798i2vxa2jsk59gfkzm9k7q9jhkk5j3xjrh2hw45xfym";
      })
    ];
  });
}
