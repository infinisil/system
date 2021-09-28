self: super: {
  zrepl = super.zrepl.overrideAttrs (old: {
    patches = old.patches or [] ++ [
      (self.fetchpatch {
        url = "https://github.com/Infinisil/zrepl/commit/d82b5df6b17bb62ae1628de258d4b9ef1d7ba18f.patch";
        sha256 = "15h2sjmn798i2vxa2jsk59gfkzm9k7q9jhkk5j3xjrh2hw45xfym";
      })
      (self.fetchpatch {
        url = "https://github.com/Infinisil/zrepl/commit/c29f72030f4b4cdac22a10bc91137f0d1aec7b60.patch";
        sha256 = "1l3s78v6iz1jk7wjrjs6dy97nbml03x3hqvsgcirmy314mc9rcfa";
      })
    ];
  });
}
