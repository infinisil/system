{ lib, ... }:

with lib;

let

  keys = {
    mac = {
      nixos = {
        infinisil = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7zf2O8yBXxh2tX9v/3ZztXtYeV4W9vTY2iSrm92HSErjz5KcIY/AAKaqbWXHZgsZk2pehBqNbQMOwn0WWdLvil2+Ah97cvl7d9b9XdCkfOPhNB6FKcTzPmMp5Rivi/IodVMhT2xO9S1zO0Y2Q7dsYgk5leKyiD10pkcw23p6MPMKhKV2DPgY6BiszrTEVmtyOHpGkji9rE1iB9MyOINY9eC4etmnNINXMlwttV0GjbJI9WXXEQN2mRaPPp1PBWaPOgoP3ufKi9MR1hEhAantyrfBm2SeqjUvXG5JN1RyooohIWIHWXNJlYFldFPsCD/C1HnE5ylJeLBbZEw0TPb6x";
        root = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQClF2osxohcHFFir5eG55WfsZfdAEphtPtQ88hqj5Tu/LJUtrRa1W+4UsJx9HpUkG7+F9BrkpZeI/1ZW6qVgGX7SCrExDFG5APMMgww6aufw67EhWTGzG+WWvD7bjIswyg7nIqsNuSX7MEkj20hFJjKF+yGvyjyMUj+vERjLBLy3tNkV89e6DgvpeRCT2qGVA3MM7J5RwKQZh4rrALr5Krwk2TEhz34jJVyEv08ojXncqbQXUSRGAIYMF56xPvYO3/P92gUGYHaDUOGrflcQZqvCwoq3n0C9MQT0pcnOoER5rbYCdRt57XHQ5yAYQxyD4e5AlCnxFB5VZmipXFoC9Vx";
      };

      osx = {
        infinisil = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsMBRhTAKrXSL3rkZ4//WpG8cOOUTMLB7yrWdEnfYuzNf7vm0+cDh7GIjCRrq2dIA6I4b+oWowI0zD4l5SjJyuYq7lr0W6MudcosUPxJ7ixmV5iKP3io2tvah2Jyw8O8wZ3iPNfUg0u4zvmgKdVlZBqdUBtl1KflOnGh+gDZ1RzlpfzDB4bWUCwiRCPzMElamMAJjhcIWi8kO9zDEjto8MMFtHJjhuEXrOavoo/51bgFustWWUBN7mQH4eezL6RyzLAJ5a1dRg5sm9wJIMFyrERJVis8wdbVXwAI+MPIQvE1EmhX5k2UlBSUj08HC/oNCEeJPtdh/4nBLLGsKQtkkj";
        old = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDG83exW8YT0mqQ8W6Y/L3BB+Zb87UyDKHXWGSKYXdhn8fcGGLrQTIaPj6IjIxz3OhSc+UxAkfDu0avaVMBb7TttYtqgoB9NVQwHZC9NUXkL1TG338JR7Tm1jpgmlpxPwBzNGHxb5SBeAoxrKO6Wp+Yj1oV8jQ5KPKcFBhRz9HICN4uGGgpxFhDZe6N4JXBm02DyG4iESoe8nLvQYG3BvP+TbWYsQ9Pgo4c+c7S1oLH93O3BS/pBSm+Kx4rvLbQ2taAS373WEshPaBGf69/bswHQvbVUsgXa2P0mmoBNw5GxNlXagwxN8nWdarDJbLwUKVhaDImpPxGi7wbjV/t940f";
      };
    };

    nixops = {
      mac = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKvQacUXH9alOeDhaPaTDyf0BVOH3jFbR9czWHqOeo63 NixOps client key for mac";
    };

    iPhone = {
      pass = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD1Ug8L5MhT4BKJ5aaDArerqtCgzMzMRM+GUsvshD5HBbckL+DB0RfKAlQWaxn8crPYXdzULAbvLArUvyb84LgIhpbVSPeX4pybasu3i4EEIJbVpNbv7k4D6WMFzE+u840xD1ijSMiYupvlDKiEGeX0/4zqe8AMG0v0ItprmH4v2NloA1C/peknhp6etaitUwXKlRZ30zZiDmqpAbOIP3DV09CGfZmsWlPsYB+0zplLT4QSsYsZ4bD/Sit1nqzwLEhXxLSBF8eMWUgrEUSNrp3sT9LAP3sdpKbSxevdpJNlsQUum4VZaJGcbmrgmRMpU1Qp523ipsiuUcpHW8AkFa8x";
    };

    server = {
      infinisil = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCfhaM5nKp4bpKM56uN3Ob9Hh2M9T836saLeGCTUa9d3c+xQM3erPxgLt5Mv+EgwHvzyk8lmI/s5za6pTzHmzYhFErPtDXD+3VWvAQxqrtyM0kRZf7+M/88/hS/2Ojn0UPkaPfoPsFRNqX5vQ3t15GPEygdbNFB17/w8dssc5UiighDBoAAU0Bs+6PZHs5JZXOG+3O5tD+1rd8oPN2t1Eqo4aOB7YPTRqUVydZz2xAQC4JHe/BH6fPeZo4GOeZMKC/FQU/oCv0E6ZuSEEDtdrWwKdxyL1HkcLm7XJVooXAJwvPwLbtZW1t4HKlVyAfT4qtAaMzPqZu7xU+wxPhbxD15 infinisil@mail.infinisil.io";
      root = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCzMEOE8qGqPor0IcRfPisrPAGNPONksDbv4KlfdNMMS21NbuNshdkoMEm76ld9EfBaDHzJ4yta4M79UItuuqX6e+y5eY2140AxC0HoQcjURJiu7rnVGVBmnpdUrh8GlugvIksBbjKfkaC6c6VePeKKku62GZEZgS0iaAblNs3bBI7BCO1pFDgmZKar+1YV3mN9SqazDz9NkoAZJilUCzgxlhj3WOy/DWsWf7g/ahoSaVfNlE+U/OHeItyejPIcdZyW+/0qdWOZlyOzAsR3haPQQMGJIq2EO92Te30oPo5RFtMl9eibFwSN4IwSbHAMPo/HpR+A/igFYkYjJHxktjNX root@mail.infinisil.io";
    };
  };
in

{

  options.sshkeys = mkOption {
    type = types.attrs;
    default = keys;
    readOnly = true;
  };

}
