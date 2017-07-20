with import <nixpkgs/lib> {};

{ config, pkgs, ... }:
let
  nixos = {
    infinisil = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7zf2O8yBXxh2tX9v/3ZztXtYeV4W9vTY2iSrm92HSErjz5KcIY/AAKaqbWXHZgsZk2pehBqNbQMOwn0WWdLvil2+Ah97cvl7d9b9XdCkfOPhNB6FKcTzPmMp5Rivi/IodVMhT2xO9S1zO0Y2Q7dsYgk5leKyiD10pkcw23p6MPMKhKV2DPgY6BiszrTEVmtyOHpGkji9rE1iB9MyOINY9eC4etmnNINXMlwttV0GjbJI9WXXEQN2mRaPPp1PBWaPOgoP3ufKi9MR1hEhAantyrfBm2SeqjUvXG5JN1RyooohIWIHWXNJlYFldFPsCD/C1HnE5ylJeLBbZEw0TPb6x infinisil@NixOS";
    root = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQClF2osxohcHFFir5eG55WfsZfdAEphtPtQ88hqj5Tu/LJUtrRa1W+4UsJx9HpUkG7+F9BrkpZeI/1ZW6qVgGX7SCrExDFG5APMMgww6aufw67EhWTGzG+WWvD7bjIswyg7nIqsNuSX7MEkj20hFJjKF+yGvyjyMUj+vERjLBLy3tNkV89e6DgvpeRCT2qGVA3MM7J5RwKQZh4rrALr5Krwk2TEhz34jJVyEv08ojXncqbQXUSRGAIYMF56xPvYO3/P92gUGYHaDUOGrflcQZqvCwoq3n0C9MQT0pcnOoER5rbYCdRt57XHQ5yAYQxyD4e5AlCnxFB5VZmipXFoC9Vx root@nixos";
  };

  mac = {
    infinisil = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsMBRhTAKrXSL3rkZ4//WpG8cOOUTMLB7yrWdEnfYuzNf7vm0+cDh7GIjCRrq2dIA6I4b+oWowI0zD4l5SjJyuYq7lr0W6MudcosUPxJ7ixmV5iKP3io2tvah2Jyw8O8wZ3iPNfUg0u4zvmgKdVlZBqdUBtl1KflOnGh+gDZ1RzlpfzDB4bWUCwiRCPzMElamMAJjhcIWi8kO9zDEjto8MMFtHJjhuEXrOavoo/51bgFustWWUBN7mQH4eezL6RyzLAJ5a1dRg5sm9wJIMFyrERJVis8wdbVXwAI+MPIQvE1EmhX5k2UlBSUj08HC/oNCEeJPtdh/4nBLLGsKQtkkj";
  };

  iPhone = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD1Ug8L5MhT4BKJ5aaDArerqtCgzMzMRM+GUsvshD5HBbckL+DB0RfKAlQWaxn8crPYXdzULAbvLArUvyb84LgIhpbVSPeX4pybasu3i4EEIJbVpNbv7k4D6WMFzE+u840xD1ijSMiYupvlDKiEGeX0/4zqe8AMG0v0ItprmH4v2NloA1C/peknhp6etaitUwXKlRZ30zZiDmqpAbOIP3DV09CGfZmsWlPsYB+0zplLT4QSsYsZ4bD/Sit1nqzwLEhXxLSBF8eMWUgrEUSNrp3sT9LAP3sdpKbSxevdpJNlsQUum4VZaJGcbmrgmRMpU1Qp523ipsiuUcpHW8AkFa8x infinisil@Silvans-Air.fritz.box"

  server = {
    infinisil = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDRy1p9nhr+cQilV93MBkPcVsfR7DQ4xCa2zBE2TVLyomamTnOxtYMtNZvIC5eOn74IEbbR8HjcwVTS8XHpWOBBxt3dGYFDG+xZio8BIJSOVAfejHG41shN3aH1l1RUXK2JyuM3Obk2l5rYcRjPGIDoO1C4/LhJU/LSy3/9ZxLJ3cuuVQkLkNKqRU3eHbYQSPCwMXAIQnH/qQLg7ruZGobh8IFlGHybmnpARXN2BY5QPSQVs1F7bXFXkLo/8lzHJ8JpniWtqSaRaFGrizU/565PlTJdY3PXrHHtG3BuPrMThVjl1zwVxcLAi1ThmXvpUR3fw5yO+pzACoPRoIDcX0ER infinisil@dobby";
    root = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDMs8zqNLH8geEVr13me/EZXR4oz0LwYfzC91wNpaEmq8RCJTtVcMDA/7lHwMuXAj1aWtk/3OEVzcahlEzCCJ6BiNZl4+NVg/noy3AxnftVxtLV0ll6vbEai84ouOUkZ988ScA9exPbxir16qqLiw0HKaIYmh8v0CZXRlyqAnxycGymuhg5jiLI8JBahlZCXhO1X+B+6HvKifyyjw6c4JKmMZNasQfVDFjBDtNdlCPRux4pcTy2am77mp3Kom+hs+F44gOjyqBOEDkUR5b7j3Y9dUKrgiUR/M9TxLA/+X9iiO0RKIuTDvrtV0TCw3I6M7Fz0udStxan/uA5t/CmdE2J root@dobby";
  };
in
{
  services.openssh.enable = true;
  programs.ssh.startAgent = true;

  users.users.root.openssh.authorizedKeys.keys = [ nixos.root server.root ];
  users.users.infinisil.openssh.authorizedKeys.keys = [ nixos.infinisil server.infinisil mac.infinisil ];

  users.users.git.openssh.authorizedKeys.keys = if config.networking.hostName == "dobby" then
    [ nixos.infinisil server.infinisil mac.infinisil iPhone ] else [];
}
