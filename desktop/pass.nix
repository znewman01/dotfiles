{ config, pkgs, ... }:

{
  home.packages = [ pkgs.pass ];
  home.file.".password-store".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Sync/passwords";
  home.file.".authinfo.gpg".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Sync/passwords/authinfo.gpg";
}
