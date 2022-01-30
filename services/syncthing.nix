{ config, lib, pkgs, ... }:

{
  services.syncthing = {
    enable = true;
    devices = {
      zjn-x1prime = {
        addresses = [ "tcp://zjn-x1prime:22000" ];
        id = "WVEU7NP-QBOQDOI-W7NXM2A-IFTDQOA-A3SE2LQ-L7V4RPB-ZT75L4V-BADSPQF";
      };
      iphone = {
        addresses = [ "tcp://iphone:22000" ];
        id = "CGR6FFL-G35D6FD-IUVYLLO-7RNUQQL-UGYI2YN-MDSWTMX-NT6MPEF-QVDUPAM";
      };
      zjn-home = {
        addresses = [ "tcp://zjn-home:22000" ];
        id = "DAEVVP3-Y544DKN-MN5AECR-PF3KPRR-GBZHWVV-IMF6LG3-32MPTV5-RVEBSQV";
      };
      zjn-work = {
        addresses = [ "tcp://zjn-work:22000" ];
        id = "PHQHBOJ-3ODRPNY-U7T2IRA-XQYWPX7-I7VVKHC-UPIPN5R-4G4MZKB-PRKVQQN";
      };
    };
    folders."Default" = {
      id = "default";
      rescanInterval = 60;
      devices = [ "zjn-x1prime" "iphone" "zjn-home" "zjn-work" ];
      path = "~/default";
    };
    extraOptions = { gui = { insecureAdminAccess = true; }; };
    guiAddress = "0.0.0.0:8384";
  };
}
