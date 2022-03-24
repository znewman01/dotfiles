{ config, pkgs, lib, ... }:

let colors = (import ./../desktop/colors) { lib = lib; };
in {
  imports = [ ./default.nix ];
  services.emacs = {
    enable = true;
    socketActivation.enable = true;
  };

  home.file.".doom-themes/base16-zjn-theme.el".text = ''
     (require 'base16-theme)
     (defvar base16-zjn-colors
       '(:base00 "#${colors.base00}"
         :base01 "#${colors.base01}"
         :base02 "#${colors.base02}"
         :base03 "#${colors.base03}"
         :base04 "#${colors.base04}"
         :base05 "#${colors.base05}"
         :base06 "#${colors.base06}"
         :base07 "#${colors.base07}"
         :base08 "#${colors.base08}"
         :base09 "#${colors.base09}"
         :base0A "#${colors.base0A}"
         :base0B "#${colors.base0B}"
         :base0C "#${colors.base0C}"
         :base0D "#${colors.base0D}"
         :base0E "#${colors.base0E}"
         :base0F "#${colors.base0F}")
     "All colors for Base16 zjn are defined here.")
    (deftheme base16-zjn)
    (base16-theme-define 'base16-zjn base16-zjn-colors)
    (provide-theme 'base16-zjn)
    (provide 'base16-zjn-theme)
  '';

  xdg.mimeApps = {
    enable = true;
    defaultApplications = { "application/pdf" = "emacsclient.desktop"; };
  };
  xdg.dataFile."applications/emacsclient.desktop".text = ''
    [Desktop Entry]
    Name=EmacsClient
    GenericName=Text Editor
    Comment=Edit text
    MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
    Exec=em %F
    Icon=emacs
    Type=Application
    Terminal=false
    Categories=Development;TextEditor;
    StartupWMClass=Emacs
    Keywords=Text;Editor;
  '';

}
