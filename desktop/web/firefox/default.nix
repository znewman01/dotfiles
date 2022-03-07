{ config, pkgs, ... }:

{
  xdg.configFile."tridactyl/tridactylrc".source = ./tridactylrc;
  home.file.".mozilla/native-messaging-hosts/tridactyl.json".text = ''
    {
        "name": "tridactyl",
        "description": "Tridactyl native command handler",
        "path": "${pkgs.tridactyl-native}/share/tridactyl/native_main.py",
        "type": "stdio",
        "allowed_extensions": [ "tridactyl.vim@cmcaine.co.uk","tridactyl.vim.betas@cmcaine.co.uk", "tridactyl.vim.betas.nonewtab@cmcaine.co.uk" ]
    }
  '';

  home.packages = [ pkgs.tridactyl-native ];
}
