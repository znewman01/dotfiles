{ config, lib, pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    profiles.zjn = {
      name = "Zachary Newman";
      settings = {
        "browser.aboutConfig.showWarning" = false;
        "general.warnOnAboutConfig" = false;
        "browser.startup.homepage" = "about:blank";
        "browser.newtabpage.enabled" = false;
        # For Tridactyl support in more places
        "privacy.resistFingerprinting.block_mozAddonManager" = true;
        "extensions.webextensions.restrictedDomains" = "";
      };
    };
  };

  xdg.configFile."tridactyl/themes/dracula.css".source = ./dracula-tridactyl.css;
  xdg.configFile."tridactyl/tridactylrc".source = ./tridactylrc;
}
