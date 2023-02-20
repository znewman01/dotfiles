{ config, lib, pkgs, ... }:
# TODO: replace with https://github.com/bertof/nix-rice?

with lib;

let
  parseFile = file:
    let dict = builtins.fromTOML (builtins.readFile file);
    in {
      colors = {
        base00 = dict.base00; # Default Background
        base01 = dict.base01; # Lighter Background
        base02 = dict.base02; # Selection Background
        base03 = dict.base03; # Comments, Invisibles, Line Highlighting
        base04 = dict.base04; # Dark Foreground (Used for status bars)
        base05 = dict.base05; # Default Foreground, Caret, Delimiters, Operators
        base06 = dict.base06; # Light Foreground (Not often used)
        base07 = dict.base07; # Light Background (Not often used)
        base08 = dict.base08; # Variables, Tags, Links/Lists, Diff Deleted
        base09 = dict.base09; # Integers, Boolean, Constants, Markup Link Url
        base0A = dict.base0A; # Classes, Markup Bold, Search Text Background
        base0B = dict.base0B; # Strings, Inherited Class, Markup Code
        base0C = dict.base0C; # Regular Expressions, Escape Characters, Quotes
        base0D = dict.base0D; # Functions, Methods, Attribute IDs, Headings
        base0E = dict.base0E; # Keywords, Storage, Selector, Markup Italic
        base0F = dict.base0F; # Deprecated, Opening/Closing Embedded Tags
      };
      mode = dict.mode;
    };
  themes = listToAttrs (builtins.map (file:
    nameValuePair (removeSuffix ".toml" (builtins.baseNameOf file))
    (parseFile file)) [
      ./espresso.toml
      ./solarized.toml
      ./material.toml
      ./material-vivid.toml
      ./nord.toml
      ./one-light.toml
    ]);
  cfg = config.colorScheme;
in {
  options.colorScheme = {
    enable = mkOption {
      type = types.bool;
      description = "Whether to enable base16 color scheme.";
      default = false;
    };
    name = mkOption {
      type = types.nullOr (types.enum (builtins.attrNames themes));
      example = "espresso";
      description = "The name of the color scheme to use.";
      default = null;
    };
    colors = mkOption {
      type = types.nullOr (types.attrsOf types.str);
      description = "The base16 color scheme.";
      example = themes.solarized.colors;
      default = null;
    };
    mode = mkOption {
      type = types.enum [ "dark" "light" ];
      description = "Dark or light mode?";
      example = "light";
      default = "light";
    };
  };

  config = let scheme = builtins.getAttr cfg.name themes;
  in mkIf (cfg.enable) { colorScheme = { inherit (scheme) colors mode; }; };
}
