# TODO: module-ify this
{ lib }:
with lib;
let
  themeName = "solarized";
  pow = let
    pow' = base: exponent: value:
      # FIXME: It will silently overflow on values > 2**62 :(
      # The value will become negative or zero in this case
      if exponent == 0 then
        1
      else if exponent <= 1 then
        value
      else
        (pow' base (exponent - 1) (value * base));
  in base: exponent: pow' base exponent base;
  hexToDec = v:
    let
      hexToInt = {
        "0" = 0;
        "1" = 1;
        "2" = 2;
        "3" = 3;
        "4" = 4;
        "5" = 5;
        "6" = 6;
        "7" = 7;
        "8" = 8;
        "9" = 9;
        "a" = 10;
        "b" = 11;
        "c" = 12;
        "d" = 13;
        "e" = 14;
        "f" = 15;
        "A" = 10;
        "B" = 11;
        "C" = 12;
        "D" = 13;
        "E" = 14;
        "F" = 15;
      };
      chars = stringToCharacters v;
      charsLen = length chars;
    in foldl (a: v: a + v) 0
    (imap0 (k: v: hexToInt."${v}" * (pow 16 (charsLen - k - 1))) chars);
in rec {
  # 00 Default Background
  # 01 Lighter Background (Used for status bars, line number and folding marks)
  # 02 Selection Background
  # 03 Comments, Invisibles, Line Highlighting
  # 04 Dark Foreground (Used for status bars)
  # 05 Default Foreground, Caret, Delimiters, Operators
  # 06 Light Foreground (Not often used)
  # 07 Light Background (Not often used)
  # 08 Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
  # 09 Integers, Boolean, Constants, XML Attributes, Markup Link Url
  # 0A Classes, Markup Bold, Search Text Background
  # 0B Strings, Inherited Class, Markup Code, Diff Inserted
  # 0C Support, Regular Expressions, Escape Characters, Markup Quotes
  # 0D Functions, Methods, Attribute IDs, Headings
  # 0E Keywords, Storage, Selector, Markup Italic, Diff Changed
  # 0F Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
  themes = {
    "material" = {
      scheme = import ./material.nix;
      mode = "light";
    };
    "nord" = {
      scheme = import ./nord.nix;
      mode = "dark";
    };
    "espresso" = {
      scheme = import ./espresso.nix;
      mode = "dark";
    };
    "one-light" = {
      scheme = import ./one-light.nix;
      mode = "light";
    };
    "solarized" = {
      scheme = import ./solarized.nix;
      mode = "light";
    };
    "dark" = { # default dark theme
      scheme = import ./espresso.nix;
      mode = "dark";
    };
    "light" = { # default light theme
      scheme = import ./one-light.nix;
      mode = "light";
    };
    "" = { # default
      scheme = import ./espresso.nix;
      mode = "dark";
    };
  };

  scheme = (builtins.getAttr themeName themes).scheme;
  mode = (builtins.getAttr themeName themes).mode;

  base00 = scheme.base00;
  base01 = scheme.base01;
  base02 = scheme.base02;
  base03 = scheme.base03;
  base04 = scheme.base04;
  base05 = scheme.base05;
  base06 = scheme.base06;
  base07 = scheme.base07;
  base08 = scheme.base08;
  base09 = scheme.base09;
  base0A = scheme.base0A;
  base0B = scheme.base0B;
  base0C = scheme.base0C;
  base0D = scheme.base0D;
  base0E = scheme.base0E;
  base0F = scheme.base0F;

  r = string: toString (hexToDec (builtins.substring 0 2 string));
  g = string: toString (hexToDec (builtins.substring 2 2 string));
  b = string: toString (hexToDec (builtins.substring 4 2 string));
}
