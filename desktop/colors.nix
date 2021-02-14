{ lib }:
with lib;
let
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

  # Base16 Espresso
  base00 = "2d2d2d";
  base01 = "393939";
  base02 = "515151";
  base03 = "777777";
  base04 = "b4b7b4";
  base05 = "cccccc";
  base06 = "e0e0e0";
  base07 = "ffffff";
  base08 = "d25252";
  base09 = "f9a959";
  base0A = "ffc66d";
  base0B = "a5c261";
  base0C = "bed6ff";
  base0D = "6c99bb";
  base0E = "d197d9";
  base0F = "f97394";
  r = string: toString (hexToDec (builtins.substring 0 2 string));
  g = string: toString (hexToDec (builtins.substring 2 2 string));
  b = string: toString (hexToDec (builtins.substring 4 2 string));

  test = builtins.trace "${r base00}" null;
}
