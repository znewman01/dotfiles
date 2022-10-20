{ config, pkgs, lib, ... }:

{
  programs.firefox = {
    enable = pkgs.stdenv.isLinux;
    profiles.zjn = { name = "Zachary Newman"; };
  };

  home.file.".mozilla/native-messaging-hosts/tridactyl.json".text = ''
    {
        "name": "tridactyl",
        "description": "Tridactyl native command handler",
        "path": "${pkgs.tridactyl-native}/share/tridactyl/native_main.py",
        "type": "stdio",
        "allowed_extensions": [ "tridactyl.vim@cmcaine.co.uk","tridactyl.vim.betas@cmcaine.co.uk", "tridactyl.vim.betas.nonewtab@cmcaine.co.uk" ]
    }
  '';
  xdg.configFile = {
    "tridactyl/tridactylrc".source = ./tridactylrc;
    "tridactyl/themes/base16.css".text =
      let colors = (import ../../colors) { inherit lib; };
      in ''
        :root {
            --base00: #${colors.base00};
            --base01: #${colors.base01};
            --base02: #${colors.base02};
            --base03: #${colors.base03};
            --base04: #${colors.base04};
            --base05: #${colors.base05};
            --base06: #${colors.base06};
            --base07: #${colors.base07};
            --base08: #${colors.base08};
            --base09: #${colors.base09};
            --base0A: #${colors.base0A};
            --base0B: #${colors.base0B};
            --base0C: #${colors.base0C};
            --base0D: #${colors.base0D};
            --base0E: #${colors.base0E};
            --base0F: #${colors.base0F};

            --tridactyl-fg: var(--base05);
            --tridactyl-bg: var(--base00);
            --tridactyl-url-fg: var(--base08);
            --tridactyl-url-bg: var(--base00);
            --tridactyl-highlight-box-bg: var(--base0B);
            --tridactyl-highlight-box-fg: var(--base00);

            /* Hint character tags */
            --tridactyl-hintspan-fg: var(--base00) !important;
            --tridactyl-hintspan-bg: var(--base0A) !important;

            /* Element Highlights */
            --tridactyl-hint-active-fg: none;
            --tridactyl-hint-active-bg: none;
            --tridactyl-hint-active-outline: none;
            --tridactyl-hint-bg: none;
            --tridactyl-hint-outline: none;
        }

        #command-line-holder {    order: 1;
            border: 2px solid var(--base0B);
            color: var(--tridactyl-bg);
        }

        #tridactyl-input {    padding: 1rem;
            color: var(--tridactyl-fg);
            width: 90%;
            font-size: 1.5rem;
            line-height: 1.5;
            background: var(--tridactyl-bg);
            padding-left: unset;
            padding: 1rem;
        }

        #completions table {    font-size: 0.8rem;
            font-weight: 200;
            border-spacing: 0;
            table-layout: fixed;
            padding: 1rem;
            padding-top: 1rem;
            padding-bottom: 1rem;
        }

        #completions > div {    max-height: calc(20 * var(--option-height));
            min-height: calc(10 * var(--option-height));
        }

        /* COMPLETIONS */

        #completions {    --option-height: 1.4em;
            color: var(--tridactyl-fg);
            background: var(--tridactyl-bg);
            display: inline-block;
            font-size: unset;
            font-weight: 200;
            overflow: hidden;
            width: 100%;
            border-top: unset;
            order: 2;
        }

        /* Olie doesn't know how CSS inheritance works */
        #completions .HistoryCompletionSource {    max-height: unset;
            min-height: unset;
        }

        #completions .HistoryCompletionSource table {    width: 100%;
            font-size: 9pt;
            border-spacing: 0;
            table-layout: fixed;
        }

        /* redundancy 2: redundancy 2: more redundancy */
        #completions .BmarkCompletionSource {    max-height: unset;
            min-height: unset;
        }

        #completions table tr td.prefix,#completions table tr td.privatewindow,#completions table tr td.container,#completions table tr td.icon {    display: none;
        }

        #completions .BufferCompletionSource table {    width: unset;
            font-size: unset;
            border-spacing: unset;
            table-layout: unset;
        }

        #completions table tr .title {    width: 50%;
        }

        #completions table tr {    white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }

        #completions .sectionHeader {    background: unset;
            font-weight: 200;
            border-bottom: unset;
            padding: 1rem !important;
            padding-left: unset;
            padding-bottom: 0.2rem;
        }

        #cmdline_iframe {    position: fixed !important;
            bottom: unset;
            top: 25% !important;
            left: 10% !important;
            z-index: 2147483647 !important;
            width: 80% !important;
            box-shadow: rgba(0, 0, 0, 0.5) 0px 0px 20px !important;
        }

        .TridactylStatusIndicator {    position: fixed !important;
            bottom: 0 !important;
            background: var(--tridactyl-bg) !important;
            border: unset !important;
            border: 1px var(--base0B) solid !important;
            font-size: 12pt !important;
            /*font-weight: 200 !important;*/
            padding: 0.8ex !important;
        }

        #completions .focused {    background: var(--base0B);
            color: var(--base00);
        }

        #completions .focused .url {    background: var(--base0B);
            color: var(--base00);
        }
      '';
  };
}
