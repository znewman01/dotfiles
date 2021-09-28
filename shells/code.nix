{ config, ... }:

{
  baseDir = "${config.home.homeDirectory}/git";
  repos = let
    blackFiles = {
      ".dir-locals.el".text = ''
        ((python-mode . ((blacken-mode . t))))
      '';
    };
  in {
    "fourierhnp" = {
      url = "git@github.com:factorable/fourierhnp.git";
      shell = ./fourier.nix;
      exclude.enable = true;
      extraFiles = blackFiles // {
        ".dir-locals.el".text = ''
          ((python-mode . ((blacken-mode . t)
                           (eval . (require 'vc))
                           (flycheck-pycheckers-max-line-length . 88)
                           (flycheck-pycheckers-pylint-rc .(expand-file-name ".pylintrc" (vc-git-root (buffer-file-name))))
                           (eval . (setenv
                                    "MYPYPATH"
                                    (expand-file-name "python" (vc-git-root (buffer-file-name))))))))
        '';
      };
    };
    "noisy-radio" = {
      url = "git@github.mit.edu:zjn/noisy-radio.git";
      shell = ./noisy-radio.nix;
      exclude.enable = true;
      extraFiles = {
        ".dir-locals.el".text = ''
          ((latex-mode . ((TeX-master . "document.tex"))))
        '';
      };
    };
    "security-seminar" = {
      url = "git@g.csail.mit.edu:security-seminar.git"; # must "kinit" first
      exclude.enable = true;
      shell = ./security-seminar.nix;
    };
    "authdict-paper" = {
      url = "git@github.com:alinush/authdict-paper.git";
      shell = ./authdict-paper.nix;
      exclude.enable = true;
    };
    "iacr-dl" = {
      url = "git@github.com:znewman01/iacr-dl.git";
      shell = ./iacr.nix;
      exclude.enable = true;
      extraFiles = blackFiles;
    };
    "resume" = {
      url = "git@github.mit.edu:zjn/resume.git";
      exclude.enable = true;
    };
    "pirate-radio" = {
      url = "git@github.mit.edu:zjn/pirate-radio.git";
      exclude.enable = true;
      shell = ./pirate-radio.nix;
    };
    "sm-proposal" = {
      url = "git@github.mit.edu:zjn/sm-proposal.git";
      exclude.enable = true;
      shell = ./sm-proposal.nix;
      extraFiles = {
        ".dir-locals.el".text = ''
          ((latex-mode . ((TeX-master . "proposal.tex"))))
        '';
      };
    };
    "spectrum-paper" = {
      url = "git@github.com:sachaservan/spectrum-paper.git";
      exclude.enable = true;
      shell = ./spectrum-paper.nix;
    };
    "spectrum-impl" = {
      url = "git@github.com:znewman01/spectrum-impl.git";
      exclude.enable = true;
    };
    "bellman-bignat" = {
      url = "git@github.com:znewman01/bellman-bignat.git";
      exclude.enable = true;
      shell = ./bellman-bignat.nix;
    };
    "sm-thesis" = {
      url = "git@github.mit.edu:zjn/sm-thesis.git";
      exclude.enable = true;
      shell = ./sm-thesis.nix;
    };
    "tor-cdn" = {
      url = "git@github.com:iowaguy/tor-cdn.git";
      exclude.enable = false;
      extraFiles = {
        "dataviz/.projectile".text = "";
        "latencies/.projectile".text = "";
      };
    };
    "dotfiles" = { url = "git@github.com:znewman01/dotfiles.git"; };
    "scalingsnapshots" = {
      url = "git@github.com:znewman01/scalingsnapshots.git";
    };
  };
}
