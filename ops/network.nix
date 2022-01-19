{
  network = { description = "zjn"; };

  "zjn-x1prime" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "zjn";
    deployment.targetHost = "zjn-x1prime";

    imports = [ ../common.nix ../machines/zjn-x1prime/system.nix ];

    home-manager.users.zjn = {
      imports =
        [ ../home-common.nix ../machines/zjn-x1prime/default.nix ../code.nix ];
    };
  };

  "zjn-home" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "zjn";
    deployment.targetHost = "zjn-home";

    imports = [ ../common.nix ../machines/zjn-home/system.nix ];

    home-manager.users.zjn = {
      imports =
        [ ../home-common.nix ../machines/zjn-home/default.nix ../code.nix ];
    };
  };
}
