{
  network = { description = "zjn"; };

  "zjn-x1prime" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "zjn";
    deployment.targetHost = "zjn-x1prime";
    imports = [ ../machines/zjn-x1prime/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-x1prime/default.nix ];
  };

  "zjn-home" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "zjn";
    deployment.targetHost = "zjn-home";
    imports = [ ../machines/zjn-home/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-home/default.nix ];
  };

  "zjn-work" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "zjn";
    deployment.targetHost = "zjn-work";
    imports = [ ../machines/zjn-work/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-work/default.nix ];
  };
}
