{
  meta.nixpkgs = <nixpkgs>;

  zjn-x1prime = { name, nodes, ... }: {
    imports = [ ../machines/zjn-x1prime/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-x1prime/default.nix ];
    deployment.allowLocalDeployment = true;
    deployment = { targetUser = "zjn"; };
  };

  zjn-home = { config, pkgs, lib, ... }: {
    imports = [ ../machines/zjn-home/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-home/default.nix ];
    deployment.allowLocalDeployment = true;
    deployment = { targetUser = "zjn"; };
  };

  zjn-work = { config, pkgs, lib, ... }: {
    imports = [ ../machines/zjn-work/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-work/default.nix ];
    deployment.allowLocalDeployment = true;
    deployment = { targetUser = "zjn"; };
  };

  zjn-cloud = { config, pkgs, lib, ... }: {
    imports = [ ../machines/zjn-cloud/system.nix ];
    deployment = { targetUser = "root"; };
  };

}
