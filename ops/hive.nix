{
  meta.nixpkgs = <nixpkgs>;

  zjn-x1prime = { name, nodes, ... }: {
    imports = [ ../machines/zjn-x1prime/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-x1prime/default.nix ];
    deployment.allowLocalDeployment = true;
  };

  zjn-home = { config, pkgs, lib, ... }: {
    imports = [ ../machines/zjn-home/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-home/default.nix ];
    deployment.allowLocalDeployment = true;
  };

  zjn-work = { config, pkgs, lib, ... }: {
    imports = [ ../machines/zjn-work/system.nix ];
    home-manager.users.zjn.imports = [ ../machines/zjn-work/default.nix ];
    deployment.allowLocalDeployment = true;
  };

  zjn-cloud = { config, pkgs, lib, ... }: {
    imports = [ ../machines/zjn-cloud/system.nix ];
    deployment = {
      targetUser = "root";
      targetHost = "34.83.62.121";
    };
  };

}
