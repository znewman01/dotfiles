{ ... }:

{
  zjn-x1prime = { config, pkgs, lib, ... }: {
    imports = [ ./zjn-x1prime ];
    deployment.allowLocalDeployment = true;
    deployment.targetUser = "zjn";
  };

  zjn-home = { config, pkgs, lib, ... }: {
    imports = [ ./zjn-home ];
    deployment.allowLocalDeployment = true;
    deployment.targetUser = "zjn";
  };

  zjn-work = { config, pkgs, lib, ... }: {
    imports = [ ./zjn-work ];
    deployment.allowLocalDeployment = true;
    deployment.targetUser = "zjn";
  };

  zjn-cloud = { config, pkgs, lib, ... }: { imports = [ ./zjn-cloud ]; };
}
