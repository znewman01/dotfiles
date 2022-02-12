{ inputs, ... }:

{
  zjn-x1prime = { config, pkgs, lib, ... }:
    {
      deployment.allowLocalDeployment = true;
      deployment.targetUser = "zjn";
    } // (import ./zjn-x1prime { inherit config pkgs lib inputs; });

  zjn-home = { config, pkgs, lib, ... }:
    {
      imports = [ ./zjn-home ];
      deployment.allowLocalDeployment = true;
      deployment.targetUser = "zjn";
    } // (import ./zjn-home { inherit config pkgs lib inputs; });

  zjn-work = { config, pkgs, lib, ... }:
    {
      imports = [ ./zjn-work ];
      deployment.allowLocalDeployment = true;
      deployment.targetUser = "zjn";
    } // (import ./zjn-work { inherit config pkgs lib inputs; });

  zjn-cloud = { config, pkgs, lib, ... }: { imports = [ ./zjn-cloud ]; };
}
