{ inputs, ... }:

{
  # zjn-x1prime = inputs.pkgs.lib.nixosSystem {
  #   {
  #   } // (import ./zjn-x1prime { inherit config pkgs lib inputs; });
  # };

  zjn-work = inputs.pkgs.lib.nixosSystem {
    inherit system;
    modules = [
	import ./zjn-work { inherit config pkgs lib inputs; }
    ];
  };

  # zjn-home = { config, pkgs, lib, ... }:
  #   {
  #     imports = [ ./zjn-home ];
  #     deployment.allowLocalDeployment = true;
  #     deployment.targetUser = "zjn";
  #   } // (import ./zjn-home { inherit config pkgs lib inputs; });

  # zjn-cloud = { config, pkgs, lib, ... }: { imports = [ ./zjn-cloud ]; };
}
