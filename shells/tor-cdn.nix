with import <nixpkgs> { };

let
  pythonPackages = ps:
    with ps; [
      # Linting
      black
      mypy
      pylint

      # Testing
    ];
in pkgs.mkShell rec {
  buildInputs = with pkgs; [
    stdenv

    # Dev environment
    vagrant
    qemu
    libvirt

    # Python development
    (python38.withPackages pythonPackages)
    nodePackages.pyright
  ];
}
