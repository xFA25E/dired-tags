{
  description = "Emacs package";

  inputs = {
    eldev.flake = false;
    eldev.url = "github:emacs-eldev/eldev/1.11.1";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    std.inputs.nixpkgs.follows = "nixpkgs";
    std.url = "github:divnix/std";

    epkg-xattr.inputs.eldev.follows = "eldev";
    epkg-xattr.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-xattr.inputs.nixpkgs.follows = "nixpkgs";
    epkg-xattr.inputs.std.follows = "std";
    epkg-xattr.url = "github:xFA25E/xattr";
  };

  outputs = {
    std,
    self,
    ...
  } @ inputs:
    std.growOn {
      inherit inputs;
      systems = ["x86_64-linux"];
      cellsFrom = ./nix;
      cellBlocks = with std.blockTypes; [
        (functions "lib")
        (installables "packages")
        (installables "devshells")
      ];
    }
    {
      devShells = std.harvest self ["automation" "devshells"];
      packages = std.harvest self ["main" "packages"];
      checks = std.harvest self [["automation" "packages"] ["main" "packages"]];
    };
}
