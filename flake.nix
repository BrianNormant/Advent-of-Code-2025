{
	description = "devenv for idris2 and java";

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
	};

	outputs = { nixpkgs, ... }: let
		system = "x86_64-linux";
		pkgs = import nixpkgs { inherit system; };
	in {
		devShells."${system}".default = pkgs.mkShell {
			packages = with pkgs; [
				zsh

				# Java
				jdk25
				
				# Idris
				idris2Packages.pack
				idris2Packages.idris2Lsp
				gmp
				chez
				rlwrap
				idris2

				# Idris-sld
                pkg-config
                SDL # TODO clone and fix the Makefile of
                # https://github.com/ECburx/Idris2GL/blob/main/src/c_src/Makefile
                # to link -lSDL
                # also override the package in ~/pack/user.toml
                SDL2
                SDL2_ttf
                SDL2_gfx
                SDL2_mixer
                SDL2_image
			];

			shellHook = ''
				export PROJECT=AOC2025
				export SHELL=zsh
                # override idris2 and idris2-lsp to the lastest version compiled by pack
                # export PATH=$PATH:/home/brian/.pack/bin
				export IDRIS2_PACKAGE_PATH=$(pack package-path)
				export IDRIS2_DATA=$(pack data-path)
				export IDRIS2_LIBS=$(pack libs-path)
				exec zsh
			'';
		};
	};
}
