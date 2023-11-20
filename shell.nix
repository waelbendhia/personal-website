{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ zlib watchexec ghcid ];
  shellHook = ''
    export GIT_HASH="$(git rev-parse --short HEAD)"
  '';
}
