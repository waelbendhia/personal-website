{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ zlib watchexec ];
  shellHook = ''
    export GIT_HASH="$(git rev-parse --short HEAD)"
  '';
}
