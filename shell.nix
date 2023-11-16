{ pkgs ? import <nixpkgs> { } }: pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ zlib ];
  shellHook = ''
    export GIT_HASH="$(git rev-parse --short HEAD)"
  '';
}
