{ pkgs ? import <nixpkgs> {} }:

let
  faissCustom = pkgs.faiss.overrideAttrs (old: {
    cmakeFlags = (old.cmakeFlags or []) ++ [
      (pkgs.lib.cmakeBool "FAISS_ENABLE_C_API" true)
      (pkgs.lib.cmakeBool "BUILD_SHARED_LIBS" true)
    ];
  });
in

pkgs.mkShell {
  name = "faiss-shell";

  buildInputs = [ faissCustom pkgs.gmp pkgs.stack ];

  shellHook = ''
    export LD_LIBRARY_PATH=${faissCustom}/lib:$LD_LIBRARY_PATH
    echo "LD_LIBRARY_PATH set to include FAISS: ${faissCustom}/lib"
  '';
}
