final: prev: {
  webkitgtk-eme = prev.webkitgtk.overrideAttrs (old: {
    buildInputs =
      old.buildInputs
      ++ [
        prev.libgpgerror
      ];
    cmakeFlags =
      old.cmakeFlags
      ++ [
        "-DENABLE_ENCRYPTED_MEDIA=ON"
      ];
  });
}
