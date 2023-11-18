_:

{
  flake.overlays.default = final: prev: {
    ### -- derivations 
    otf-apple = prev.callPackage ./derivations/otf-apple.nix { };
    phocus-oxocarbon = prev.callPackage ./derivations/phocus-oxocarbon.nix { };
    sf-mono-liga-bin = prev.callPackage ./derivations/sf-mono-liga-bin.nix { };

    ### -- overlays 
    firefox-unwrapped = prev.firefox-unwrapped.overrideAttrs (old: {
      patches = (old.patches or [ ]) ++ [ ./patches/D164578.diff ];
    });
     
    ### -- scripting
    screenshot = prev.writeShellScriptBin "screenshot" ''
      ${prev.grim}/bin/grim -g "$(${prev.slurp}/bin/slurp)" - -t png | ${prev.wl-clipboard}/bin/wl-copy -t image/png
    '';
    ocrScript =
      let
        inherit (prev) grim libnotify slurp tesseract5 wl-clipboard;
        _ = prev.lib.getExe;
      in
      prev.writeShellScriptBin "wl-ocr" ''
        ${_ grim} -g "$(${_ slurp})" -t ppm - | ${_ tesseract5} - - | ${wl-clipboard}/bin/wl-copy
        ${_ libnotify} "$(${wl-clipboard}/bin/wl-paste)"
      '';
    volume = prev.writeShellScriptBin "volume" ''
      #!/bin/sh
  
      ${prev.pamixer}/bin/pamixer "$@"
      volume="$(${prev.pamixer}/bin/pamixer --get-volume)"
  
      if [ $volume = 0 ]; then
          ${prev.libnotify}/bin/notify-send -r 69 \
              -a "Volume" \
              "Muted" \
              -t 888 \
              -u low
      else
          ${prev.libnotify}/bin/notify-send -r 69 \
              -a "Volume" "Currently at $volume%" \
              -h int:value:"$volume" \
              -t 888 \
              -u low
      fi
  
      ${prev.eww-wayland-git}/bin/eww update volume-level=$volume
    '';
    microphone = prev.writeShellScriptBin "microphone" ''
      #!/bin/sh
  
      ${prev.pamixer}/bin/pamixer --default-source "$@"
      mic="$(${prev.pamixer}/bin/pamixer --default-source --get-volume-human)"
  
      if [ "$mic" = "muted" ]; then
          ${prev.libnotify}/bin/notify-send -r 69 \
              -a "Microphone" \
              "Muted" \
              -t 888 \
              -u low
      else
        ${prev.libnotify}/bin/notify-send -r 69 \
              -a "Microphone" "Currently at $mic" \
              -h int:value:"$mic" \
              -t 888 \
              -u low
      fi
    '';
    brightness =
      let
        brightnessctl = prev.brightnessctl + "/bin/brightnessctl";
      in
      prev.writeShellScriptBin "brightness" ''
        #!/bin/sh
  
        ${brightnessctl} "$@"
        brightness=$(echo $(($(${brightnessctl} g) * 100 / $(${brightnessctl} m))))
  
        ${prev.libnotify}/bin/notify-send -r 69 \
            -a "Brightness" "Currently at $brightness%" \
            -h int:value:"$brightness" \
            -t 888 \
            -u low
  
  
        ${prev.eww-wayland-git}/bin/eww update brightness-level=$brightness
      '';
  };
}