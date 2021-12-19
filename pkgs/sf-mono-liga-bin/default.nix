

# Now to create the package for the SFmono font itself

# [[file:../../../nix-config.org::*Overlays][Overlays:2]]
{ lib, fetchFromGitHub }:
let version = "2021-07-29";
in fetchFromGitHub rec {
  name = "sf-mono-liga-bin-${version}";

  owner = "shaunsingh";
  repo = "SFMono-Nerd-Font-Ligaturized";
  rev = "83d887c6ec4989897fff19131a5de84766ecfdc9";
  sha256 = "sha256-hH5pLXD1NWqpKuQSqfDn50u2NPJmLlqOqllo2j3x2ag=";

  postFetch = ''
    mkdir -p $out/share/fonts/opentype
    tar xf $downloadedFile -C $out/share/fonts/opentype
  '';

  meta = with lib; {
    description = "Ligaturized SF Mono";
    homepage = "https://github.com/shaunsingh/SFMono-Nerd-Font-Ligaturized";
    license = licenses.asl20;
    platforms = platforms.all;
    maintainers = with maintainers; [ boppyt ];
  };
}
# Overlays:2 ends here
