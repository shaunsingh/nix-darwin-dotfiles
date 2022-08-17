{ pkgs, darwinAppWrapper, mkDarwinApp, ... }:

darwinAppWrapper rec {
  appName = "Firefox";

  app = pkgs.callPackage ./app.nix {
    inherit mkDarwinApp appName appMeta;
    version = "99.0";
    sha256 = "";
  };

  appMeta = with pkgs.stdenv.lib; {
    description = "Mozilla Firefox, free web browser (binary package)";
    homepage = http://www.mozilla.org/firefox/;
    license = {
      free = false;
      url = http://www.mozilla.org/en-US/foundation/trademarks/policy/;
    };
  };
}
