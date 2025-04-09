{ lib, stdenv, fetchFromGitHub, fetchpatch, autoreconfHook, pam, openssh, shadow, makeWrapper
, fetchurl, buildPackages, perl, coreutils
}:
let 
  openssl_1_0_2 = stdenv.mkDerivation rec {
    pname = "openssl";
    version = "1.0.2u";

    src = fetchurl {
      url = "https://www.openssl.org/source/${pname}-${version}.tar.gz";
      sha256 = "ecd0c6ffb493dd06707d38b14bb4d8c2288bb7033735606569d8f90f89669d16";
    };

    patches = [
      ../patches/nix-ssl-cert-file.patch
      (if stdenv.hostPlatform.isDarwin
       then ../patches/use-etc-ssl-certs-darwin.patch
       else ../patches/use-etc-ssl-certs.patch)
    ];

    postPatch = ''
      patchShebangs Configure
      patchShebangs test/*
      for a in test/t* ; do
        substituteInPlace "$a" \
          --replace /bin/rm rm
      done
    '';

    outputs = [ "bin" "dev" "out" "man" ];
    setOutputFlags = false;
    separateDebugInfo = !(stdenv.hostPlatform.useLLVM or false) && stdenv.cc.isGNU;

    nativeBuildInputs = [ perl ];

    configurePlatforms = [];
    configureScript = {
        armv6l-linux = "./Configure linux-armv4 -march=armv6";
        armv7l-linux = "./Configure linux-armv4 -march=armv7-a";
        x86_64-darwin  = "./Configure darwin64-x86_64-cc";
        x86_64-linux = "./Configure linux-x86_64";
        x86_64-solaris = "./Configure solaris64-x86_64-gcc";
      }.${stdenv.hostPlatform.system} or (
        if stdenv.hostPlatform == stdenv.buildPlatform
          then "./config"
        else if stdenv.hostPlatform.isLinux
          then "./Configure linux-generic${toString stdenv.hostPlatform.parsed.cpu.bits}"
        else
          throw "Not sure what configuration to use for ${stdenv.hostPlatform.config}"
      );

    configureFlags = [
      "shared" # "shared" builds both shared and static libraries
      "--libdir=lib"
      "--openssldir=etc/ssl"
    ];

    makeFlags = [
      "MANDIR=$(man)/share/man"
      # This avoids conflicts between man pages of openssl subcommands (for
      # example 'ts' and 'err') man pages and their equivalent top-level
      # command in other packages (respectively man-pages and moreutils).
      # This is done in ubuntu and archlinux, and possiibly many other distros.
      "MANSUFFIX=ssl"
    ];

    enableParallelBuilding = true;

    postInstall = ''
      # If we're building dynamic libraries, then don't install static
      # libraries.
      if [ -n "$(echo $out/lib/*.so $out/lib/*.dylib $out/lib/*.dll)" ]; then
          rm "$out/lib/"*.a
      fi

      mkdir -p $bin
      substituteInPlace $out/bin/c_rehash --replace ${buildPackages.perl} ${perl}

      mv $out/bin $bin/

      mkdir $dev
      mv $out/include $dev/

      # remove dependency on Perl at runtime
      rm -r $out/etc/ssl/misc

      rmdir $out/etc/ssl/{certs,private}
    '';

    postFixup = ''
      # Check to make sure the main output doesn't depend on perl
      if grep -r '${buildPackages.perl}' $out; then
        echo "Found an erroneous dependency on perl ^^^" >&2
        exit 1
      fi
    '';

    meta = with lib; {
      homepage = https://www.openssl.org/;
      description = "A cryptographic library that implements the SSL and TLS protocols";
      license = licenses.openssl;
      platforms = platforms.all;
      knownVulnerabilities = [ "Support for OpenSSL 1.0.2 ended with 2019." ];
    };
  };
in
stdenv.mkDerivation rec {
  version = "2.20";
  pname = "shellinabox";

  src = fetchFromGitHub {
    owner = "shellinabox";
    repo = "shellinabox";
    rev = "v${version}";
    sha256 = "1hmfayh21cks2lyj572944ll0mmgsxbnj981b3hq3nhdg8ywzjfr";
  };

  patches = [
    ../patches/shellinabox-minus.patch
    (fetchpatch {
      name = "CVE-2018-16789.patch";
      url = "https://github.com/shellinabox/shellinabox/commit/4f0ecc31ac6f985e0dd3f5a52cbfc0e9251f6361.patch";
      sha256 = "1mpm6acxdb0fms9pa2b88fx6hp07ph87ahxi82yyqj2m7p79jx7a";
    })
  ];

  nativeBuildInputs = [ autoreconfHook makeWrapper ];
  buildInputs = [ pam openssl_1_0_2 openssh ];

  # Disable GSSAPIAuthentication errors. Also, paths in certain source files are
  # hardcoded. Replace the hardcoded paths with correct paths.
  preConfigure = ''
    substituteInPlace ./shellinabox/service.c --replace "-oGSSAPIAuthentication=no" ""
    substituteInPlace ./shellinabox/launcher.c --replace "/usr/games" "${openssh}/bin"
    substituteInPlace ./shellinabox/service.c --replace "/bin/login" "${shadow}/bin/login"
    substituteInPlace ./shellinabox/launcher.c --replace "/bin/login" "${shadow}/bin/login"
    substituteInPlace ./libhttp/ssl.c --replace "/usr/bin" "${openssl_1_0_2.bin}/bin"
  '';

  postInstall = ''
    wrapProgram $out/bin/shellinaboxd \
      --prefix LD_LIBRARY_PATH : ${openssl_1_0_2.out}/lib
    mkdir -p $out/lib
    cp shellinabox/* $out/lib
  '';

  meta = with lib; {
    homepage = "https://github.com/shellinabox/shellinabox";
    description = "Web based AJAX terminal emulator";
    license = licenses.gpl2;
    platforms = platforms.linux;
  };
}
