final: prev:

let
  nix-cl = final.lispPackages_new;
  sbcl = nix-cl.sbcl;

  nyxt-src = final.fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "nyxt";
    rev = "aef5118a7c30c48583b464e2e0c27d4b0060999b";
    sha256 = "1b75bqaxbpsik7wampwri5j1avldkvkqnvdgg2c3hgj05yccmkbv";
  };

  nyxt-asdf = nix-cl.build-asdf-system {
    pname = "nyxt-asdf";
    version = "2022";
    src = nyxt-src;
    systems = [ "nyxt-asdf" ];
    lisp = sbcl;
  };

  ndebug = nix-cl.build-asdf-system {
    src = final.fetchzip {
      url = "https://github.com/atlas-engineer/ndebug/archive/d9488f82489de42a2b9bbf32120a003a3aced5c6.tar.gz";
      sha256 = "0j52kn9vqadhq2n0nnw7q3440fsvl21p30i6nn75pbmnq2xgdshz";
    };
    pname = "ndebug";
    version = "0.0.2";
    lisp = sbcl;
    lispLibs = with nix-cl.sbclPackages; [
      dissect
      trivial-custom-debugger
      trivial-gray-streams
      bordeaux-threads
    ];
  };

  lisp-unit2 = nix-cl.sbclPackages.lisp-unit2.overrideLispAttrs (_: {
    src = final.fetchFromGitHub {
      owner = "AccelerationNet";
      repo = "lisp-unit2";
      rev = "db50c8ce5806b3c46713a62bae362951b32c1829";
      sha256 = "0dnq0qvbsn7ciknvmwzfjnimlzq1gdkv5xd87agmhxm1cpm1ksz0";
    };
    version = "0.9.4";
  });

  hu_dot_dwim_dot_defclass-star = nix-cl.sbclPackages.hu_dot_dwim_dot_defclass-star.overrideLispAttrs (_: {
    src = final.fetchFromGitHub {
      owner = "hu-dwim";
      repo = "hu.dwim.defclass-star";
      rev = "2698bd93073f9ba27583351221a3a087fb595626";
      sha256 = "0v6bj3xbcpz98bkv3a2skz2dh0p50mqaflgkfbrzx1dzbkl1630y";
    };
  });

  ospm = nix-cl.build-asdf-system {
    src = final.fetchzip {
      url = "https://github.com/atlas-engineer/ospm/archive/df261dedaa2e98f00b4b9ef6c41c08d231558682.tar.gz";
      sha256 = "0ixx6y20q4kcvm60lp3wca5q1nhnpakdw5avz4mlj6mm8m4z526g";
    };
    pname = "ospm";
    version = "0.0.2";
    lisp = sbcl;
    lispLibs = with nix-cl.sbclPackages; [
      alexandria
      calispel
      local-time
      moptilities
      named-readtables
      osicat
      serapeum
      trivia
    ] ++ [ hu_dot_dwim_dot_defclass-star ];
  };

  nyxt = nix-cl.build-asdf-system {
    pname = "nyxt";
    version = "2022";
    src = nyxt-src;
    lisp = sbcl;
    systems = [
      "nyxt"
      "nyxt/history-tree"
      "nyxt/class-star"
      "nyxt/prompter"
      "nyxt/tests"
      "nyxt/history-tree/tests"
      "nyxt/class-star/tests"
      "nyxt/prompter/tests"
    ];
    lispLibs = with nix-cl.sbclPackages; [
      alexandria
      bordeaux-threads
      calispel
      cl-base64
      cl-containers
      cl-css
      cl-custom-hash-table
      enchant
      cl-gopher
      cl-html-diff
      cl-json
      cl-ppcre
      cl-ppcre-unicode
      cl-prevalence
      cl-qrencode
      str
      cl-tld
      closer-mop
      clss
      cluffer
      dexador
      flexi-streams
      iolib
      local-time
      log4cl
      lparallel
      moptilities
      named-readtables
      nfiles
      nhooks
      nkeymaps
      parenscript
      phos
      plump
      quri
      serapeum
      swank
      spinneret
      trivia
      trivial-clipboard
      trivial-features
      trivial-garbage
      trivial-package-local-nicknames
      trivial-types
      uiop
      unix-opts
      cl-cffi-gtk
      cl-webkit2
      mk-string-metrics
      dissect
      py-configparser
      slynk
    ]
    ++ [
      hu_dot_dwim_dot_defclass-star
      nyxt-asdf
      ndebug
      ospm
      lisp-unit2
    ];
  };

in
{
  nyxt-3 = nyxt;
}
#   [
#   nyxt
#   (nix-cl.sbclWithPackages (_: [ nyxt ]))
# ]

# final: prev:

# let
#   src = final.fetchFromGitHub {
#     owner = "atlas-engineer";
#     repo = "nyxt";
#     rev = "aef5118a7c30c48583b464e2e0c27d4b0060999b";
#     sha256 = "e83KmC9APjiYeK9ti+eejW4VZImZ36r4mVHf1RVe5aw=";
#   };
#   inherit (final.lispPackages_new) build-asdf-system sbcl sbclPackages;

# in
# {
#   nyxt-3 = build-asdf-system {
#     lisp = sbcl;
#     pname = "nyxt";
#     version = "2022-09-22";

#     inherit (sbclPackages.nyxt) nativeBuildInputs buildInputs buildScript installPhase;

# #     asds = [ "nyxt" "nyxt-asdf" ];
# #     systems = [ "nyxt" "nyxt/tests" ];

#     lispLibs =
#       sbclPackages.nyxt.lispLibs ++ # [ nyxt_tests ] ++
#       (with sbclPackages; [ cl-cffi-gtk cl-webkit2 mk-string-metrics ]);

#     inherit src;
#   };
# }
