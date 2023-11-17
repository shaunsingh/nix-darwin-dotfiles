{ pkgs
, lib
, inputs
, ...
}: {
  imports = [
    # auto generated
    ./hardware-configuration.nix
  ];

  boot = {
    kernelModules = [ "amd-pstate" ];

    kernelParams = [
      "amd_pstate=passive"
      "amd_pstate.shared_mem=1"
      "initcall_blacklist=acpi_cpufreq_init"
    ];
  };

  hardware = {
    cpu.amd.updateMicrocode = true;

    /*
      hardware-configuration.nix enables this by default because of this line:

      >  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

      See https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/installer/scan/not-detected.nix
    */
    enableRedistributableFirmware = true;

    nvidia = {
      modesetting.enable = true;
      open = true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
    };

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
  }; 

  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      WLR_NO_HARDWARE_CURSORS = "1";
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
      MOZ_DISABLE_RDD_SANDBOX = "1";
      MOZ_GLX_TEST_EARLY_WL_ROUNDTRIP = "1";
    };

    greetd = {
      enable = true;
      settings = {
        default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd 'sway --unsupported-gpu'";

        initial_session = {
          command = "sway --unsupported-gpu";
          user = "shaunsingh";
        };
      };
    };
  };

  # use systemd-boot
  boot.loader.systemd-boot.enable = true;

  networking.hostName = "shaunsingh-nixos-c1";
  system.stateVersion = "23.05";
}
