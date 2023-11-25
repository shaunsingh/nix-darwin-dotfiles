{ pkgs
, lib
, config
, ...
}:
let
  nverStable = config.boot.kernelPackages.nvidiaPackages.stable.version;
  nverBeta = config.boot.kernelPackages.nvidiaPackages.beta.version;
  nvidiaPackage =
    if (lib.versionOlder nverBeta nverStable)
    then config.boot.kernelPackages.nvidiaPackages.stable
    else config.boot.kernelPackages.nvidiaPackages.beta;
in
{
  imports = [
    # auto generated
    ./hardware-configuration.nix
  ];

  users.users.shaurizard = {
    isNormalUser = true;
    home = "/home/shaurizard";
    shell = pkgs.fish;

    extraGroups = [
      "wheel"
      "video"
      "audio"
      "realtime"
    ];
  };

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
    enableRedistributableFirmware = true;
    nvidia = {
      modesetting.enable = true;
      package = nvidiaPackage;
      powerManagement.enable = false;
    };
    opengl = {
      enable = true;
      driSupport = true;
    };
  };

  environment = {
    sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      WLR_RENDERER = "vulkan";
      WLR_DRM_NO_ATOMIC = "1";
      NVD_BACKEND = "direct";
      LIBVA_DRIVER_NAME = "nvidia";
      MOZ_DISABLE_RDD_SANDBOX = "1";
      MOZ_GLX_TEST_EARLY_WL_ROUNDTRIP = "1";
    };
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd 'sway --unsupported-gpu'";
      initial_session = {
        command = "sway --unsupported-gpu";
        user = "shaurizard";
      };
    };
  };
}
