{ pkgs
, lib
, inputs
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
  extraEnv = { WLR_NO_HARDWARE_CURSORS = "1"; };
in
{
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
    enableRedistributableFirmware = true;
    nvidia = {
      modesetting.enable = true;
      package = nvidiaPackage;
      powerManagement.enable = false;
    };

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
  }; 

  environment = {
    variables = extraEnv;
    sessionVariables = extraEnv;
    systemPackages = with pkgs; [
      glxinfo
      vulkan-tools
    ];
  };

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    displayManager.gdm.wayland = true;
  };

  programs.fish.enable = true;

  # use systemd-boot
  boot.loader.systemd-boot.enable = true;

  networking.hostName = "shaunsingh-nixos-c1";
  system.stateVersion = "23.05";
}
