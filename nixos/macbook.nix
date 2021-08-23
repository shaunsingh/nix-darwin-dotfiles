{ config, pkgs, ... }:
  # Scale cpu
  services.auto-cpufreq.enable = true;


  # Macbook (non-pro), I mean it should work right?
  services.mbpfan.enable = true;

  # Intel power daemon
  services.thermald.enable = true;

}
