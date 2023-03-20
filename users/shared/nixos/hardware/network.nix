{ pkgs
, lib
, inputs
, config
, ...
}: {
  networking.networkmanager.enable = true;
  systemd.services.NetworkManager-wait-online.enable = false;
  networking.firewall = {
    allowedTCPPorts = [ 5900 ];
    allowedUDPPorts = [ 5900 ];
  };
}
