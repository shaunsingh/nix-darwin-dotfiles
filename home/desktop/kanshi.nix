{ pkgs
, lib
, inputs
, config
, ...
}: {
  services.kanshi = {
    enable = true;
    profiles = {
      undocked = {
        outputs = [
          {
            criteria = "eDP-1";
            scale = 2.0;
          }
        ];
      };
    };
  };
}
