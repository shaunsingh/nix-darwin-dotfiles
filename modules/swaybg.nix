 { pkgs
, lib
, inputs
, config
, ...
}: {
  systemd.user.services.swaybg = {
    Unit = {
      Description = "Wayland wallpaper daemon";
      PartOf = ["graphical-session.target"];
      After = ["graphical-session.target"];
    };
 
    Service = {
      ExecStart = "${pkgs.swaybg}/bin/swaybg --mode fill --image ${../extra/wallpapers/IBM_Developer_Posters.jpg}";
      Restart = "on-failure";
    };
    Install.WantedBy = ["graphical-session.target"];
  };
}
