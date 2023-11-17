{ pkgs
, lib
, inputs
, config
, ...
}: {
  services = {
    greetd = {
      enable = true;
      package = pkgs.greetd.tuigreet;
      settings = {
        default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd 'sway --unsupported-gpu'";
	initial_session = {
	  command = "sway --unsupported-gpu";
	   user= "shauryasingh";
	};
      };
    };
  };
}
