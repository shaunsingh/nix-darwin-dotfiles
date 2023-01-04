{ pkgs
, lib
, inputs
, config
, ...
}: {
  # todo declarative minecraft launcher
  home.packages = with pkgs; [ prismlauncher ];
}
