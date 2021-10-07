{ pkgs, ... }: {
  system.activationScripts.postUserActivation.text = ''
    # Setup mu if .mbsync doesn't exist
    if [[ ! -d "/Users/shauryasingh/.mbsync/" ]]; then
      mkdir ~/.mbsync
      mu init --maildir=~/.mbsync --my-address=shaunsingh0207@gmail.com
      mu index
      mbsync --all
    fi
  '';
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    mu
    msmtp
    isync
  ];
}
