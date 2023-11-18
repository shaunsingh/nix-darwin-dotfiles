let # TODO fill these
  shaunsingh.darwin = "ssh-ed25519 ";
  shaurizard.linux = "ssh-ed25519 ";

  nix-darwin-aarch64 = "ssh-ed25519 ";
  nixos-c1-x86_64 = "ssh-ed25519 ";

  users = [ shaurizard.linux shaunsingh.darwin ];
  hosts = [ nix-darwin-aarch64 nixos-c1-x86 ];
in
{
  "tokens.age".publicKeys = users ++ hosts;
}