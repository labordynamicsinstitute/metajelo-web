let

  nixpkgs = import (builtins.fetchTarball {
    name = "nixpkgs";
    # # 18.09; doesn't work due to https://github.com/NixOS/nixpkgs/issues/42833
    # url = https://github.com/NixOS/nixpkgs-channels/tarball/95fed28ac372c61eb83c87ad97c24b0f957827bf;
    # sha256 = "03jdb28khdm45gzwl7wvcb7h10yb6y45s7ds8bhlfk9a8phzj4hx";
    # 19.03: doesn't work due to https://github.com/NixOS/nixpkgs/issues/56025
    url = https://github.com/NixOS/nixpkgs-channels/tarball/07e2b59812de95deeedde95fb6ba22d581d12fbc;
    sha256 = "1yxmv04v2dywk0a5lxvi9a2rrfq29nw8qsm33nc856impgxadpgf";
  }) {};

in nixpkgs