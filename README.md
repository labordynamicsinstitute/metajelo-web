# metajelo-web
Web tools to display metajelo packages
* Example Nix + Purescript project

This is a simple example project for setting up Purescript development
environment using Nix. The basic tooling like Pulp, Purescript compiler and
psc-package are downloaded from a pinned revision of nixpkgs. This allows us to
use new Purescript compiler and things like that from the pinned revision while
otherwise sticking with stable NixOS release.

In this example, the Purescript libraries are installed using psc-package and
Nix is only used to install the basic tooling.

# Usage

## Docker

* First build the image: `./build-docker.sh`

* Run `./psc.sh <command>`, e.g. `./psc.sh pulp --psc-package build`. This will run
the command in the container with the CWD mounted and then exit. Alternatively
if you want to issue multiple commands in the container quickly, you can
run `./psc.sh bash`.

## Shell environment

   1. git clone git://github.com/kaitanie/nix-purescript-example-project
   2. cd nix-purescript-example-project
   3. nix-shell

   Purescript, psc-package, and such from the pinned nixpkgs version should now
   be available in the `nix-shell` environment and can be used normally.

* Known issues

** Currently the version of Pulp from nixpkgs throws exception when used

   The relevant issue is here: https://github.com/NixOS/nixpkgs/issues/40406

   Currently the workaround is to use `npm install -g pulp` and use that one
   instead of the nixpkgs version.
