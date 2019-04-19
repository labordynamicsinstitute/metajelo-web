#!/usr/bin/env bash

# Make these directories so docker (root) does not!
mkdir -p ~/.pulp
mkdir -p ~/.npm
mkdir -p ~/.npm-packages
mkdir -p ~/.cache
touch ~/.pulp/github-oauth-token

docker run --rm -ti \
       --volume /etc/passwd:/etc/passwd:ro \
       --volume "$PWD":/wd \
       --volume "$HOME/.gitconfig:$HOME/.gitconfig:ro" \
       --volume "$HOME/.ssh:$HOME/.ssh:ro" \
       --volume "$HOME/.pulp:$HOME/.pulp" \
       --volume "$HOME/.cache:$HOME/.cache" \
       --volume "$HOME/.npmrc:$HOME/.npmrc" \
       --volume "$HOME/.npm:$HOME/.npm" \
       --volume "$HOME/.npm-packages:$HOME/.npm-packages" \
       --user "$UID" \
       --workdir /wd \
       -e "XDG_CONFIG_HOME=/wd/.xdg_config_home" \
       -e "XDG_DATA_HOME=/wd/.xdg_data_home" \
       purescript:0.12.3 "$@"

# Add this before the last line (image name) for debugging:
#        --entrypoint "/bin/bash" \
