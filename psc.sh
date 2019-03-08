#!/usr/bin/env bash

docker run --rm -ti \
       --volume /etc/passwd:/etc/passwd:ro \
       --volume "$PWD":/wd \
       --user "$UID" \
       --workdir /wd \
       purescript:0.12.3 "$@"

# Add this before the last line (image name) for debugging:
#        --entrypoint "/bin/bash" \
