#!/usr/bin/env bash

#
# This is modified copy of psc.sh with the following considerations:
#
# 1. fewer volume mounts (only run dir) - for less persistent caches but
#    better isolation.
# 2. Container is run in a detached state with a fixed runtime configured
#    below; `docker exec` and `docker cp` are used to then interact with
#    the container as needed

: "${CONT_NAME:=metajelo_web_builder}"
echo "container name is ${CONT_NAME}"

CONT_RUN_TIME=3000 # seconds

: "${IMG_NAME:=purescript-hodgepodge}"
: "${IMG_VER:=latest}"
# Set this to the empty string to use locally built image:
if ! [[ -v "DHUB_PREFIX" ]]; then
  : "${DHUB_PREFIX:=bbarker/}"
fi

USE_EXISTING=false

if docker ps -a | grep -q "${CONT_NAME}" ; then
  FOUND_IMG=$(docker inspect --format='{{.Config.Image}}' "${CONT_NAME}")
  EXPECTED_IMG="${DHUB_PREFIX}${IMG_NAME}:${IMG_VER}"
  if [ "$FOUND_IMG" == "$EXPECTED_IMG" ]; then
    USE_EXISTING=true
  else
    echo "FOUND_IMG is '$FOUND_IMG' but exptected '$EXPECTED_IMG'"
  fi
fi

if [ "$USE_EXISTING" = true ]; then
    docker start "${CONT_NAME}"
else

  # If really an empty string, then we interpret as using a local image
  # and do not pull:
  if ! [ -z "$DHUB_PREFIX" ]; then
    docker pull "${DHUB_PREFIX}${IMG_NAME}:${IMG_VER}"
  fi

  docker run \
         --user "node" \
         --volume "$PWD":/wd \
         --workdir /wd \
          --detach=true \
         --name "${CONT_NAME}" \
         "${DHUB_PREFIX}${IMG_NAME}:${IMG_VER}" sleep 10000

fi
# Now you can run some commands, e.g.:
# docker exec "${CONT_NAME}" "$@"

# Stop the container
# docker exec "${CONT_NAME}" pkill sleep
