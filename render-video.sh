#!/usr/bin/env bash

# TODO(#7): render-video.sh script parameters are not customizable (threads, fps, output folder, etc.)

set -xe

pushd out/
  find -type f -name \*.svg | xargs -t -P 5 -n 1 -I xx convert xx xx.png
  ffmpeg -framerate 30 -i %d.svg.png output.mp4
popd
