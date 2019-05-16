#!/usr/bin/env bash

npm install && \
bower install && \
npm run build && \
npm run testbrowser && \
npm run prod
