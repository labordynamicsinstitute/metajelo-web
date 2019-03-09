FROM node:8

MAINTAINER Brandon Elam Barker
# Based on Alexej Bondarenko's Dockerfile at:
# https://raw.githubusercontent.com/ersocon/docker-purescript/master/Dockerfile

ENV PURESCRIPT_DOWNLOAD_SHA1 f0c9fae808ae27973de7c77519f87ae6e4837312
ENV PSC_PACKAGE_DOWNLOAD_SHA1 09e033708f412e25ff049e5a9de42438b4696a33

RUN npm install -g bower parcel-bundler pulp@12.3.0

RUN cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.12.3/linux64.tar.gz \
    && echo "$PURESCRIPT_DOWNLOAD_SHA1 linux64.tar.gz" | sha1sum -c - \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz \
    && wget https://github.com/purescript/psc-package/releases/download/v0.5.1/linux64.tar.gz \
    && echo "$PSC_PACKAGE_DOWNLOAD_SHA1 linux64.tar.gz" | sha1sum -c - \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz

ENV PATH /opt/purescript:/opt/psc-package:$PATH

RUN userdel node

RUN echo "#!/usr/bin/env bash\n\$@\n" > /opt/entrypoint && \
  chmod a+x /opt/entrypoint

ENTRYPOINT ["/opt/entrypoint"]
