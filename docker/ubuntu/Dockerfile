FROM ubuntu:focal

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get dist-upgrade -y \
  && apt-get install -y --no-install-recommends git gcc curl gulp liblzma-dev libmysqlclient-dev pkg-config libpcre3-dev libpq-dev make libpq5 libmysqlclient21 libatomic1 ca-certificates libssl-dev openssh-client \
  && rm -Rf /var/cache/apt

RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -
RUN apt-get install -y --no-install-recommends nodejs

RUN npm install -g yarn
RUN curl -sSL https://get.haskellstack.org/ | sh
