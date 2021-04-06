FROM archlinux/base

RUN echo "Server = https://mirrors.kernel.org/archlinux/\$repo/os/\$arch" > /etc/pacman.d/mirrorlist && pacman -Syyu --noconfirm base-devel python git npm gulp libmariadbclient postgresql-libs && rm -Rf /var/cache

RUN npm install -g yarn
RUN curl -sSL https://get.haskellstack.org/ | sh
