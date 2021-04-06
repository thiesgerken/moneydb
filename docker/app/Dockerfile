FROM archlinux/base

RUN echo "Server = https://mirrors.kernel.org/archlinux/\$repo/os/\$arch" > /etc/pacman.d/mirrorlist && pacman -Syyu --noconfirm make unzip jdk8-openjdk jre8-openjdk maven && rm -Rf /var/cache

RUN curl https://dl.google.com/android/repository/sdk-tools-linux-4333796.zip -o sdk-tools-linux.zip && unzip sdk-tools-linux.zip -d /opt/android-sdk && rm sdk-tools-linux.zip

ENV ANDROID_HOME /opt/android-sdk
ENV JAVA_HOME /usr/lib/jvm/default-runtime
ENV PATH ${PATH}:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/platform-tools

RUN sdkmanager --update
RUN echo yes | sdkmanager "build-tools;28.0.3" "platform-tools" "platforms;android-27" "platforms;android-28"
