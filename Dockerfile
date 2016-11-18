FROM ros:kinetic-perception
ENV STACK_VERSION 1.2.0
ENV STACK_DOWNLOAD_URL https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz
ENV PATH $PATH:/root/.local/bin
ENV LANG C.UTF-8
ENV TERM xterm
COPY ./keyboard /etc/default/keyboard
RUN apt-get update; apt-get install -qy ros-kinetic-turtlesim
RUN apt-get install -qy g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase && \
    mkdir -p /root/.local/bin && \
    curl -L $STACK_DOWNLOAD_URL | tar zx -C /root/.local/bin/ --wildcards '*/stack' --strip=1  && \
    chmod +x /root/.local/bin/stack
RUN stack setup 8.0.1

# x2go setup
RUN apt-get install -qy openssh-server software-properties-common python-software-properties
RUN add-apt-repository ppa:x2go/stable
RUN apt-get update
RUN apt-get install -qy x2goserver x2goserver-xsession lxde-core
