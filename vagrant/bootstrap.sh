#!/usr/bin/env bash

# Install ROS Indigo
echo "deb http://packages.ros.org/ros/ubuntu trusty main" > /etc/apt/sources.list.d/ros-latest.list

wget https://raw.githubusercontent.com/ros/rosdistro/master/ros.key -O - | sudo apt-key add -

apt-get update

apt-get install -y ros-indigo-ros-base

su vagrant <<EOF
rosdep init
rosdep update
echo "source /opt/ros/indigo/setup.bash" >> ~/.bashrc
EOF

# source ~/.bashrc

# Install rosinstall and support for add-apt-repository for working
# with PPAs
apt-get install -y python-rosinstall python-software-properties software-properties-common

# Get GHC and cabal-install from Herbert's PPA
# add-apt-repository ppa:hvr/ghc
# sed 's/ raring / precise /' /etc/apt/sources.list.d/hvr-ghc-raring.list > /etc/apt/sources.list.d/hvr-ghc-precise.list
apt-add-repository "deb http://ppa.launchpad.net/hvr/ghc/ubuntu precise main"
apt-get update
apt-get install -y --force-yes ghc-7.8.3
apt-get install -y --force-yes cabal-install-1.20

# cabal-install bash completion
wget https://raw.githubusercontent.com/haskell/cabal/master/cabal-install/bash-completion/cabal -O /etc/bash_completion.d/cabal

# Setup PATH, run cabal update, and configure SSH
su vagrant <<'EOF'
echo 'PATH=/home/vagrant/.cabal/bin:/opt/ghc/7.8.3/bin:/opt/cabal/1.20/bin:$PATH' >> ~/.bashrc
echo 'PATH=/opt/ros/indigo/bin:$PATH' >> ~/.bashrc
/opt/cabal/1.20/bin/cabal update
/opt/cabal/1.20/bin/cabal install cabal-install
echo "Host github.com" > ~/.ssh/config
echo "User git" >> ~/.ssh/config
# echo "IdentityFile ~/.ssh/github" >> ~/.ssh/config
echo "IdentityFile /vagrant/vagrant/ssh/github" >> ~/.ssh/config
EOF
