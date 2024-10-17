#!/bin/bash


# setup locales (user interaction required)
# do this first separately
sudo dpkg-reconfigure locales

# install dependencies
sudo apt-get update

# install opencl-headers and clang
sudo apt-get install -y opencl-headers
sudo apt-get install -y clang

# install python3.8 for baco
sudo apt install -y wget build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libsqlite3-dev libbz2-dev
cd /usr/src
sudo wget https://www.python.org/ftp/python/3.8.16/Python-3.8.16.tgz
sudo tar xzf Python-3.8.16.tgz
cd Python-3.8.16
sudo ./configure --enable-optimizations
sudo make altinstall
cd ~

# install java and sbt
sudo apt install -y default-jdk
sudo apt install -y autoconf automake libtool libgsl-dev
sudo apt-get install apt-transport-https curl gnupg -yqq
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
sudo apt-get update
sudo apt-get install -y sbt

# install baco
git clone https://github.com/johanneslenfers/baco_paper_version.git
cd baco_paper_version
git checkout exploration
python3.8 -m venv .venv

# check if this command works
# otherwise use the location of the venv directly
source .venv/bin/activate
pip install -e .

# install opentuner
pip install opentuner
pip install scikit-optimize

# check this
# export LIBCCONFIGSPACE_SO_={/usr/local/lib/libconfigspace.so.0.0.0}

# install shine
cd ..
git clone https://github.com/rise-lang/shine.git
cd shine
./setup.sh
sbt compile

git checkout exploration

# test 
#sbt test