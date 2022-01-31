#!/bin/bash

# install dependencies
sudo apt-get update

# install opencl-headers and clang
sudo apt-get install -y opencl-headers
sudo apt-get install -y clang

# install java and sbt
sudo apt install -y default-jdk
sudo apt-get install apt-transport-https curl gnupg -yqq
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
sudo apt-get update
sudo apt-get install -y sbt

# install hypermapper
git clone https://johanneslenfers:${1}@github.com/luinardi/hypermapper_dev.git
cd hypermapper_dev
git checkout known_constraints
pip install .
cd ..

# install opentuner
pip install opentuner

# install shine
git clone https://github.com/rise-lang/shine.git
cd shine
./setup.sh
sbt compile

# test 
#sbt test

