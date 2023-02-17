#!/bin/bash

# install dependencies
sudo apt-get update

# install opencl-headers and clang
sudo apt-get install -y opencl-headers
sudo apt-get install -y clang

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

# install hypermapper
git clone https://johanneslenfers:${1}@github.com/luinardi/hypermapper_dev.git
cd hypermapper_dev

# git checkout known_constraints
git checkout new_main

# install ytopt
mkdir ytopt_packages
cd ytopt_packages
python3 -m venv venv_hm
source venv_hm/bin/activate
pip install --upgrade pip
pip install torch
pip install matplotlib
pip install opentuner
pip install numexpr
pip install networkx
pip install graphviz
pip install GPy
pip install jsonschema
pip install scikit-learn==1.1.3

git clone https://github.com/ytopt-team/ConfigSpace.git
cd ConfigSpace
pip install -e .
cd ..

git clone https://github.com/ytopt-team/scikit-optimize.git
cd scikit-optimize
pip install -e .
cd ..

git clone -b version1 https://github.com/ytopt-team/autotune.git
cd autotune
pip install -e .
cd ..

git clone https://github.com/argonne-lcf/CCS.git
cd CCS
./autogen.sh
mkdir build
cd build
../configure
make
make install
cd ../bindings/python
pip install parglare==0.12.0
pip install -e .

cd ..
cd ..
cd ..

source venv_hm/bin/activate
export LIBCCONFIGSPACE_SO_={/usr/local/lib/libconfigspace.so.0.0.0}

pip install .
cd ..

# install opentuner
pip install opentuner
pip install scikit-optimize

# install shine
git clone https://github.com/rise-lang/shine.git
cd shine
./setup.sh
sbt compile

# test 
#sbt test

