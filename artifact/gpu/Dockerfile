FROM  openjdk:11

# check versions
#ENV SCALA_VERSION 2.13.3
#ENV SBT_VERSION 1.3.13

# install opencl headers and clang
RUN \
  apt-get update && \
  apt-get install -y \
	build-essential \
	vim \
  	cmake \
	mpich \
    ruby \
    pkg-config \
  	opencl-headers \
	ocl-icd-libopencl1 \
	ocl-icd-opencl-dev \
 	clang \
  	clinfo

RUN mkdir -p /etc/OpenCL/vendors && \
    echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES compute,utility

# install java and sbt
RUN \
  apt install -y default-jdk && \
  apt install -y autoconf automake libtool libgsl-dev && \
  apt-get install apt-transport-https curl gnupg -yqq && \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import && \
  chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg && \
  apt-get update && \
  apt-get install -y sbt

# install baco
RUN \
  cd /home && \
  git clone https://github.com/baco-authors/baco.git && \
  cd baco && \
  apt-get install -y pip && \
  pip install --upgrade pip && \
  pip install -e .

# install baselines
RUN \
	cd /home/baco && \
	bash install_baselines.sh

# set environment variables
ENV PYTHONPATH="/home/baco:/home/baco/extra_packages/CCS/bindings/python/"
ENV LD_LIBRARY_PATH="/usr/local/lib"

# install shine
RUN \
    cd /home && \
    git clone https://github.com/rise-lang/shine.git && \
    cd shine && \
    ./setup.sh && \
    sbt compile && \
    git checkout autotuning_benchmarks_artifact && \
    ./setup.sh