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

# Copy AMD APP SDK into container
#COPY AMD-APP-SDKInstaller-v3.0.130.136-GA-linux64.tar.bz2 /data/AMD-APP-SDKInstaller-v3.0.130.136-GA-linux64.tar.bz2

# Install OpenCL
#RUN \
#  cd /data && \
#  tar xf AMD-APP-SDKInstaller-v3.0.130.136-GA-linux64.tar.bz2 && \
#  ./AMD-APP-SDK-v3.0.130.136-GA-linux64.sh -- -s true -a yes && \
#  export LD_LIBRARY_PATH="/opt/AMDAPPSDK-3.0/lib/x86_64/sdk/:/opt/AMDAPPSDK-3.0/lib/x86_64/:$LD_LIBRARY_PATH" && \
#  clinfo

# install hypermapper/baco - does not occur in docker? 
RUN \
  cd /home && \
  git clone https://github.com/baco-authors/baco.git && \
  cd baco && \
  apt-get install -y pip && \
  pip install --upgrade pip && \
  pip install -e .

# export pythonpath with baco
ENV PYTHONPATH="/home/baco"

# install shine
RUN \
    cd /home && \
    git clone https://github.com/rise-lang/shine.git && \
    cd shine && \
    ./setup.sh && \
    sbt compile


# warning: Command fails - Solution:(ssh vs. https) 
# build transform ... fails 
#RUN \
	#cd /home/baco && \
	#bash install_baselines.sh

#ENV LIBRARY_PATH="/opt/AMDAPPSDK-3.0/lib/x86_64/sdk/:/opt/AMDAPPSDK-3.0/lib/x86_64/:$LIBRARY_PATH"
#ENV LD_LIBRARY_PATH="/opt/AMDAPPSDK-3.0/lib/x86_64/sdk/:/opt/AMDAPPSDK-3.0/lib/x86_64/:$LD_LIBRARY_PATH"
#ENV CPATH="/opt/AMDAPPSDK-3.0/include/"
