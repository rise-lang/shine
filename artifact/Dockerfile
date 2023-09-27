FROM  openjdk:11

# check versions
#ENV SCALA_VERSION 2.13.3
#ENV SBT_VERSION 1.3.13

# install opencl headers and clang
RUN \
  apt-get update && \
  apt-get install -y build-essential && \
  apt-get install -y opencl-headers && \
  apt-get install -y clang

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
