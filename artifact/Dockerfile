FROM  openjdk:11

# check versions
#ENV SCALA_VERSION 2.13.3
#ENV SBT_VERSION 1.3.13

# install opencl headers and clang
RUN \
  sudo apt-get install -y build-essentials && \
  sudo apt-get install -y opencl-headers && \
  sudo apt-get install -y clang

# install java and sbt
RUN \
  sudo apt install -y default-jdk && \
  sudo apt install -y autoconf automake libtool libgsl-dev && \
  sudo apt-get install apt-transport-https curl gnupg -yqq && \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list && \
  echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list && \
  curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import && \
  sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg && \
  sudo apt-get update && \
  sudo apt-get install -y sbt
