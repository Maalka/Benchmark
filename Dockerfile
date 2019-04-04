FROM hseeberger/scala-sbt
ARG VERSION
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY target/universal/benchmark-${VERSION}.zip /usr/src/app/
RUN unzip benchmark-${VERSION}.zip
