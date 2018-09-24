FROM hseeberger/scala-sbt
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY target/universal/benchmark-1.10.4.0.zip /usr/src/app/
RUN unzip benchmark-1.10.4.0.zip
