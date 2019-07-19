FROM practicalscheme/gauche:latest

RUN apt-get update
RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

WORKDIR /code
CMD make run
