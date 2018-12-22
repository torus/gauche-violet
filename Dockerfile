FROM practicalscheme/gauche

RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

WORKDIR /code
CMD make run
