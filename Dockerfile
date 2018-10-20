FROM practicalscheme/gauche

RUN apt install -y libuv1 libuv1-dev
RUN apt install -y gdb

WORKDIR /code
CMD make run
