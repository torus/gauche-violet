FROM practicalscheme/gauche

RUN apt install -y libuv1 libuv1-dev

WORKDIR /code
CMD make run
