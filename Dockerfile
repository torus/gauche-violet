FROM practicalscheme/gauche:latest

RUN apt-get update
RUN apt-get install -y libuv1 libuv1-dev

WORKDIR /work
RUN curl -L -O https://github.com/torus/gauche-rheingau/releases/download/v0.1.0/gauche-rheingau-0.1.0.tgz
RUN gauche-package install gauche-rheingau-*.tgz

RUN curl -L -O https://github.com/torus/gauche-violet/archive/v0.2.0.tar.gz
RUN pwd && ls && tar xvfz v*.tar.gz

WORKDIR /code
RUN make -C /work/gauche-violet-* install

CMD bash
