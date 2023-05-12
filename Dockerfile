FROM practicalscheme/gauche:latest

RUN apt-get update
RUN apt-get install -y libuv1 libuv1-dev

WORKDIR /work
RUN curl -L -O https://github.com/torus/gauche-rheingau/releases/download/v0.1.1/rheingau-0.1.1.tgz
RUN gauche-package install rheingau-*.tgz

RUN curl -L -O https://github.com/torus/gauche-violet/archive/v0.3.2.tar.gz
RUN pwd && ls && tar xvfz v*.tar.gz

WORKDIR /code
RUN make -C /work/gauche-violet-* install

CMD bash
