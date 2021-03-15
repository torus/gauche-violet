FROM practicalscheme/gauche:latest

RUN apt-get update
RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g nodemon

WORKDIR /work
RUN curl -L -O https://github.com/torus/gauche-rheingau/releases/download/v0.1.0/gauche-rheingau-0.1.0.tgz
RUN gauche-package install gauche-rheingau-*.tgz

RUN curl -L -O https://github.com/torus/gauche-violet/archive/v0.1.0-pre001.tar.gz
RUN pwd && ls && tar xvfz v0.1.0-pre001.tar.gz

WORKDIR /code
RUN make -C /work/gauche-violet-0.1.0-pre001 install

CMD make -f docker.mk run
