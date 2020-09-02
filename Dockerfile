FROM practicalscheme/gauche:latest

RUN apt-get update
RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g nodemon

WORKDIR /code
CMD make run
