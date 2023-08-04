FROM practicalscheme/gauche:latest

RUN apt-get update --allow-releaseinfo-change
RUN apt-get install -y libuv1 libuv1-dev

RUN curl -sL https://deb.nodesource.com/setup_16.x | bash -
RUN apt-get install -y nodejs
RUN apt-get install -y npm
RUN npm install -g nodemon

WORKDIR /code
CMD bash
