FROM practicalscheme/gauche

RUN apt-get install -y libuv1 libuv1-dev
RUN apt-get install -y gdb

RUN git clone https://github.com/shirok/Gauche.git
RUN apt-get install -y m4 autoconf automake libtool pkg-config
WORKDIR Gauche
RUN ./DIST gen
RUN ./configure
RUN make && make check && make install

WORKDIR /code
CMD make run
