FROM ubuntu:21.10

RUN apt-get update
RUN apt-get upgrade -y
RUN DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y bc bison ca-certificates clang flex git libgmp3-dev libssl-dev locales make mysql-server mysql-client m4 opam openjdk-8-jdk postgresql postgresql-contrib python3 python3-setuptools texlive vim

RUN adduser --disabled-password --gecos "" rcsql
RUN locale-gen en_US.UTF-8 &&\
    echo "export LANG=en_US.UTF-8 LANGUAGE=en_US.en LC_ALL=en_US.UTF-8" >> /home/rcsql/.bashrc

USER rcsql
ENV WDIR /home/rcsql
ENV LDDIR ${WDIR}/ldd-r6438
WORKDIR ${WDIR}

RUN opam init -y --disable-sandboxing
RUN opam install -y ocamlbuild ocamlfind dune dune-build-info menhir qcheck zarith
RUN opam switch create 4.05.0
RUN opam switch default

# Amazon
RUN mkdir -p amazon
RUN cd amazon; wget http://deepyeti.ucsd.edu/jianmo/amazon/categoryFiles/Gift_Cards.json.gz
RUN cd amazon; wget http://deepyeti.ucsd.edu/jianmo/amazon/metaFiles2/meta_Gift_Cards.json.gz
RUN cd amazon; wget http://deepyeti.ucsd.edu/jianmo/amazon/categoryFiles/Musical_Instruments.json.gz
RUN cd amazon; wget http://deepyeti.ucsd.edu/jianmo/amazon/metaFiles2/meta_Musical_Instruments.json.gz

# MonPoly/VeriMon
RUN git clone https://bitbucket.org/jshs/monpoly.git
RUN eval `opam config env`; cd monpoly; dune build --release; dune install

# local files
ADD . ${WDIR}
USER root
RUN chmod 755 /home/rcsql
RUN chown -R rcsql:rcsql *
USER rcsql

# Amazon
RUN make -C amazon
RUN cd amazon; python3 amazon.py Gift_Cards; python3 amazon.py Musical_Instruments

# MonPoly-REG
RUN cd /home/rcsql/monpoly-reg-1.0/src/mona; ./configure; make
USER root
RUN cd /home/rcsql/monpoly-reg-1.0/src/mona; make install
USER rcsql
RUN opam switch 4.05.0; eval `opam config env`; cd monpoly-reg-1.0; make
RUN opam switch default

# DDD
RUN make -C dddlib
RUN make -C ddd-rc
USER root
RUN cp /home/rcsql/dddlib/lib/* /usr/local/lib
USER rcsql

# LDD
RUN cd ldd-r6438; ./configure; make
RUN make -C ldd-rc

# Ailamazyan et al.
RUN eval `opam config env`; make -C ailamazyan/src

# radb
USER root
RUN cd radb; python3 setup.py build; python3 setup.py install
USER rcsql

# RC2SQL/VGTrans
RUN eval `opam config env`; make -C src

# Tools
RUN make -C tools

# PostgreSQL
USER root
RUN find /etc/postgresql -name "postgresql.conf" | xargs sed -i "s/.enable_nestloop = on/enable_nestloop = off/"

# MySQL
RUN usermod -d /var/lib/mysql mysql
RUN echo '[mysqld]' >> /etc/mysql/my.cnf
RUN echo 'local-infile=1' >> /etc/mysql/my.cnf

# Startup
RUN echo 'export LD_LIBRARY_PATH=/usr/local/lib' >> /home/rcsql/.bashrc
RUN echo "/etc/init.d/postgresql start; su - postgres -c 'psql --command \"CREATE ROLE rcsql WITH SUPERUSER LOGIN;\"'; su - postgres -c 'psql --command \"CREATE DATABASE rcsql;\"'" >> /root/.bashrc
RUN echo '/etc/init.d/mysql start; mysql -e "CREATE DATABASE db;"; mysql -e "CREATE USER rcsql@localhost;"; mysql -e "GRANT ALL ON *.* to rcsql@localhost;"' >> /root/.bashrc
RUN echo 'su - rcsql' >> /root/.bashrc
