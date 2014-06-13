FROM alanz/haskell-ghc-7.6.3-64

# Build with
# docker build -t alanz/htelehash .

MAINTAINER alan.zimm@gmail.com

ENV DEBIAN_FRONTEND noninteractive



######### cabal-install ################################################

#RUN echo "1" && apt-get update
#RUN echo "2" && apt-get update

RUN apt-get -y install zlib1g-dev wget

RUN wget http://www.haskell.org/cabal/release/cabal-install-1.20.0.2/cabal-install-1.20.0.2.tar.gz
RUN tar xvfz cabal-install-1.20.0.2.tar.gz
RUN (cd cabal-install-1.20.0.2 && ./bootstrap.sh)
RUN rm -fr cabal-install-1.20.0.2*

########################################################################

ENV CABAL //.cabal/bin/cabal
ENV DIR  ./htelehash

RUN $CABAL update

ADD . $DIR
RUN ls $DIR

RUN cd $DIR && $CABAL sandbox delete
RUN cd $DIR && $CABAL clean
RUN cd $DIR && $CABAL sandbox init
#RUN cd $DIR && $CABAL install happy
#RUN cd $DIR && $CABAL install crypto-random
#RUN cd $DIR && $CABAL install hscolour
RUN cd $DIR && $CABAL install --dependencies-only
#RUN cd $DIR && $CABAL install
