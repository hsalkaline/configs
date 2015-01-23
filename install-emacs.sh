set -e

BASE_DIR=$(pwd)

mkdir ~/bin || true

EMACS=emacs-24.4
wget http://mirror.tochlab.net/pub/gnu/emacs/$EMACS.tar.gz
tar -zxvf $EMACS.tar.gz
cd $EMACS
./configure
make
ln -s $BASE_DIR/src/emacs ~/bin/emacs
cd $BASE_DIR

git clone https://github.com/ggreer/the_silver_searcher.git ag
cd ag
./build.sh
ln -s $BASE_DIR/ag/ag ~/bin/ag
cd $BASE_DIR

npm i tern
ln -s $BASE_DIR/node_modules/.bin/tern ~/bin/tern

echo 'todo: ispell'
# wget http://mirror.tochlab.net/pub/gnu/non-gnu/ispell/ispell-3.1.20.tar.gz