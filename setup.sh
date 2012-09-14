#!/bin/sh

for i in dot.*; do
    filename=`echo $i | sed 's/^dot//'`
    if [ -e "$HOME/$filename" ]; then
        rm -rf $HOME/$filename
    fi
    ln -s $PWD/$i $HOME/$filename
done
