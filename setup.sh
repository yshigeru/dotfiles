#!/bin/sh

for i in dot.*; do
    filename=`echo $i | sed 's/^dot//'`
    if [ -h "$HOME/$filename" ]; then
        rm $HOME/$filename
    fi
    ln -s $PWD/$i $HOME/$filename
done
