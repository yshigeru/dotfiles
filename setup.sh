#!/bin/sh

for i in dot.*; do
    filename=`echo $i | sed 's/^dot//'`
    ln -s $PWD/$i $HOME/$filename
done
