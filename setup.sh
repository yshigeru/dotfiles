#!/bin/sh

for i in dot.*; do
    filename=`echo $i | sed 's/^dot//'`
    if [ -h "$HOME/$filename" ]; then
        rm $HOME/$filename
    fi
    ln -s $PWD/$i $HOME/$filename
done

mkdir -p $HOME/.config/fish
if [ -h "$HOME/.config/fish/config.fish" ]; then
    rm "$HOME/.config/fish/config.fish"
fi
ln -s $PWD/config.fish $HOME/.config/fish

exit 0
