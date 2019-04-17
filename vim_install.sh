#!/bin/sh

echo "================="
echo "Installing neovim"
echo "================="
yay -Sy --needed neovim

# echo "================="
# echo "Installing intero"
# echo "================="
# DIR_BUILD_INTERO=~/.cache/build_intero
# if [ -d "$DIR_BUILD_INTERO" ]; then
#    (cd $DIR_BUILD_INTERO ; git fetch ; git reset --hard origin/master)
# else
#    git clone https://github.com/chrisdone/intero $DIR_BUILD_INTERO
# fi
# (cd $DIR_BUILD_INTERO ; stack build)

echo "==================="
echo "Installing vim-plug"
echo "==================="

curl -s -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

mkdir -p ~/.config/nvim

echo "=============="
echo "Copying config"
echo "=============="
\cp -f init.vim ~/.config/nvim/init.vim


echo "========="
echo "Finished!"
echo "========="

echo ":PlugUpdate" | nvim -e
