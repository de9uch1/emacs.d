#!/bin/bash

function install_nerd() {
    curl -o "$HOME/.local/share/fonts/NFM.ttf" "https://github.com/rainstormstudio/nerd-icons.el/raw/refs/heads/main/fonts/NFM.ttf"
}

function install_plemoljp(){
    curl -L -o "$HOME/.local/share/fonts/PlemolJP_NF_v3.0.0.zip" "https://github.com/yuru7/PlemolJP/releases/download/v3.0.0/PlemolJP_NF_v3.0.0.zip"
    pushd "$HOME/.local/share/fonts"
    unzip "./PlemolJP_NF_*.zip"
    popd
}

mkdir -p $HOME/.local/share/fonts
install_nerd
install_plemoljp
fc-cache -frv
