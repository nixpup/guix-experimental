#!/usr/bin/env bash

echo "[ Adding Flathub Repo ]"
flatpak --user remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
