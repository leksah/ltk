#!/bin/sh -ex
sudo apt-get update -qq
sudo apt-get --no-install-recommends install darcs
darcs get --lazy http://patch-tag.com/r/hamish/gtk2hs
cd gtk2hs
sh bootstrap.sh -fgtk3

