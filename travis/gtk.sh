#!/bin/sh -ex
darcs get --lazy http://patch-tag.com/r/hamish/gtk2hs
cd gtk2hs
sh bootstrap.sh -fgtk3

