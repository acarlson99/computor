#!/bin/sh


find src/ app/ -name '*.hs' -type f -exec brittany --indent=4 --write-mode=inplace {} ';'
