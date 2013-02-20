#!/bin/bash 

DISPLAY=:1.0

xinit $HOME/scripts/steam.sh $* -- :1
