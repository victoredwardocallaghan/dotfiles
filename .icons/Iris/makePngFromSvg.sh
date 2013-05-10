#!/bin/sh
# This script will generate png icons in various resolutions
# for people who don't want to use the svg icons.
# Requires rsvg (comes with librsvg)
for z in 12 24 48 96 192 36 72; do
	mkdir ${z}x${z};
	mkdir ${z}x${z}/apps;
	mkdir ${z}x${z}/devices;
	mkdir ${z}x${z}/emblems;
	mkdir ${z}x${z}/filesystems;
	mkdir ${z}x${z}/mimetypes;
done;
for x in `find scalable -type f -name *.svg`; do
	echo "Doing file $x";
	for y in 12 24 48 96 192 36 72; do
		rsvg -w $y -h $y $x `echo $x | sed -e s/^scalable/${y}x${y}/ -e s/.svg$/.png/`;
	done;
done;


