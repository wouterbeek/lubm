#!/bin/sh

java -jar build/lubm.jar -onto data/univ-bench.owl -univ $1
for i in *.owl; do
    [ -f "$i" ] || break
    rapper -i rdfxml -o ntriples $i | gzip > $2
    rm $i
done
