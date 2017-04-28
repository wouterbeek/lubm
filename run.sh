#!/bin/bash
java \
    -cp classes/ \
    edu.lehigh.swat.bench.uba.Generator \
    -index "$1" \
    -onto "$2" \
    -seed "$3" \
    -univ "$4"
