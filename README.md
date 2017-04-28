# Copy of the Lehigh University Benchmark (LUBM)

This is a copy of the benchmark published at
http://swat.cse.lehigh.edu/projects/lubm/

This copy is cleaned to be compliant with SPARQL 1.1 syntax, has
correct out-of-the-box behavior on Linux, and includes a SWI-Prolog
based API for generating & querying datasets from this benchmark.

## Build the Data Generator (UBA 1.7)

```sh
apt install ant
ant -f build.xml
```

## Generate a dataset

```sh
./run.sh
```
