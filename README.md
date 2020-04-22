# Copy of the Lehigh University Benchmark (LUBM)

This is a copy of the benchmark published at
<http://swat.cse.lehigh.edu/projects/lubm/>.

This copy is cleaned to be compliant with SPARQL 1.1 syntax, has
correct out-of-the-box behavior on Linux.

## Installation

  1. Install [[https://ant.apache.org][Apache Ant]],
     [[https://www.gnu.org/software/gzip/][GNU zip]], and
     [[http://librdf.org/raptor/][Raptor]] (available in most package
     managers).

  2. Clone this repository:

     ```sh
     git clone https://github.com/wouterbeek/lubm
     ```

  3. Build it with Ant:

     ```sh
     cd lubm && ant -f build.xml
     ```

## Use

Create a dataset:

```sh
$ ./run.sh 1 out.nt.gz
```
