:- use_module(library(apply)).
:- use_module(library(process)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(stream_ext)).
:- use_module(library(sw/rdf_deref)).
:- use_module(library(sw/rdf_export)).

%! run(NumberOfUniversities:nonneg) is det.

run(N) :-
  absolute_file_name('build/lubm.jar', JarFile, [access(read)]),
  absolute_file_name('univ-bench.owl', SchemaFile, [access(read)]),
  process_create(
    path(java),
    ['-jar',file(JarFile),'-onto',file(SchemaFile),'-univ',N],
    [cwd(out)]
  ),
  expand_file_name('out/*.owl', OutFiles),
  absolute_file_name('out/lubm.nt.gz', OutFile, [access(write)]),
  setup_call_cleanup(
    gzopen(OutFile, write, Out),
    maplist(
      {Out}/[File]>>rdf_deref_file(
                      File,
                      {Out}/[BNodePrefix,Triples,_]>>maplist(
                                                      rdf_write_triple(Out, BNodePrefix),
                                                      Triples
                                                    )
                    ),
      OutFiles
    ),
    close(Out)
  ).
