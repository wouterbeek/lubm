:- use_module(library(apply)).
:- use_module(library(process)).
:- use_module(library(settings)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(conf_ext)).
:- use_module(library(file_ext)).
:- use_module(library(stream_ext)).
:- use_module(library(sw/rdf_deref)).
:- use_module(library(sw/rdf_export)).

:- initialization
   init_script.

:- setting(data_directory, any, _, "").

%! run(NumberOfUniversities:nonneg) is det.

run(N) :-
  absolute_file_name('build/lubm.jar', JarFile, [access(read)]),
  absolute_file_name('univ-bench.owl', SchemaFile, [access(read)]),
  setting(data_directory, Dir),
  process_create(
    path(java),
    ['-jar',file(JarFile),'-onto',file(SchemaFile),'-univ',N],
    [cwd(Dir)]
  ),
  directory_file_path(Dir, '*.owl', Wildcard),
  expand_file_name(Wildcard, OutFiles),
  absolute_file_name('lubm.nt.gz', OutFile, [access(write),relative_to(Dir)]),
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
  ),
  maplist(delete_file, OutFiles).

init_script :-
  conf_json(Conf),
  create_directory(Conf.'data-directory'),
  set_setting(data_directory, Conf.'data-directory').
