:- use_module(library(apply)).
:- use_module(library(process)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(zlib)).

:- use_module(library(conf_ext)).
:- use_module(library(file_ext)).
:- use_module(library(stream_ext)).
:- use_module(library(semweb/rdf_deref)).
:- use_module(library(semweb/rdf_export)).

:- initialization
   init_script.

:- setting(data_directory, any, _, "").

%! run(+NumberOfUniversities:nonneg, +File:atom) is det.

run(N, NtFile) :-
  absolute_file_name('../build/lubm.jar', JarFile, [access(read)]),
  absolute_file_name('../data/univ-bench.owl', SchemaFile, [access(read)]),
  setting(data_directory, Dir),
  uri_file_name(Uri, SchemaFile),
  process_create(
    path(java),
    ['-jar',file(JarFile),'-onto',Uri,'-univ',N],
    [cwd(Dir)]
  ),
  directory_file_path(Dir, '*.owl', Wildcard),
  expand_file_name(Wildcard, XmlFiles),
  format(atom(Local), 'lubm-~d.nt.gz', [N]),
  directory_file_path(Dir, Local, NtFile),
  setup_call_cleanup(
    gzopen(NtFile, write, Out),
    maplist(process_xml_file(Out), XmlFiles),
    close(Out)
  ).

process_xml_file(Out, XmlFile) :-
  rdf_deref_file(XmlFile, process_xml_triples(Out)),
  delete_file(XmlFile).

process_xml_triples(Out, BNodePrefix, Triples, _) :-
  maplist(rdf_write_triple(Out, BNodePrefix), Triples).

init_script :-
  conf_json(Conf),
  create_directory(Conf.'data-directory'),
  set_setting(data_directory, Conf.'data-directory').
