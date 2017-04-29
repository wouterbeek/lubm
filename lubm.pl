:- module(lubm, []).

/** <module> LUBM

@author Wouter Beek
@version 2017/04
*/

:- use_module(library(apply)).
:- use_module(library(file_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(uri)).

:- discontiguous
    websql:query_complexity/3,
    websql:query_html_description//2,
    websql:query_html_title//2,
    websql:query_size/4,
    websql:query_string/3.

:- dynamic
    websql:dataset_file/3,
    websql:query_complexity/3,
    websql:query_html_description//2,
    websql:query_html_title//2,
    websql:query_size/4,
    websql:query_string/3.

:- multifile
    websql:dataset_file/3,
    websql:query_complexity/3,
    websql:query_html_description//2,
    websql:query_html_title//2,
    websql:query_size/4,
    websql:query_string/3.

% Q2 %

websql:query_html_description("LUBM", "Q2") -->
  html(p("This query increases in complexity: 3 classes and 3 properties are involved.  Additionally, there is a triangular pattern of relationships between the objects involved.")).
         
websql:query_string("LUBM", "Q2", '\c
prefix ub: <http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
select ?x ?y ?z {
  ?x a ub:GraduateStudent ;
    ub:undergraduateDegreeFrom ?y ;
    ub:memberOf ?z .
  ?y a ub:University .
  ?z a ub:Department ;
    ub:subOrganizationOf ?y
}').



% LUBM %
% Q3 %

websql:dataset_file("LUBM", NumUniversities, HdtFile) :-
  atomic_list_concat(['LUBM',NumUniversities], -, Base),
  file_name_extension(Base, hdt, Local),
  (   absolute_file_name(
        data(Local),
        HdtFile,
        [access(read),file_errors(fail)]
      )
  ->  true
  ;   absolute_file_name(lubm('run.sh'), Program, [access(execute)]),
      absolute_file_name(lubm('univ-bench.owl'), OntologyFile, [access(read)]),
      uri_file_name(OntologyUri, OntologyFile),
      process_create(Program, [0,OntologyUri,0,NumUniversities], []),
      absolute_file_name(lubm(.), Dir, [access(read),file_type(directory)]),
      directory_file_path(Dir, 'University*.owl', Spec),
      expand_file_name(Spec, DirtyFiles),
      findall(
        CleanFile,
        (
          member(DirtyFile, DirtyFiles),
          reserialize0(DirtyFile, CleanFile)
        ),
        CleanFiles
      ),
      file_name_extension(Base, nt, CleanLocal),
      absolute_file_name(data(CleanLocal), CleanFile, [access(write)]),
      concatenate_files(CleanFiles, CleanFile),
      maplist(delete_file, CleanFiles),
      finish_ntriples_file(CleanFile, HdtFile, _)
websql:query_html_description("LUBM", "Q3") -->
  html(
    p([
      "This query is similar to query LUBM1 but class ",
      code("Publication"),
      " has a wide hierarchy."
    ])
  ).

reserialize0(DirtyFile1, CleanFile) :-
  file_base_name(DirtyFile1, DirtyLocal),
  absolute_file_name(data(DirtyLocal), DirtyFile2, [access(write)]),
  rename_file(DirtyFile1, DirtyFile2),
  file_name_extension(Base, _, DirtyFile2),
  file_name_extension(Base, nt, CleanFile),
  uri_file_name(BaseUri, DirtyFile2),
  setup_call_cleanup(
    open(DirtyFile2, read, In),
    rdf_reserialize(BaseUri, In, CleanFile),
    close(In)
  ),
  delete_file(DirtyFile2),
  writeln(CleanFile).





% Q1 %

websql:query_html_description("LUBM", "Q1") -->
  html(p("This query bears large input and high selectivity.  It queries about just one class and one property and does not assume any hierarchy information or inference.")).
         
websql:query_size("SP²B", "Q1",    10000, 1).
websql:query_size("SP²B", "Q1",    50000, 1).
websql:query_size("SP²B", "Q1",   250000, 1).
websql:query_size("SP²B", "Q1",  1000000, 1).
websql:query_size("SP²B", "Q1",  5000000, 1).
websql:query_size("SP²B", "Q1", 25000000, 1).

websql:query_string("LUBM", "Q1", '\c
websql:query_string("LUBM", "Q3", '\c
prefix ub: <http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
select ?x {
  ?x a ub:GraduateStudent ;
    ub:takesCourse <http://www.Department0.University0.edu/GraduateCourse0>
  ?x a ub:Publication ;
    ub:publicationAuthor <http://www.Department0.University0.edu/AssistantProfessor0>
}').



