:- module(lubm, []).

/** <module> LUBM

@author Wouter Beek
@version 2017/04-2017/05
*/

:- use_module(library(apply)).
:- use_module(library(file_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(os_ext)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(uri)).

:- discontiguous
    ws:query_complexity/3,
    ws:query_html_description//2,
    ws:query_html_title//2,
    ws:query_size/4,
    ws:query_string/3.

:- dynamic
    ws:dataset_file/3,
    ws:query_complexity/3,
    ws:query_html_description//2,
    ws:query_html_title//2,
    ws:query_size/4,
    ws:query_string/3.

:- multifile
    ws:dataset_file/3,
    ws:query_complexity/3,
    ws:query_html_description//2,
    ws:query_html_title//2,
    ws:query_size/4,
    ws:query_string/3.

ws:dataset_file("LUBM", NumUniversities, HdtFile) :-
  atomic_list_concat(['LUBM',NumUniversities], -, Base),
  file_name_extension(Base, hdt, Local),
  (   absolute_file_name(
        data(Local),
        HdtFile,
        [access(read),file_errors(fail)]
      )
  ->  true
  ;   absolute_file_name(lubm('build/lubm.jar'), Jar, [access(read)]),
      absolute_file_name(data(.), Dir, [access(write),file_type(directory)]),
      OntologyUri = 'http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl',
      run_jar(
        Jar,
        [
          '-index', 0,
          '-onto', OntologyUri,
          '-seed', 0,
          '-univ', NumUniversities
        ],
        [Out]>>copy_stream_data(Out, user_output),
        [cwd(Dir)]
      ),
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
      directory_file_path(Dir, 'log.txt', LogFile),
      maplist(delete_file, [LogFile|CleanFiles]),
      finish_ntriples_file(CleanFile, HdtFile, _)
  ).

reserialize0(DirtyFile, CleanFile) :-
  file_name_extension(Base, _, DirtyFile),
  file_name_extension(Base, nt, CleanFile),
  uri_file_name(BaseUri, DirtyFile),
  setup_call_cleanup(
    open(DirtyFile, read, In),
    rdf_reserialize(BaseUri, In, CleanFile),
    close(In)
  ),
  delete_file(DirtyFile).



% Q1 %

ws:query_html_description("LUBM", "Q1") -->
  html(p("This query bears large input and high selectivity.  It queries about just one class and one property and does not assume any hierarchy information or inference.")).
         
ws:query_string("LUBM", "Q1", '\c
prefix ub: <http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
select ?x {
  ?x a ub:GraduateStudent ;
    ub:takesCourse <http://www.Department0.University0.edu/GraduateCourse0>
}').



% Q2 %

ws:query_html_description("LUBM", "Q2") -->
  html(p("This query increases in complexity: 3 classes and 3 properties are involved.  Additionally, there is a triangular pattern of relationships between the objects involved.")).
         
ws:query_string("LUBM", "Q2", '\c
prefix ub: <http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
select ?x ?y ?z {
  ?x a ub:GraduateStudent ;
    ub:undergraduateDegreeFrom ?y ;
    ub:memberOf ?z .
  ?y a ub:University .
  ?z a ub:Department ;
    ub:subOrganizationOf ?y
}').



% Q3 %

ws:query_html_description("LUBM", "Q3") -->
  html(
    p([
      "This query is similar to query LUBM1 but class ",
      code("Publication"),
      " has a wide hierarchy."
    ])
  ).

ws:query_string("LUBM", "Q3", '\c
prefix ub: <http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
select ?x {
  ?x a ub:Publication ;
    ub:publicationAuthor <http://www.Department0.University0.edu/AssistantProfessor0>
}').
