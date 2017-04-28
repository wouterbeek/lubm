lubm_init :-
  source_file(lubm_init, File),
  file_directory_name(File, Dir),
  assert(user:file_search_path(lubm, Dir)).
:- lubm_init.
