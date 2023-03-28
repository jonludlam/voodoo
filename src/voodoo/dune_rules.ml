(* Parse the output of [dune rules] *)

(* Given: 
   
((deps
  ((File
    (In_build_dir
     _build/default/src/.either.objs/byte/either__Either_intf.cmt))))
 (targets
  ((files (install/default/lib/either/either__Either_intf.cmt))
   (directories ())))
 (context default)
 (action
  (symlink
   ../../../../default/src/.either.objs/byte/either__Either_intf.cmt
   _build/install/default/lib/either/either__Either_intf.cmt)))

((deps
  ((File (External /Users/jonathanludlam/devel/voodoo/_opam/bin/ocamlc.opt))
   (File (In_build_dir _build/default/src/.either.objs/byte/either__.cmi))
   (File (In_build_dir _build/default/src/either_intf.ml))))
 (targets
  ((files
    (default/src/.either.objs/byte/either__Either_intf.cmi
     default/src/.either.objs/byte/either__Either_intf.cmo
     default/src/.either.objs/byte/either__Either_intf.cmt))
   (directories ())))
 (context default)
 (action
  (chdir
   _build/default
   (run
    /Users/jonathanludlam/devel/voodoo/_opam/bin/ocamlc.opt
    -w
    -40
    -g
    -bin-annot
    -I
    src/.either.objs/byte
    -no-alias-deps
    -open
    Either__
    -o
    src/.either.objs/byte/either__Either_intf.cmo
    -c
    -impl
    src/either_intf.ml))))

   *)

module M = Map.Make(String)

type rules =
  {
    installed : Fpath.t M.t;
      (* Map from install location to source-tree location. For the above, contains
         a binding mapping "either/either__Either_intf.cmt" to "src/.either.objs/byte/either__Either_intf.cmt" *)
    src : Fpath.t M.t
      (* Map from build tree location to ml file. For the above, contains
         a binding mapping "src/.either.objs/byte/either__Either_intf.cmt" to "src/either_intf.ml" *)
  }

let parse_dep = function
  | Sexplib.Sexp.List [ Atom "File"; List [ Atom "In_build_dir"; Atom b ] ] -> Some (Fpath.v b)
  | List [ Atom "File"; List _ ] -> (* Not in build dir, this is OK *) None
  | List ( Atom "File" :: _) as s -> Format.eprintf "parse_dep: failure 1\n%a\n%!" Sexplib.Sexp.pp_hum s; None
  | List ( Atom "glob" :: _) -> (* Fine, external glob *) None
  | List _ as s -> Format.eprintf "parse_dep: failure 2\n%a\n%!" Sexplib.Sexp.pp_hum s; None
  | s -> Format.eprintf "parse_dep: failure 3\n%a\n%!"  Sexplib.Sexp.pp_hum s; None

let parse_deps = function
  | [ Sexplib.Sexp.List l ] -> List.filter_map parse_dep l
  | s ->
    Format.eprintf "parse_deps: failure 1\n%a\n%!" Sexplib.Sexp.pp_hum (List s); []

let parse_files = function
    | Sexplib.Sexp.List [Atom "files"; List l] -> Some (List.filter_map (function | Sexplib.Sexp.Atom x -> Some (Fpath.v x) | _ -> None) l)
    | Sexplib.Sexp.List [Atom "directories"; List _] -> None (* This is fine *)
    | s ->
      Format.eprintf "parse_files: failure 1\n%a\n%!" Sexplib.Sexp.pp_hum s;
      None

let parse_targets = function
  | [ Sexplib.Sexp.List l ] -> List.filter_map parse_files l |> List.flatten
  | s -> Format.eprintf "parse_targets: failure 1\n%a\n%!" Sexplib.Sexp.pp_hum (List s); []

let read : Fpath.t -> _ = fun path ->
  let open Sexplib.Sexp in
  let init = { installed = M.empty; src = M.empty } in
  let p =
    match Dune.sexp_of_file path with
    | Ok p -> p
    | Error (`Msg m) -> Format.eprintf "Failed to read '%a' (%s)\n%!" Fpath.pp path m; Sexplib.Sexp.List []
  in
  let rs = match p with | List l -> l | _ -> [] in
  let vs = List.map (fun r ->
    Result.value ~default:[] (Dune.assoc_list r)) rs in
  List.fold_left (fun acc r ->
    try
      let deps = List.assoc "deps" r |> parse_deps in
      let targets = List.assoc "targets" r |> parse_targets in
      let is_install_rule =
        List.exists (fun target -> Fpath.is_prefix (Fpath.v "install/default") target) targets
      in
      if is_install_rule
      then
        (Format.eprintf "install rule\n%!";
         match targets, deps with
         | [ target ], [dep] ->
          let t = Fpath.(relativize ~root:(Fpath.v "install/default") target) |> Option.get in
          let d = Fpath.(relativize ~root:(Fpath.v "_build/default") dep) |> Option.get in
          {acc with installed = M.add (Fpath.to_string t) d acc.installed}
         | _, _ ->
          Format.eprintf "install rule with unexpected number of deps/targets\n%a\n%a\n%!" Sexplib.Sexp.pp_hum (List (List.assoc "deps" r)) Sexplib.Sexp.pp_hum (List (List.assoc "targets" r)); acc)
      else
        (Format.eprintf "build rule\ntarget=%a\n%!" Fpath.pp (List.hd targets);
         let cmt_target = List.find (Fpath.has_ext ".cmt") targets in
         let cmt_target = Fpath.relativize ~root:(Fpath.v "default") cmt_target |> Option.get in
         let ml_deps = List.find_all (fun p -> Fpath.has_ext ".ml" p || Fpath.has_ext ".ml-gen" p) deps in
         match ml_deps with
         | [ ml_dep ] ->
          let ml_dep = Fpath.relativize ~root:(Fpath.v "_build/default") ml_dep |> Option.get in
          {acc with src = M.add (Fpath.to_string cmt_target) ml_dep acc.src}
         | _ ->
            Format.eprintf "Unhandled, multiple ml files for a cmt dependency found: %a" (Format.pp_print_list ~pp_sep:(Format.pp_print_space) Fpath.pp) ml_deps;
          acc)
      
    with _ -> acc
    ) init vs
  
let ml_of_cmt t cmt_path =
  try
    let build_path = M.find cmt_path t.installed in
    let src_path = M.find (Fpath.to_string build_path) t.src in
    Some src_path
  with _ -> 
    None
