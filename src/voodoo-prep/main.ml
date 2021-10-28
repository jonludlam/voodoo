(* Prep *)

type actions = {
  copy : (Fpath.t * Fpath.t) list;
  info : Fpath.t list;
  objinfo : Fpath.t list;
}

let process_package : Fpath.t -> Package.t -> Fpath.t list -> unit =
 fun root package files ->
  let dest = Package.prep_path package in

  (* Some packages produce ocaml artefacts that can't be processed with the switch's
      ocaml compiler - most notably the secondary compiler! This switch is intended to
      be used to ignore those files *)
  let process_ocaml_artefacts =
    let _, name, _ = package in
    let package_blacklist = [ "ocaml-secondary-compiler" ] in
    not (List.mem name package_blacklist)
  in

  let foldfn fpath acc =
    let is_in_doc_dir =
      match Fpath.segs fpath with "doc" :: _ -> true | _ -> false
    in

    (* Menhir puts a dune build dir into docs for some reason *)
    let in_build_dir = List.exists (fun x -> x = "_build") (Fpath.segs fpath) in

    let _, filename = Fpath.split_base fpath in
    let ext = Fpath.get_ext filename in
    let no_ext = Fpath.rem_ext filename in
    let has_hyphen = String.contains (Fpath.to_string filename) '-' in
    let is_module =
      process_ocaml_artefacts && List.mem ext [ ".cmt"; ".cmti"; ".cmi" ] && not has_hyphen
    in
    let do_copy =
      (not in_build_dir)
      && (is_in_doc_dir || is_module
         || List.mem no_ext (List.map Fpath.v [ "META"; "dune-package" ]))
    in
    let is_cma = process_ocaml_artefacts && List.mem ext [ ".cma"; ".cmxa" ] in
    let copy =
      if do_copy then Fpath.(root // fpath, dest // fpath) :: acc.copy
      else acc.copy
    in
    let info = if is_module then fpath :: acc.info else acc.info in
    let objinfo = if is_cma then fpath :: acc.objinfo else acc.objinfo in
    { copy; info; objinfo }
  in
  let actions =
    List.fold_right foldfn files { copy = []; info = []; objinfo = [] }
  in
  List.iter
    (fun (src, dst) ->
      let dir, _ = Fpath.split_base dst in
      Util.mkdir_p dir;
      Util.cp (Fpath.to_string src) (Fpath.to_string dst))
    actions.copy;
  List.iter
    (fun fpath ->
      let lines =
        Util.lines_of_process
          Bos.Cmd.(v "ocamlobjinfo" % Fpath.(to_string (root // fpath)))
      in
      Util.write_file Fpath.(dest // set_ext "ocamlobjinfo" fpath) lines)
    actions.objinfo

let do_jsoo root pkg_contents findlib_cmas =
  let open Bos in
  List.iter (fun (package, contents) ->
    let dest = Package.prep_path package in
    let cmas = List.filter (fun p -> Fpath.has_ext ".cma" p) contents in
    let findlib_pkgs = List.filter_map (fun p -> try Some (p, List.assoc Fpath.(root // p) findlib_cmas) with Not_found ->
      Format.eprintf "XXX No findlib pkg for archive %s\n%!" (Fpath.to_string p); None ) cmas in
    let jsoo = Bos.Cmd.v "js_of_ocaml" in
    let jsoo_mkcmis = Bos.Cmd.v "jsoo_mkcmis" in
    List.iter (fun (cma_fpath, findlib_pkg) ->
      let name = Fpath.basename cma_fpath in
      let cmd = Cmd.(jsoo % Fpath.(to_string (root // cma_fpath)) % "-o" % Fpath.(to_string (dest / (name ^ ".js") ))) in
      ignore (Util.lines_of_process cmd);
      let cmd = Cmd.(jsoo_mkcmis % findlib_pkg % "-o" % Fpath.(to_string (dest / (name ^ ".cmis.js") ))) in
      ignore (Util.lines_of_process cmd);
      let deps = 
        try
          Findlib.package_deep_ancestors ["byte";"javascript"] [findlib_pkg] |>
          List.filter_map (fun p -> try Some (Findlib.package_directory p ^ "/" ^ Findlib.package_property ["byte";"javascript"] p "archive") with _ -> None) 
        with _ -> []
      in
      let opam_packages =
        List.filter_map (fun cma_path ->
          match List.find_opt (fun (_, contents) -> List.exists (fun p -> Fpath.(to_string (root // p)) = cma_path) contents) pkg_contents with
          | Some (pkg, _) -> Some (pkg, cma_path)
          | None -> Format.eprintf "Failed to find package containing %s\n%!" cma_path;
          None) deps
      in
      Util.write_file Fpath.(dest / "jsoo.deps") (List.map (fun ((universe, name, version), cma_path) ->
        Format.sprintf "%s %s %s %s" universe name version (Fpath.basename (Fpath.v cma_path))) opam_packages)

      ) findlib_pkgs
     ) pkg_contents

let run _ (universes : (string * string) list) =
  let get_universe =
    match universes with
    | [] ->
        let id = ref 0 in
        Printf.eprintf
          "Warning: No universes have been specified: will generate dummy \
           universes\n\
           %!";
        fun pkg ->
          let universe = string_of_int !id in
          incr id;
          Some (universe, pkg.Opam.name, pkg.version)
    | _ -> (
        fun pkg ->
          try Some (List.assoc pkg.Opam.name universes, pkg.name, pkg.version)
          with _ -> None)
  in

  let packages =
    Opam.all_opam_packages ()
    |> List.fold_left
         (fun acc pkg ->
           match get_universe pkg with Some pkg -> pkg :: acc | None -> acc)
         []
  in

  let findlib_cmas =
    List.filter_map (fun pkg ->
      try
        let archives = Findlib.package_property ["byte"; "javascript"] pkg "archive" in
        let path = Findlib.package_directory pkg in
        Some (Fpath.v (Printf.sprintf "%s/%s" path archives), pkg)
      with _ -> None) (Findlib.list_packages' ()) in
  let root = Opam.prefix () |> Fpath.v in
  let pkg_contents =
    List.map
      (fun ((_, pkg_name, _) as package) ->
        (package, Opam.pkg_contents pkg_name))
      packages
  in
  List.iter
    (fun (package, files) -> process_package root package files)
    pkg_contents;
  List.iter
    (fun package ->
      let _, name, version = package in
      match Opam.opam_file name version with
      | Some lines ->
          let dest = Package.prep_path package in
          Util.write_file Fpath.(dest / "opam") lines
      | None -> ())
    packages;
  do_jsoo root pkg_contents findlib_cmas
  
