(* Prep *)

type actions = {
  copy : (Fpath.t * Fpath.t) list;
  info : Fpath.t list;
  objinfo : Fpath.t list;
}

let path_of_pkg =
  let paths : (string, string) Hashtbl.t = Hashtbl.create 100 in
  let findlib_dest =
    lazy
      (Util.lines_of_process Bos.Cmd.(v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "printconf" % "destdir")
      |> List.hd)
  in
  fun pkg ->
    if Hashtbl.mem paths pkg then Hashtbl.find paths pkg
    else
      let pkg_path =
        Util.lines_of_process Bos.Cmd.(v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "query" % pkg) |> List.hd
      in
      let prefix = Lazy.force findlib_dest in
      assert (Astring.String.is_prefix ~affix:prefix pkg_path);
      let rel_path =
        String.sub pkg_path (String.length prefix)
          (String.length pkg_path - String.length prefix)
      in
      Hashtbl.replace paths pkg rel_path;
      rel_path

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
      process_ocaml_artefacts
      && List.mem ext [ ".cmt"; ".cmti"; ".cmi" ]
      && not has_hyphen
    in
    let is_cma = ext = ".cma" in
    let is_js = ext = ".js" in
    let do_copy =
      (not in_build_dir)
      && (is_in_doc_dir || is_module || is_cma || is_js
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
      Util.cp src dst)
    actions.copy;
  List.iter
    (fun fpath ->
      let lines =
        Util.lines_of_process
          Bos.Cmd.(v "ocamlobjinfo" % Fpath.(to_string (root // fpath)))
      in
      Util.write_file Fpath.(dest // set_ext "ocamlobjinfo" fpath) lines)
    actions.objinfo

let do_jsoo root pkg_contents findlib_cmas stdlib_cmis =
  List.iter
    (fun (package, ty, contents) ->
      let cmas =
        if ty = `Prep then
          List.filter (fun p -> Fpath.has_ext ".cma" p) contents
        else []
      in
      let findlib_pkgs =
        List.filter_map
          (fun p ->
            try Some (p, List.assoc Fpath.(root // p) findlib_cmas)
            with Not_found ->
              Format.eprintf "XXX No findlib pkg for archive %s\n%!"
                (Fpath.to_string p);
              None)
          cmas
      in
      List.iter
        (fun (cma_fpath, findlib_pkg) ->
          let cma = Jsoo.Cma.{ cma_fpath; package } in
          Jsoo.Cma.marshal cma;
          let include_path_cmd =
            Bos.Cmd.(
              v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "query" % "-predicates" % "byte,javascript"
              % "-i-format" % "-r" % findlib_pkg)
          in
          let include_path = Util.lines_of_process include_path_cmd in
          let include_path =
            List.map
              (fun f ->
                match Astring.String.fields f with
                | [ "-I"; path ] -> path ^ "/"
                | _ -> assert false)
              include_path
          in
          Format.eprintf "Include paths: [%s]\n%!"
            (String.concat ";" include_path);
          let cmis =
            List.concat
              (List.map
                 (fun (package, _, contents) ->
                   List.filter_map
                     (fun path ->
                       let dir, file = Fpath.split_base path in
                       if
                         Fpath.has_ext ".cmi" file
                         && List.mem
                              Fpath.(to_string (root // dir))
                              include_path
                       then Some Jsoo.Cmi.{ package; path }
                       else None)
                     contents)
                 pkg_contents)
          in
          let cmis = cmis @ stdlib_cmis in

          let js_files_cmd =
            Bos.Cmd.(
              v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "query" % "-predicates" % "byte,javascript"
              % "-o-format" % "-r" % findlib_pkg)
          in
          let js_files = Util.lines_of_process js_files_cmd in
          let js_files =
            List.map
              (fun s ->
                let universe, pkg, version = package in
                match s.[0] with
                | '-' ->
                    Format.eprintf "Ignoring jsoo option passed: %s\n%!" s;
                    None
                | '+' -> Some (String.sub s 1 (String.length s - 1) |> Fpath.v)
                | _ ->
                    Format.eprintf "Bad js file for: (%s,%s,%s) (%s)\n%!"
                      universe pkg version s;
                    failwith "Bad package")
              js_files
          in
          let js_files =
            List.fold_right
              (fun f acc -> match f with Some x -> x :: acc | None -> acc)
              js_files []
          in

          let deps_cmd =
            Bos.Cmd.(
              v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "query" % "-predicates" % "byte,javascript"
              % "-a-format" % "-r" % findlib_pkg)
          in
          let deps = Util.lines_of_process deps_cmd in
          let dep_cmas =
            List.filter_map
              (fun cma_path ->
                match
                  List.find_opt
                    (fun (_, _, contents) ->
                      List.exists
                        (fun p -> Fpath.(to_string (root // p)) = cma_path)
                        contents)
                    pkg_contents
                with
                | Some (pkg, _, _) ->
                    let cma =
                      { Jsoo.Cma.package = pkg; cma_fpath = Fpath.v cma_path }
                    in
                    Some cma
                | None ->
                    Format.eprintf "Failed to find package containing %s\n%!"
                      cma_path;
                    None)
              deps
          in

          let toplevel =
            Jsoo.Toplevel.
              { package; cma_fpath; js_files; dep_cmas; cmis; digest = None }
          in
          Jsoo.Toplevel.build_toplevel toplevel;
          Jsoo.Toplevel.copy_js_files root toplevel;
          Jsoo.Toplevel.marshal toplevel)
        findlib_pkgs)
    pkg_contents

type pkg_kind = [ `Prep | `Reference ]

let run _ (universes : (string * string) list)
    (extra_universes : (string * string) list) =
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
          Some ((universe, pkg.Opam.name, pkg.version), `Prep)
    | _ -> (
        fun pkg ->
          try
            Some
              ( (List.assoc pkg.Opam.name universes, pkg.name, pkg.version),
                `Prep )
          with Not_found -> (
            try
              Some
                ( ( List.assoc pkg.Opam.name extra_universes,
                    pkg.name,
                    pkg.version ),
                  `Reference )
            with Not_found -> None))
  in

  let packages =
    Opam.all_opam_packages ()
    |> List.fold_left
         (fun acc pkg ->
           match get_universe pkg with Some pkg -> pkg :: acc | None -> acc)
         []
  in
  let findlib_pkgs =
    Util.lines_of_process Bos.Cmd.(v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "list")
    |> List.map (fun line -> Astring.String.fields line |> List.hd)
  in
  List.iter (fun i -> Format.eprintf "findlib_pkg: %s\n%!" i) findlib_pkgs;

  let findlib_cmas =
    List.filter_map
      (fun pkg ->
        try
          let cmd =
            Bos.Cmd.(
              v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "query" % "-predicates" % "byte,javascript"
              % "-a-format" % pkg)
          in
          let exec_answer = Util.lines_of_process cmd |> List.hd in
          Some (Fpath.v exec_answer, pkg)
        with _ -> None)
      findlib_pkgs
  in
  let stdlib_cmd = Bos.Cmd.(v "opam" % "exec" % ("--switch="^Opam.get_switch()) % "--" % "ocamlfind" % "printconf" % "stdlib") in
  let stdlib = Util.lines_of_process stdlib_cmd |> List.hd in
  let opam_prefix = Opam.prefix () |> Fpath.v in
  let stdlib_cmis =
    match Bos.OS.Dir.fold_contents
    (fun path acc ->
      if Fpath.has_ext "cmi" path then
        match Fpath.relativize ~root:opam_prefix path
      with | Some p -> p::acc
    | None -> acc else acc) [] Fpath.(v stdlib) with
    | Ok cmis -> cmis
    | Error (`Msg m) ->
      failwith m
  in
  let stdlib_pkg =
    let pkg,_ = List.find (fun ((_,n,_),_) -> n="ocaml-base-compiler") packages in
    pkg
  in
  let stdlib_cmis = List.map (fun cmi -> Jsoo.Cmi.{ package=stdlib_pkg; path=cmi }) stdlib_cmis in
  let root = Opam.prefix () |> Fpath.v in
  let pkg_contents =
    List.map
      (fun (((_, pkg_name, _) as package), ty) ->
        (package, ty, Opam.pkg_contents pkg_name))
      packages
  in
  List.iter
    (fun (package, ty, files) ->
      if ty = `Prep then process_package root package files)
    pkg_contents;
  List.iter
    (fun (package, ty) ->
      let _, name, version = package in
      match (Opam.opam_file name version, ty) with
      | Some lines, `Prep ->
          let dest = Package.prep_path package in
          Util.write_file Fpath.(dest / "opam") lines
      | _, _ -> ())
    packages;

  do_jsoo root pkg_contents findlib_cmas stdlib_cmis
