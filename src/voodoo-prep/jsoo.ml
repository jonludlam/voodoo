module StringSet = Set.Make(String)

module Cma = struct
  type t = { cma_fpath : Fpath.t; package : Package.t }

  let name v = Fpath.basename v.cma_fpath

  let func_name vv =
    let n = name vv in
    Astring.String.cuts ~sep:"." n |> String.concat "_dot_"

  let pp fmt v =
    Format.fprintf fmt "{ cma_fpath: %a; package: %a }" Fpath.pp v.cma_fpath
      Package.pp v.package

  let ext = ".jscma"

  let cma_path vv = Fpath.(Package.prep_path vv.package // vv.cma_fpath)

  let meta_path vv =
    Fpath.(Package.prep_path vv.package // add_ext ext (v (name vv)))

  let output_path vv =
    Fpath.(Package.jsoo_path vv.package // add_ext ".js" (v (name vv)))

  let server_path vv = Fpath.(v "/toplevels" // output_path vv)

  let marshal v =
    let path = meta_path v in
    let oc = open_out_bin (Fpath.to_string path) in
    Marshal.to_channel oc v [];
    close_out oc

  let unmarshal path : t =
    let ic = open_in_bin path in
    let result = Marshal.from_channel ic in
    close_in ic;
    result

  let cmd (vv : t) =
    Bos.Cmd.(
      v "js_of_ocaml"
      % Fpath.(cma_path vv |> to_string)
      % "--pretty" % "--wrap-with-fun" % func_name vv % "-o"
      % Fpath.(output_path vv |> to_string))
end

module Cmi = struct
  type t = { package : Package.t; path : Fpath.t }

  let prep_path vv = Fpath.(Package.prep_path vv.package // vv.path)

  let output_path vv = Fpath.(Package.jsoo_path vv.package // vv.path)

  let server_path vv = Fpath.(v "/toplevels" // output_path vv)

  let pp fmt v =
    Format.fprintf fmt "{ package: %a; path: %a }" Package.pp v.package Fpath.pp
      v.path

  let compare v1 v2 =
    let s1 = Format.asprintf "%a" pp v1 in
    let s2 = Format.asprintf "%a" pp v2 in
    String.compare s1 s2

  let copy universes cmis =
    let cmis = List.filter (fun cmi ->
      let (uid, _, _) = cmi.package in
      StringSet.mem uid universes) cmis in
    let l = List.map (fun cmi -> (prep_path cmi, output_path cmi)) cmis in
    let dst_dirs =
      List.map
        (fun (_, f) ->
          let dir, _ = Fpath.split_base f in
          dir)
        l
      |> List.sort_uniq Fpath.compare
    in
    List.iter Util.mkdir_p dst_dirs;
    List.iter (fun (src, dst) -> Util.cp src dst) l
end

module Toplevel = struct
  type t = {
    package : Package.t;
    cma_fpath : Fpath.t;
    dep_cmas : Cma.t list;
    cmis : Cmi.t list;
    js_files : Fpath.t list;
    digest : Digest.t option;
  }

  let meta_ext = ".jstop"

  let bc_exe_name vv =
    Printf.sprintf "top_worker_main_%s.bc" Fpath.(basename vv.cma_fpath)

  let js_name vv = Fpath.basename vv.cma_fpath ^ ".worker.js"

  let meta_name vv = js_name vv ^ meta_ext

  let pp fmt v =
    Format.fprintf fmt "{ package: %a; bc_exe_fpath: %a; js_files: [%a] }"
      Package.pp v.package Fpath.pp v.cma_fpath
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
         Fpath.pp)
      v.js_files

  let meta_path vv = Fpath.(Package.prep_path vv.package / meta_name vv)

  let output_path vv = Fpath.(Package.jsoo_path vv.package / js_name vv)

  let bc_exe_path vv = Fpath.(Package.prep_path vv.package / bc_exe_name vv)

  let js_dest_path vv = Fpath.(Package.prep_path vv.package / "voodoo_js_files")

  let digested_path vv =
    match vv.digest with
    | Some digest -> Fpath.(Paths.jsoo / "toplevel-by-digest" / digest / "toplevel.js")
    | None -> failwith "No digest"

  let marshal v =
    let path = meta_path v in
    let oc = open_out_bin (Fpath.to_string path) in
    Marshal.to_channel oc v [];
    close_out oc

  let unmarshal path : t =
    let ic = open_in_bin path in
    let result = Marshal.from_channel ic in
    close_in ic;
    result

  let toplevel_ml _ =
    [ "let () = Js_top_worker.Worker.run ()" ]

  let build_toplevel vv =
    let path = Fpath.(Package.prep_path vv.package / "top_worker_main.ml") in
    Util.write_file path (toplevel_ml vv);
    let bc_exe_fpath = bc_exe_path vv in
    let cmd =
      Bos.Cmd.(
        v "ocamlfind" % "ocamlc" % "-package" % "js_top_worker"
        % Fpath.(to_string path)
        % "-linkpkg" % "-linkall" % "-o"
        % Fpath.(to_string bc_exe_fpath))
    in
    Format.eprintf "toplevel bc: %a\n%!" Bos.Cmd.pp cmd;
    ignore (Util.lines_of_process cmd)

  let copy_js_files root vv =
    let dest_path = js_dest_path vv in
    Util.mkdir_p dest_path;
    List.iter (fun js ->
      let src = Fpath.(root // v "lib" // js) in
      let dst = Fpath.(dest_path // js) in
      let (dir, _) = Fpath.split_base dst in
      Util.mkdir_p dir;
      Util.cp src dst) vv.js_files

  let cmd (vv : t) =
    let cmd = Bos.Cmd.(v "js_of_ocaml" % "--toplevel" % "--pretty" % "--no-cmis") in
    let js f = Fpath.(js_dest_path vv // f |> to_string) in
    let cmd =
      List.fold_right
        (fun a cmd -> Bos.Cmd.(cmd % a))
        (List.map js vv.js_files
        @ [ "+toplevel.js"; "+dynlink.js" ])
        cmd
    in
    let cmd =
      Bos.Cmd.(
        cmd
        % Fpath.(to_string (bc_exe_path vv))
        % "-o"
        % Fpath.(to_string (output_path vv)))
    in
    cmd
end

let handle_cma cma_fpath =
  let cma = Cma.unmarshal (Fpath.to_string cma_fpath) in
  let cmd = Cma.cmd cma in
  let dir = fst (Fpath.split_base (Cma.output_path cma)) in
  Util.mkdir_p dir;
  Format.printf "Cma: %a\n%!" Bos.Cmd.pp cmd;
  ignore @@ Util.lines_of_process cmd;
  None

let handle_toplevel toplevel_fpath =
  let toplevel = Toplevel.unmarshal (Fpath.to_string toplevel_fpath) in
  let cmd = Toplevel.cmd toplevel in
  let dir = fst (Fpath.split_base (Toplevel.output_path toplevel)) in
  Util.mkdir_p dir;
  Format.printf "Toplevel: %a\n%!" Bos.Cmd.pp cmd;
  try
    ignore @@ Util.lines_of_process cmd;
    let digest = Some (Digest.file (Fpath.to_string @@ Toplevel.output_path toplevel) |> Digest.to_hex) in
    let toplevel = { toplevel with digest } in
    let digested_dir, _ = Fpath.split_base (Toplevel.digested_path toplevel) in
    Util.mkdir_p digested_dir;
    Util.mv (Toplevel.output_path toplevel) (Toplevel.digested_path toplevel);
    Some { toplevel with digest }
  with e ->
    Format.eprintf "Caught failure building js toplevel: %s\n%!" (Printexc.to_string e);
    let lines = Util.lines_of_process Bos.Cmd.(v "ls" % "-lR") in
    List.iter (fun line -> Format.eprintf "> %s\n%!" line) lines;
    exit 1

type config = {
  package : string;
  cma : string;
  dep_cmas : (string * string) list;
  cmis : string list;
  digest : string
} [@@deriving yojson]

type config_list = config list [@@deriving yojson]
let run path package =
  match
    Bos.OS.Dir.fold_contents
      (fun fpath (acc,universes) ->
        let rel_fpath =
          match Fpath.relativize ~root:path fpath with
          | Some x -> x
          | None -> failwith "Bos error"
        in

        let segs = Fpath.segs rel_fpath in
        let universes = match segs with
        | "universes" :: universe_id :: _ ->
          StringSet.add universe_id universes
        | _ -> universes in 

        let path_relevant =
          match (package, segs) with
          | Some s, "universes" :: _universe_id :: package' :: _ -> package' = s
          | Some _, _ -> false
          | None, _ -> true
        in

        if
          path_relevant
          && List.mem (Fpath.get_ext fpath) [ Cma.ext; Toplevel.meta_ext ]
        then (fpath :: acc, universes)
        else (acc, universes))
      ([], StringSet.empty) path
  with
  | Ok (files, universes) ->
      let toplevels =
        List.filter_map
          (fun x ->
            if Fpath.has_ext Cma.ext x then handle_cma x else handle_toplevel x)
          files
      in
      let cmis =
        List.concat
          (List.map (fun toplevel -> toplevel.Toplevel.cmis) toplevels)
      in
      let cmis = List.sort_uniq Cmi.compare cmis in
      Cmi.copy universes cmis;
      (* Output json like:
      [ 
        { "package": "base",
          "cma": "base.cma.worker.js",
          "dep_cmas": [ [ "/jsoo/universes/abcde/sexplib0/sexplib0.cma.js", "sexplib_dot_cma" ] ],
          "cmis": [ ... ]
        ...
      ]
      *)
      let packages = List.map (fun t -> t.Toplevel.package) toplevels |> List.sort_uniq Package.compare in
      List.iter (fun package ->
        let pkg_toplevels = List.filter (fun t -> t.Toplevel.package = package) toplevels in
        let toplevels = List.map (fun t ->
          let name = Fpath.basename t.Toplevel.cma_fpath in
          { package = name;
            cma = Toplevel.js_name t;
            dep_cmas = List.map (fun cma -> (Cma.server_path cma |> Fpath.to_string, Cma.func_name cma)) t.dep_cmas;
            cmis = List.map (fun cmi -> Cmi.server_path cmi |> Fpath.to_string) t.cmis;
            digest = match t.digest with | Some d -> d | None -> failwith "No digest" }) pkg_toplevels in
        let toplevel_json = config_list_to_yojson toplevels in 
        Util.write_file Fpath.(Package.jsoo_path package / "toplevels.json")
          [Yojson.Safe.to_string toplevel_json ]) packages
  | Error (`Msg m) ->
      Format.eprintf "Error while scanning: %s\n%!" m;
      exit 1
