(* Odoc documentation generator *)
open Cmdliner

(** Example: [conv_compose Fpath.of_string Fpath.to_string Arg.dir] *)
let conv_compose ?docv parse to_string c =
  let open Arg in
  let docv = match docv with Some v -> v | None -> conv_docv c in
  let parse v = match conv_parser c v with Ok x -> parse x | Error _ as e -> e
  and print fmt t = conv_printer c fmt (to_string t) in
  conv ~docv (parse, print)

(* Just to find the location of all relevant ocaml cmt/cmti/cmis *)
let read_lib_dir () = Opam.lib ()

module Prep = struct
  let switch =
    let doc =
      "Opam switch to use. If not set, defaults to the current switch"
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "s"; "switch" ] ~doc ~docv:"SWITCH")

  let prep lib_dir switch universes =
    Opam.switch := switch;
    Main.run lib_dir universes

  let lib_dir =
    let doc =
      "Path to libraries. If not set, defaults to the global environment by \
       querying $(b,ocamlfind)."
    in
    let fpath_dir = conv_compose Fpath.of_string Fpath.to_string Arg.dir in
    (* [some string] and not [some dir] because we don't need it to exist yet. *)
    Arg.(value & opt_all fpath_dir [] & info [ "L" ] ~doc ~docv:"LIB_DIR")

  let universes =
    let doc = "Provide universe spec as 'package=universe id' couples" in
    Arg.(
      value
      & opt (list (pair ~sep:':' string string)) []
      & info [ "u"; "universes" ] ~doc)

  let cmd = Term.(const prep $ lib_dir $ switch $ universes)

  let info = Term.info "prep" ~doc:"Prep a directory tree for compiling"
end

module Test = struct
  let test () =
    let pkgs = Findlib.list_packages' () in
    List.iter (fun s -> Format.eprintf "%s\n%!" s) pkgs;
  let root = Opam.prefix () |> Fpath.v in
  let packages = Opam.all_opam_packages () in

  let pkg_contents =
    List.map
      (fun pkg ->
        (pkg.Opam.name, Opam.pkg_contents pkg.Opam.name))
      packages
  in
  ignore(root, pkg_contents);
  (* let pkgmetas = List.map (fun pkg -> (pkg, Findlib.package_meta_file pkg)) pkgs in
  let metas = List.map snd pkgmetas |> List.sort_uniq String.compare in
  let metas = List.map (fun meta -> (meta, List.filter (fun (_, meta') -> meta' = meta) pkgmetas |> List.map fst)) metas in 
  ignore (metas, pkg_contents);
  let findlib_of_opam = List.map (fun (pkgname, contents) ->
    let ms = List.find_all (fun f -> Fpath.basename f = "META") contents in
    let findlib_packages =
      List.map (fun meta ->
        match List.assoc_opt (Fpath.(to_string (root // meta))) metas with
        | Some pkgs -> pkgs
        | None -> []) ms
      in
      (pkgname, List.concat findlib_packages)) pkg_contents in
  List.iter (fun (opam_pkg, findlib_pkgs) ->
    Format.eprintf "%s: [%s]\n%!" opam_pkg (String.concat "; " findlib_pkgs)) findlib_of_opam *)

  List.iter (fun pkg ->
    try
      let archives = Findlib.package_property ["byte"; "javascript"] pkg "archive" in
      let path = Findlib.package_directory pkg in 
      begin
        let archive_path = Printf.sprintf "%s/%s" path archives in
        match List.find_opt (fun (_, contents) -> List.exists (fun fpath -> Fpath.(to_string (root // fpath)) = archive_path) contents) pkg_contents with
        | Some (opam_pkg, _) -> Format.eprintf "opam package: %s findlib package: %s archive: %s\n%!" opam_pkg pkg archive_path;
        | None -> Format.eprintf "No opam package found for findlib package %s archive %s\n%!" pkg archive_path
      end
    with e ->
      Format.eprintf "Exception %s handling package %s\n%!" (Printexc.to_string e) pkg) pkgs;
  
  let findlib_cmas =
    List.filter_map (fun pkg ->
      try
        let archives = Findlib.package_property ["byte"; "javascript"] pkg "archive" in
        let path = Findlib.package_directory pkg in
        Some (Fpath.v (Printf.sprintf "%s/%s" path archives), pkg)
      with _ -> None) pkgs
  in

  List.iter (fun (opam_pkg, contents) ->
    let cmas = List.filter (fun p -> Fpath.has_ext ".cma" p) contents in
    let findlib_pkgs = List.filter_map (fun p -> try Some (List.assoc Fpath.(root // p) findlib_cmas) with Not_found ->
      Format.eprintf "XXX No findlib pkg for archive %s\n%!" (Fpath.to_string p); None ) cmas in
    Format.eprintf "opam pkg %s: findlib archives: [%s]\n%!" opam_pkg (String.concat "; " findlib_pkgs)
    ) pkg_contents


  let cmd = Term.(const test $ const ())

  let info = Term.info "test" ~doc:"Test function"
end


let _ =
  match Term.eval_choice ~err:Format.err_formatter Prep.(cmd, info) [Test.(cmd, info)] with
  | `Error _ ->
      Format.pp_print_flush Format.err_formatter ();
      exit 2
  | _ -> ()
