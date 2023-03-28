(* src *)

type t = {
  path : Paths.t;
  name : string;
  contents : string list;
  parent : Mld.t;
}

let pp fmt v =
  Format.fprintf fmt "{ path: %a; name: %s; parent %a }" Fpath.pp
    v.path v.name Mld.pp v.parent

let output_dir : base:Fpath.t -> t -> Paths.t =
 fun ~base src ->
  let pdir = Mld.output_dir ~base src.parent in
  Fpath.(pdir / src.parent.name)

let compile_dir = output_dir ~base:Paths.compile
let link_dir = output_dir ~base:Paths.link

let output_file src =
  Format.eprintf "ZZZ: src=%a\nparent output_dir=%a\n%!" pp src Fpath.pp (Mld.output_dir ~base:Paths.compile src.parent);
  Fpath.(output_dir ~base:Paths.compile src / ("src-" ^ src.name ^ ".odoc"))

let output_odocl src =
  Fpath.(output_dir ~base:Paths.link src / ("src-" ^ src.name ^ ".odocl"))
    
let compile src =
  let () = Bos.OS.File.delete (output_file src) |> Util.get_ok in
  let extra_include, parent =
    (Mld.compile_dir src.parent, src.parent.name)
  in
  ignore
    (Odoc.source_tree parent ~output:(output_file src) src.path extra_include)

let write src =
  let contents = List.sort String.compare src.contents in
  let oc = open_out Fpath.(to_string src.path) in
  List.iter (fun line ->
    Printf.fprintf oc "%s\n" line
  ) contents;
  close_out oc

let v dir name parent contents =
  let path = Fpath.(dir / (name ^ ".map")) in
  let src = { path; name; parent; contents } in
  Util.mkdir_p dir;
  write src;
  src
