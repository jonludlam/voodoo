type universe_id = string

and package_name = string

and package_version = string

and t = universe_id * package_name * package_version

let path ty package =
  let universe, pkg_name, pkg_version = package in
  Fpath.(ty / "universes" / universe / pkg_name / pkg_version)

let prep_path = path Paths.prep

let jsoo_path = path Paths.jsoo

let pp fmt ((u, n, v) : t) = Format.fprintf fmt "(%s,%s,%s)" u n v
