module C = Configurator.V1

let get_vulkan c =
  match C.Pkg_config.get c with
  | None -> C.die "Could not find 'pkg-config'"
  | Some pkg_conf ->
    match C.Pkg_config.query pkg_conf ~package:"vulkan" with
    | Some cfg -> cfg
    | None -> C.die "pkg-config didn't find vulkan"

let () =
  C.main ~args:[] ~name:"discover_vulkan" @@ fun c ->
  let C.Pkg_config.{ cflags; libs } = get_vulkan c in
  C.Flags.write_sexp "cflags.sexp" cflags;
  C.Flags.write_sexp "libs.sexp" libs
