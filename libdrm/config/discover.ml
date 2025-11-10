module C = Configurator.V1

let () =
  C.main ~name:"checkdrm" (fun c ->
      let conf =
        match C.Pkg_config.get c with
        | None -> failwith "pkg-config is not installed"
        | Some pc ->
          match C.Pkg_config.query pc ~package:"libdrm" with
          | None -> failwith "libdrm is not installed (according to pkg-config)"
          | Some deps -> deps
      in
      let defs =
        C.C_define.import c ~c_flags:conf.cflags ~includes:["linux/dma-buf.h"; "xf86drm.h"]
          C.C_define.Type.[
            "DMA_BUF_SYNC_RW", Int;
            "DMA_BUF_IOCTL_EXPORT_SYNC_FILE", Int;
            "DMA_BUF_IOCTL_IMPORT_SYNC_FILE", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      C.Flags.write_lines "config.ml" defs;
      C.Flags.write_sexp "c_flags.sexp"         conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs
    )
