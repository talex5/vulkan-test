type t = {
  code : Drm.Fourcc.t;
  modifier : Drm.Modifier.t;
}

let pp f { code; modifier } =
  Fmt.pf f "%4s : %a" (Drm.Fourcc.to_string code) Drm.Modifier.pp modifier

let v code ~modifier = { code; modifier }
