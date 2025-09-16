module A = Vulkan.A

type t = {
  vertices : Vertex.t A.t;
  indices : int list;
}

let f = float_of_string

let parse_node s =
  match String.split_on_char '/' s with
  | [v; vt; _vn] -> int_of_string v, int_of_string vt
  | _ -> Fmt.failwith "Bad node %S" s

let parse lines =
  let v = ref [] in
  let vt = ref [] in
  let faces = ref [] in
  lines |> Seq.iter (function
      | "" -> ()
      | line when line.[0] = '#' -> ()
      | line ->
        match String.split_on_char ' ' line with
        | ["v"; x; y; z] -> v := (f x, f y, f z) :: !v
        | ["vt"; u; v] -> vt := (f u, ~-. (f v)) :: !vt
        | ["vn"; _; _; _] -> ()
        | "f" :: nodes -> faces := List.map parse_node nodes :: !faces
        | _ -> Log.debug (fun f -> f "Skipping unknown directive %S@." line)
    );
  let v = Array.of_list (List.rev !v) in
  let vt = Array.of_list (List.rev !vt) in
  let faces = Array.of_list (List.rev !faces) in
  let combined = Dynarray.create () in
  let indices = ref [] in
  let comb = Hashtbl.create (Array.length v) in
  let combine (v_i, vt_i) =
    match Hashtbl.find_opt comb (v_i, vt_i) with
    | Some i -> i
    | None ->
      let v = v.(v_i - 1) in
      let vt = vt.(vt_i - 1) in
      let i = Dynarray.length combined in
      Dynarray.add_last combined (Vertex.make v vt);
      i
  in
  faces |> Array.iter (fun nodes ->
      let nodes = List.map combine nodes in
      indices := nodes @ !indices;
    );
  let vertices = A.of_list Vertex.ctype (Dynarray.to_list combined) in
  let indices = List.rev !indices in
  { vertices; indices }
