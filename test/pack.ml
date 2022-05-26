let bind_read (r : Ogit.Pack.read_result) (f : Ogit.Pack.t -> unit) =
  match r with
  | Error e -> (
      match e with
      | Invalid_data s | Not_supported s | Failed s ->
          Alcotest.fail (Format.sprintf "unexpected error %s" s)
      | Partial_entries _ -> Alcotest.fail "partial entries")
  | Ok pack -> f pack

let ( >>= ) = bind_read

(* based on similar idea in irmin-bench test suite *)
let fixture_path f =
  let cwd = Fpath.v (Sys.getcwd ()) in
  let segs = cwd |> Fpath.segs |> List.rev in
  (match segs with
  | "test" :: "default" :: "_build" :: tl -> List.append (List.rev tl) f
  | _ -> f)
  |> String.concat Fpath.dir_sep
  |> Fpath.v
  |> Fpath.to_string

let pack_fixture =
  fixture_path
    [ "test"; "fixtures"; "pack-08631b7e544a9618098922174a04f29e70d72ae4.pack" ]

let test_header () =
  Ogit.Pack.read pack_fixture >>= fun pack ->
  Alcotest.(check string) "signature" "PACK" pack.header.signature;
  Alcotest.(check int) "version number" 2 pack.header.version_number;
  Alcotest.(check int) "no. of objects" 14 pack.header.number_of_objects

let test_entries () =
  Ogit.Pack.read pack_fixture >>= fun pack ->
  let entries = pack.entries in
  let n = List.length entries in
  Alcotest.(check int) "no. of entries" pack.header.number_of_objects n;
  List.iter2
    (fun (a : Ogit.Pack.object_type) (b : Ogit.Pack.entry) ->
      Alcotest.(check string)
        "object type"
        (Ogit.Pack.object_type_to_string a)
        (Ogit.Pack.object_type_to_string b.object_type))
    [
      Commit;
      Commit;
      Commit;
      Blob;
      Blob;
      Blob;
      Blob;
      Blob;
      Blob;
      Tree;
      Tree;
      Tree;
      Tree;
      Tree;
    ]
    entries;
  List.iter
    (fun (e : Ogit.Pack.entry) ->
      if e.original_object_type != Ofs_Delta then
        Alcotest.(check int) "data length" e.length (Bytes.length e.data);
      Alcotest.(check bool) "offset >0" true (e.offset > 0))
    entries

let test_trailer () =
  Ogit.Pack.read pack_fixture >>= fun pack ->
  Alcotest.(check int) "trailer length" 20 (Bytes.length pack.trailer)

let suite =
  [
    Alcotest.test_case "test_header" `Quick test_header;
    Alcotest.test_case "test_entries" `Quick test_entries;
    Alcotest.test_case "test_trailer" `Quick test_trailer;
  ]
