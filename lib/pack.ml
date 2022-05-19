type header = {
  signature : string;
  version_number : int;
  number_of_objects : int;
}

type object_type =
  | Invalid
  | Commit
  | Tree
  | Blob
  | Tag
  | Reserved
  | Ofs_Delta
  | Ref_Delta

type entry = {
  object_type : object_type;
  length : int;
  data : (bytes, string) result;
}

type t = { header : header; entries : entry list; trailer : bytes }

let test_pack =
  ".git/objects/pack/pack-b1d1fa42c7419f9b3b2b6296b0ce93d3d1312d6b.pack"

let parse_header ic =
  let header_len = 12 in
  let buf = Bytes.create header_len in
  really_input ic buf 0 header_len;

  let sig_b = Bytes.sub buf 0 4 in
  let signature = Bytes.to_string sig_b in
  let version_number = Bytes.get_int32_be buf 4 in
  let number_of_objects = Bytes.get_int32_be buf 8 in
  {
    signature;
    version_number = Int32.to_int version_number;
    number_of_objects = Int32.to_int number_of_objects;
  }

let parse_object_type ic =
  let b = input_byte ic in
  let o_type_i = (b land 0x70) lsr 4 in
  match o_type_i with
  | 1 -> Commit
  | 2 -> Tree
  | 3 -> Blob
  | 4 -> Tag
  | 6 -> Ofs_Delta
  | 7 -> Ref_Delta
  | _ -> Invalid

let parse_size_encoding ic =
  seek_in ic (pos_in ic - 1);
  let rec parse byte size shift =
    if byte land 0x80 = 0 then size
    else
      let byte = input_byte ic in
      let new_size = (byte land 0x7f) lsl shift in
      let size = size lor new_size in
      let shift = shift + 7 in
      parse byte size shift
  in
  let byte = input_byte ic in
  let size = byte land 0x0f in
  parse byte size 4

let parse_data ic =
  let open Zl in
  let allocate bits = De.make_window ~bits in
  let o = De.bigstring_create De.io_buffer_size in
  let i = De.bigstring_create De.io_buffer_size in
  let decoder = Inf.decoder `Manual ~o ~allocate in
  let buf = Buffer.create 0 in
  let bigstring_output o len =
    let res = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set res i o.{i}
    done;
    Buffer.add_bytes buf res
  in
  let rec go decoder =
    match Inf.decode decoder with
    | `Await decoder ->
        i.{0} <- input_char ic;
        let len = 1 in
        Inf.src decoder i 0 len |> go
    | `Flush decoder ->
        let len = De.io_buffer_size - Inf.dst_rem decoder in
        bigstring_output o len;
        Inf.flush decoder |> go
    | `Malformed err -> Error (Format.asprintf "%s." err)
    | `End decoder ->
        let len = De.io_buffer_size - Inf.dst_rem decoder in
        if len > 0 then bigstring_output o len;
        Ok buf
  in
  go decoder

let parse_entry ic =
  let object_type = parse_object_type ic in
  let length = parse_size_encoding ic in
  let data =
    match parse_data ic with
    | Error _ as e -> e
    | Ok b -> Ok (Buffer.to_bytes b)
  in
  { object_type; length; data }

let parse_entries ic n =
  let entries = ref [] in
  for _ = 1 to n do
    let entry = parse_entry ic in
    entries := entry :: !entries
  done;
  List.rev !entries

let parse_trailer ic =
  let len = (in_channel_length ic) - (pos_in ic) in
  let b = Bytes.create len in
  really_input ic b 0 len;
  b

let read path =
  let ic = open_in_bin path in
  let header = parse_header ic in
  let entries = parse_entries ic header.number_of_objects in
  let trailer = parse_trailer ic in
  close_in ic;
  { header; entries; trailer }
