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
  data : bytes;
  original_object_type : object_type;
  offset : int;
}

type t = { header : header; entries : entry list; trailer : bytes }

type error =
  | Invalid_data of string
  | Not_supported of string
  | Failed of string
  | Partial_entries of t * error

type read_result = (t, error) result

let object_type_to_string = function
  | Invalid -> "invalid"
  | Commit -> "commit"
  | Tree -> "tree"
  | Blob -> "blob"
  | Tag -> "Tag"
  | Reserved -> "reserved"
  | Ofs_Delta -> "offset delta"
  | Ref_Delta -> "ref delta"

let next_byte ic () = input_byte ic

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
  | 5 -> Reserved
  | 6 -> Ofs_Delta
  | 7 -> Ref_Delta
  | _ -> Invalid

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
    | `Malformed err -> Error (Invalid_data (Format.asprintf "%s." err))
    | `End decoder ->
        let len = De.io_buffer_size - Inf.dst_rem decoder in
        if len > 0 then bigstring_output o len;
        Ok (Buffer.to_bytes buf)
  in
  go decoder

let rec parse_entry ic =
  let pos = pos_in ic in
  let original_object_type = parse_object_type ic in
  seek_in ic (pos_in ic - 1);
  (* move back one since object type byte has part of the size *)
  let length = Encoding.read_size ~nibble:true (next_byte ic) in
  let object_type, data =
    match original_object_type with
    | Ofs_Delta -> parse_ofs_delta ic pos
    | Ref_Delta -> (Invalid, Error (Not_supported "ref delta"))
    | _ -> (original_object_type, parse_data ic)
  in
  match data with
  | Ok data ->
      Ok { object_type; length; data; offset = pos; original_object_type }
  | Error _ as e -> e

and parse_ofs_delta ic pos =
  let offset = Encoding.read_offset (next_byte ic) in
  let offset_pos = pos - offset in
  let data_pos = pos_in ic in
  seek_in ic offset_pos;
  let entry = parse_entry ic in
  match entry with
  | Error _ as e -> (Invalid, e)
  | Ok entry -> (
      seek_in ic data_pos;
      let transform_data = parse_data ic in
      match (entry.data, transform_data) with
      | _, Error _ -> (Invalid, Error (Failed "error parsing ofs delta"))
      | base_data, Ok transform_data ->
          let buf = Buffer.create 0 in
          let s = Stream.of_bytes transform_data in
          let next_byte () = Stream.next s in
          let next_int () = next_byte () |> Char.code in
          let read_bytes n = Bytes.init n (fun _ -> next_byte ()) in
          let src_size = Encoding.read_size next_int in
          let dst_size = Encoding.read_size next_int in
          assert (src_size = Bytes.length base_data);
          while Option.is_some @@ Stream.peek s do
            let byte = next_int () in
            if byte = 0 then failwith "unexpected delta opcode 0"
            else if byte land 0x80 != 0 then
              (* copy data *)
              let vals =
                Array.init 7 Fun.id
                |> Array.map (fun i ->
                       let mask = 1 lsl i in
                       if byte land mask != 0 then next_int () else 0)
              in
              let get_int_le bytes =
                let value = ref 0 in
                bytes
                |> Array.iteri (fun i b ->
                       let shift = i * 8 in
                       value := !value lor (b lsl shift));
                !value
              in
              let offset = get_int_le (Array.sub vals 0 4) in
              let size = get_int_le (Array.sub vals 4 3) in
              let size = if size = 0 then 0x10000 else size in
              Buffer.add_bytes buf (Bytes.sub base_data offset size)
            else
              (* append data *)
              let size = byte land 0x7f in
              Buffer.add_bytes buf (read_bytes size)
          done;
          assert (dst_size = Buffer.length buf);
          (entry.object_type, Ok (Buffer.to_bytes buf)))

let parse_entries ic n =
  match
    List.init n Fun.id
    |> List.fold_left
         (fun acc _ ->
           match acc with
           | Error _ as e -> e
           | Ok entries -> (
               match parse_entry ic with
               | Ok entry -> Ok (entry :: entries)
               | Error e -> Error (entries, e)))
         (Ok [])
  with
  | Ok entries -> Ok (List.rev entries)
  | Error (entries, e) -> Error (List.rev entries, e)

let parse_trailer ic =
  let len = in_channel_length ic - pos_in ic in
  let b = Bytes.create len in
  really_input ic b 0 len;
  b

let read path =
  try
    let ic = open_in_bin path in
    let header = parse_header ic in
    let entries = parse_entries ic header.number_of_objects in
    let trailer =
      if Result.is_ok entries then parse_trailer ic else Bytes.empty
    in
    close_in ic;
    match entries with
    | Ok entries -> Ok { header; entries; trailer }
    | Error (entries, e) ->
        Error (Partial_entries ({ header; entries; trailer }, e))
  with Sys_error e -> Error (Failed e)
