let read_size ?(nibble = false) next_byte =
  let rec read byte size shift =
    if byte land 0x80 = 0 then size
    else
      let byte = next_byte () in
      let new_size = (byte land 0x7f) lsl shift in
      let size = size lor new_size in
      let shift = shift + 7 in
      read byte size shift
  in
  let byte = next_byte () in
  if nibble then
    let size = byte land 0x0f in
    read byte size 4
  else
    let size = byte land 0x7f in
    read byte size 7

let read_offset next_byte =
  let rec read byte offset =
    if byte land 0x80 = 0 then offset
    else
      let byte = next_byte () in
      let offset = ((offset + 1) lsl 7) lor (byte land 0x7f) in
      read byte offset
  in
  let byte = next_byte () in
  let offset = byte land 0x7f in
  read byte offset

(* inline tests *)

let%test_unit "read_size (single, nibble)" =
  let next_byte () = 0b01110111 in
  let r = read_size next_byte ~nibble:true in
  [%test_eq: Base.int] r 7

let%test_unit "read_size (single, no nibble)" =
  let next_byte () = 0b01110111 in
  let r = read_size next_byte in
  [%test_eq: Base.int] r 119

let%test_unit "read_size (multiple, nibble)" =
  let s = Stream.of_list [ 0x80; 0x01 ] in
  let next_byte () = Stream.next s in
  let r = read_size next_byte ~nibble:true in
  [%test_eq: Base.int] r 16

let%test_unit "read_size (multiple, no nibble)" =
  let s = Stream.of_list [ 0x80; 0x01 ] in
  let next_byte () = Stream.next s in
  let r = read_size next_byte in
  [%test_eq: Base.int] r 128

let%test_unit "read_offset (single)" =
  let next_byte () = 0b01111111 in
  let r = read_offset next_byte in
  [%test_eq: Base.int] r 127

let%test_unit "read_offset (multiple)" =
  let s = Stream.of_list [ 0x81; 0x01 ] in
  let next_byte () = Stream.next s in
  let r = read_offset next_byte in
  [%test_eq: Base.int] r 257
