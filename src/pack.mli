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

val read : string -> read_result
(** [read path] reads and parses pack file at path *)

val object_type_to_string : object_type -> string
