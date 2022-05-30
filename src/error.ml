module type S = sig
  type error
  type 'a mresult = ('a, error list) result

  val return : 'a -> 'a mresult
  val fail : error -> 'a mresult
  val fail_cons : error -> error list -> 'a mresult
  val fail_all : error list -> 'a mresult
end

module Make (E : sig
  type error
end) : S with type error = E.error = struct
  type error = E.error
  type 'a mresult = ('a, error list) result

  let return v = Ok v
  let fail e = Error [ e ]
  let fail_cons e l = Error (e :: l)
  let fail_all l = Error l
end
