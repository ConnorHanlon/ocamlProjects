module type OrderedSig = sig
  type t
  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val leq: t -> t -> bool
end

module Int: (OrderedSig with type t=int) = struct
  type t = int
  let eq (i1:int)(i2:int):bool = (i1=i2)
  let lt (i1:int)(i2:int):bool = (i1<i2)
  let leq (i1:int)(i2:int):bool = (i1<=i2) 
end
  
