type t = (Typed.variable, Types.target_ty) Common.assoc

let empty = []

let lookup ctx x =  (Common.lookup x ctx)

let update ctx x sch = (x, sch) :: ctx
