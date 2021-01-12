(*
=== GENERATED FROM queens.eff ===
commit SHA: a4fc6bd92723428911a2469b49b7b7e80480dc2f
=== BEGIN SOURCE ===

external ( <> ) : int -> int -> bool = "<>"
external ( < ) : int -> int -> bool = "<"
external ( > ) : int -> int -> bool = ">"
external ( - ) : int -> int -> int = "-"
external ( + ) : int -> int -> int = "+"
external ( ~- ) : int -> int = "~-"

let absurd void = match void with;;

let abs x = if x < 0 then -x else x

let rec (@) xs ys =
  match xs with
  | [] -> ys
  | x :: xs -> x :: (xs @ ys)

(******************************************************************************)
let no_attack (x, y) (x', y') =
  x <> x' && y <> y' && abs (x - x') <> abs (y - y')

let rec not_attacked x' = function
  | [] -> true
  | x :: xs -> if no_attack x' x then not_attacked x' xs else false

let available (number_of_queens, x, qs) =
  let rec loop (possible, y) =
    if y < 1 then
      possible
    else if not_attacked (x, y) qs then
      loop ((y :: possible), (y - 1))
    else
      loop (possible, (y - 1))
  in
  loop ([], number_of_queens)

(******************************************************************************)

effect Decide : unit -> bool
effect Fail : unit -> empty

type 'a option = None | Some of 'a

let rec choose = function
  | [] -> (match (#Fail ()) with)
  | x::xs -> if #Decide () then x else choose xs

let optionalize = handler
  | val y -> (Some y)
  | #Decide _ k -> (match k true with Some x -> Some x | None -> k false)
  | #Fail _ _ -> None

let backtrack = handler
  | val y -> (fun _ -> y)
  | #Decide _ k -> (fun kf -> k true (fun () -> k false kf) )
  | #Fail _ _ -> (fun kf -> kf ())

let choose_all = handler
  | val x -> [x]
  | #Decide _ k -> k true @ k false
  | #Fail _ _ -> []

(******************************************************************************)

let queens number_of_queens =
  let rec place (x, qs) =
    if x > number_of_queens then qs else
      let y = choose (available (number_of_queens, x, qs)) in
      place ((x + 1), ((x, y) :: qs))
  in
  place (1, [])

let queens_one_option number_of_queens =
  with optionalize handle queens number_of_queens

let queens_one_cps number_of_queens =
  (with backtrack handle queens number_of_queens) (fun () -> (absurd (#Fail ())))

let queens_all number_of_queens =
  with choose_all handle queens number_of_queens

=== END SOURCE ===
*)

type effect_symbol = string

type ('eff_arg, 'eff_res) effect_type = effect_symbol

type 'a computation =
  | Value : 'a -> 'a computation
  | Call : ('eff_arg, 'eff_res) effect_type * 'eff_arg * ('eff_res -> 'a computation) -> 'a computation

type ('a, 'b) value_clause = 'a -> 'b computation

type 'a effect_clauses =
  | Nil : 'a effect_clauses
  | Cons : ('eff_arg, 'eff_res) effect_type * ('eff_arg -> ('eff_res -> 'a computation) -> 'a computation) * 'a effect_clauses -> 'a effect_clauses

type ('a, 'b, 'c) handler = {
  value_clause: ('a, 'b) value_clause;
  effect_clauses: 'b effect_clauses;
}

let rec find_case : 'eff_arg 'eff_res. ('eff_arg, 'eff_res) effect_type -> 'a effect_clauses -> ('eff_arg -> ('eff_res -> 'a computation) -> 'a computation) =
  fun eff eff_clauses ->
    match eff_clauses with
    | Nil ->
      (fun x k -> Call (eff, x, k))
    | Cons (eff', case, eff_clauses) ->
      if eff = eff' then Obj.magic case else find_case eff eff_clauses

let rec (>>) (c : 'a computation) (f : 'a -> 'b computation) =
  match c with
  | Value x -> f x
  | Call (eff, arg, k) -> Call (eff, arg, fun y -> k y >> f)

let rec handle (h : ('a, 'b, 'c) handler) (c : 'a computation) : 'c computation =
  let rec handler = function
  | Value x -> h.value_clause x
  | Call (eff, arg, k) ->
    let f = find_case eff h.effect_clauses in
    f arg (fun y -> handler (k y))
  in
  handler c

let value (x : 'a) : 'a computation = Value x

let call (eff : ('a, 'b) effect_type) (arg : 'a) (cont : 'b -> 'c computation) : 'c computation =
  Call (eff, arg, cont)

let effect_type eff = fun arg -> call eff arg value

let run =
  function | Value x -> x | Call (eff,_,_) -> failwith "Uncaught effect"
let ( ** ) =
  let rec pow a =
    let open Pervasives in
      function
      | 0 -> 1
      | 1 -> a
      | n ->
          let b = pow a (n / 2)  in
          (b * b) * (if (n mod 2) = 0 then 1 else a)
     in
  pow
let string_length _ = assert false
let to_string _ = assert false
let lift_unary f x = value (f x)
let lift_binary f x = value (fun y  -> value (f x y))
;;"End of pervasives"
let _var_1 = (<>)
let _var_2 = (<)
let _var_3 = (>)
let _var_4 = (-)
let _var_5 = (+)
let _var_6 = (~-)
let _absurd_7 _void_8 = match _void_8 with | _ -> assert false
let _abs_9 _x_10 = if _x_10 < 0 then - _x_10 else _x_10
let rec _var_13 _xs_14 _ys_15 =
  match _xs_14 with
  | [] -> _ys_15
  | _x_16::_xs_17 -> _x_16 :: (_var_13 _xs_17 _ys_15)
let _no_attack_20 (_x_21,_y_22) (_x'_23,_y'_24) =
  if _x_21 <> _x'_23
  then
    (if _y_22 <> _y'_24
     then (_abs_9 (_x_21 - _x'_23)) <> (_abs_9 (_y_22 - _y'_24))
     else false)
  else false
let rec _not_attacked_36 _x'_37 _gen_function_38 =
  match _gen_function_38 with
  | [] -> true
  | _x_39::_xs_40 ->
      if _no_attack_20 _x'_37 _x_39
      then _not_attacked_36 _x'_37 _xs_40
      else false

let _available_44 (_number_of_queens_45,_x_46,_qs_47) =
  let rec _loop_48 (_possible_49,_y_50) =
    if _y_50 < 1
    then _possible_49
    else
      if _not_attacked_36 (_x_46, _y_50) _qs_47
      then _loop_48 ((_y_50 :: _possible_49), (_y_50 - 1))
      else _loop_48 (_possible_49, (_y_50 - 1))
     in
  _loop_48 ([], _number_of_queens_45)
  let effect_Decide : ( unit,  bool) effect_type = "effect_Decide";;
  let effect_Fail : ( unit, 'empty) effect_type = "effect_Fail";;
type 't1 option =
  | None
  | Some of 't1
let rec _choose_59 _gen_let_rec_function_60 =
  match _gen_let_rec_function_60 with
  | [] ->
      call effect_Fail ()
        (fun _result_3  -> value (match _result_3 with | _ -> assert false))
  | _x_62::_xs_63 ->
      call effect_Decide ()
        (fun _result_6  ->
           if _result_6 then value _x_62 else _choose_59 _xs_63)
(*
let _optionalize_65 comp =

    {
      value_clause = (fun _y_69  -> value (Some _y_69));
      effect_clauses = Cons( (effect_Decide),
               (fun (_ : unit)  ->
                  fun (_k_66 : bool -> _)  ->
                    (_k_66 true) >>
                      (fun _gen_bind_68  ->
                         match _gen_bind_68 with
                         | Some _x_67 -> value (Some _x_67)
                         | None  -> _k_66 false)),
           Cons( (effect_Fail),
               (fun (_ : unit)  -> fun (_ : unit -> _)  -> value None), (Nil)))
    } comp

let _backtrack_70 comp =
  handler
    {
      value_clause =
        (fun _y_76  ->
           value
             (fun _lift_fun_238  -> value ((fun _  -> _y_76) _lift_fun_238)));
      effect_clauses = fun (type a) -> fun (type b) ->
        fun (x : (a,b) effect)  ->
          (match x with
           | Effect_Decide  ->
               (fun (_ : unit)  ->
                  fun (_k_72 : bool -> _)  ->
                    value
                      (fun _kf_73  ->
                         (_k_72 true) >>
                           (fun _gen_bind_74  ->
                              _gen_bind_74
                                (fun ()  ->
                                   (_k_72 false) >>
                                     (fun _gen_bind_75  ->
                                        _gen_bind_75 _kf_73)))))
           | Effect_Fail  ->
               (fun (_ : unit)  ->
                  fun (_ : unit -> _)  -> value (fun _kf_71  -> _kf_71 ()))
           | eff' -> (fun arg  -> fun k  -> Call (eff', arg, k)) : a ->
                                                                    (b -> _)
                                                                    ->
                                                                    _)
    } comp

let _choose_all_77 comp =
  handler
    {
      value_clause = (fun _x_82  -> value [_x_82]);
      effect_clauses = fun (type a) -> fun (type b) ->
        fun (x : (a,b) effect)  ->
          (match x with
           | Effect_Decide  ->
               (fun (_ : unit)  ->
                  fun (_k_78 : bool -> _)  ->
                    (_k_78 true) >>
                      (fun _gen_bind_80  ->
                         let _gen_bind_79 = _var_13 _gen_bind_80  in
                         (_k_78 false) >>
                           (fun _gen_bind_81  ->
                              value (_gen_bind_79 _gen_bind_81))))
           | Effect_Fail  ->
               (fun (_ : unit)  -> fun (_ : unit -> _)  -> value [])
           | eff' -> (fun arg  -> fun k  -> Call (eff', arg, k)) : a ->
                                                                    (b -> _)
                                                                    ->
                                                                    _)
    } comp *)

let _queens_83 _number_of_queens_84 =
  let rec _place_85 (_x_86,_qs_87) =
    if _x_86 > _number_of_queens_84
    then value _qs_87
    else
      (_choose_59 (_available_44 (_number_of_queens_84, _x_86, _qs_87))) >>
        ((fun _y_90  -> _place_85 ((_x_86 + 1), ((_x_86, _y_90) :: _qs_87))))
     in
  _place_85 (1, [])
let _queens_one_option_94 _number_of_queens_95 =
  let rec _newvar_17 (_x_14,_qs_13) =
    if _x_14 > _number_of_queens_95
    then Some _qs_13
    else
      (let rec _newvar_35 _gen_let_rec_function_60 =
         match _gen_let_rec_function_60 with
         | [] -> None
         | _x_62::_xs_63 ->
             (match _newvar_17 ((_x_14 + 1), ((_x_14, _x_62) :: _qs_13)) with
              | Some _x_70 -> Some _x_70
              | None  -> _newvar_35 _xs_63)
          in
       _newvar_35 (_available_44 (_number_of_queens_95, _x_14, _qs_13)))
     in
  _newvar_17 (1, [])
let _queens_one_cps_96 _number_of_queens_97 =
  let rec _newvar_86 (_x_83,_qs_82) =
    if _x_83 > _number_of_queens_97
    then fun _lift_fun_239  -> value ((fun _  -> _qs_82) _lift_fun_239)
    else
      (let rec _newvar_112 _gen_let_rec_function_60 =
         match _gen_let_rec_function_60 with
         | [] -> (fun _kf_132  -> _kf_132 ())
         | _x_62::_xs_63 ->
             (fun _kf_159  ->
                _newvar_86 ((_x_83 + 1), ((_x_83, _x_62) :: _qs_82))
                  (fun ()  -> _newvar_112 _xs_63 _kf_159))
          in
       _newvar_112 (_available_44 (_number_of_queens_97, _x_83, _qs_82)))
     in
  _newvar_86 (1, [])
    (fun ()  ->
       call effect_Fail () (fun _result_73  -> value (_absurd_7 _result_73)))

let _queens_all_100 _number_of_queens_101 =
  let rec _newvar_173 (_x_170,_qs_169) =
    if _x_170 > _number_of_queens_101
    then [_qs_169]
    else
      (let rec _newvar_195 _gen_let_rec_function_60 =
         match _gen_let_rec_function_60 with
         | [] -> []
         | _x_62::_xs_63 ->
             _var_13
               (_newvar_173 ((_x_170 + 1), ((_x_170, _x_62) :: _qs_169)))
               (_newvar_195 _xs_63)
          in
       _newvar_195 (_available_44 (_number_of_queens_101, _x_170, _qs_169)))
     in
  _newvar_173 (1, [])
;;

let n =
    if Array.length Sys.argv > 1
    then int_of_string Sys.argv.(1)
    else 8
  in
  for i = 1 to 1000 do
 _queens_one_cps_96 n
 done;;
