open OcamlHeader

let rec _loop_pure_45 _x_51 =
  match _x_51 = 0 with true -> () | false -> _loop_pure_45 (_x_51 - 1)

let loop_pure = _loop_pure_45

let _test_pure_57 (_n_58 : int) = _loop_pure_45 _n_58

let test_pure = _test_pure_57

type (_, _) eff_internal_effect += Fail : (unit, empty) eff_internal_effect

let rec _loop_latent_59 _x_70 =
  match _x_70 = 0 with
  | true -> Value ()
  | false -> (
      match _x_70 < 0 with
      | true ->
          Call
            ( Fail,
              (),
              fun (_y_79 : empty) ->
                Value (match _y_79 with _ -> assert false) )
      | false -> _loop_latent_59 (_x_70 - 1))

let loop_latent = _loop_latent_59

let _test_latent_82 (_n_83 : int) = _loop_latent_59 _n_83

let test_latent = _test_latent_82

type (_, _) eff_internal_effect += Incr : (unit, unit) eff_internal_effect

let rec _loop_incr_84 _x_92 =
  match _x_92 = 0 with
  | true -> Value ()
  | false -> Call (Incr, (), fun (_y_98 : unit) -> _loop_incr_84 (_x_92 - 1))

let loop_incr = _loop_incr_84

let _test_incr_101 (_n_102 : int) =
  (let rec _loop_incr_117 _x_92 (_k_120 : unit -> int -> int) =
     match _x_92 = 0 with
     | true -> _k_120 ()
     | false ->
         fun (_x_124 : int) ->
           _loop_incr_117 (_x_92 - 1)
             (fun (_x_119 : unit) -> _k_120 _x_119)
             (_x_124 + 1)
   in
   _loop_incr_117 _n_102 (fun (_x_109 : unit) (_x_111 : int) -> _x_111))
    0

let test_incr = _test_incr_101

let rec _loop_incr'_128 _x_136 =
  match _x_136 = 0 with
  | true -> Value ()
  | false ->
      _loop_incr'_128 (_x_136 - 1) >> fun _ ->
      Call (Incr, (), fun (_y_144 : unit) -> Value _y_144)

let loop_incr' = _loop_incr'_128

let _test_incr'_145 (_n_146 : int) =
  (let rec _loop_incr'_161 _x_136 (_k_165 : unit -> int -> int) =
     match _x_136 = 0 with
     | true -> _k_165 ()
     | false ->
         _loop_incr'_161 (_x_136 - 1) (fun (_ : unit) (_x_169 : int) ->
             _k_165 () (_x_169 + 1))
   and _loop_incr_162 _x_92 (_k_176 : unit -> int -> int) =
     match _x_92 = 0 with
     | true -> _k_176 ()
     | false ->
         fun (_x_180 : int) ->
           _loop_incr_162 (_x_92 - 1)
             (fun (_x_175 : unit) -> _k_176 _x_175)
             (_x_180 + 1)
   in
   _loop_incr'_161 _n_146 (fun (_x_153 : unit) (_x_155 : int) -> _x_155))
    0

let test_incr' = _test_incr'_145

type (_, _) eff_internal_effect += Get : (unit, int) eff_internal_effect

type (_, _) eff_internal_effect += Put : (int, unit) eff_internal_effect

let rec _loop_state_184 _x_197 =
  match _x_197 = 0 with
  | true -> Value ()
  | false ->
      Call
        ( Get,
          (),
          fun (_y_206 : int) ->
            Call
              ( Put,
                _y_206 + 1,
                fun (_y_209 : unit) -> _loop_state_184 (_x_197 - 1) ) )

let loop_state = _loop_state_184

let _test_state_212 (_n_213 : int) =
  (let rec _loop_state_230 _x_197 (_k_233 : unit -> int -> int) =
     match _x_197 = 0 with
     | true -> _k_233 ()
     | false ->
         fun (_s_245 : int) ->
           _loop_state_230 (_x_197 - 1)
             (fun (_x_254 : unit) -> _k_233 _x_254)
             (_s_245 + 1)
   in
   _loop_state_230 _n_213 (fun (_x_221 : unit) (_x_223 : int) -> _x_223))
    0

let test_state = _test_state_212
