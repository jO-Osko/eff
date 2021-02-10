open OcamlHeader

let _op_0 (* < *) = ( < )

let _op_1 (* = *) = ( = )

let _op_2 (* - *) = ( - )

let _op_3 (* + *) = ( + )

let _op_4 (* ~- *) = ( ~- )

type (_, _) effect += Decide : (unit, bool) effect

type (_, _) effect += Fail : (unit, empty) effect

type queen = Queen of (int * int)

type rows = RowsEmpty | RowsCons of (int * rows)

type solution = SolutionEmpty | SolutionPlace of (queen * solution)

type solutions = SolutionsNil | SolutionsCons of (solution * solutions)

type optional_solution = None | Some of solution

type void = Void

let _absurd_5 (_void_6 : float) = match _void_6 with _ -> assert false

let rec _op_7 (* @ *) _x_14 (_ys_16 : solutions) =
  match _x_14 with
  | SolutionsNil -> _ys_16
  | SolutionsCons (_x_18, _xs_17) ->
      SolutionsCons (_x_18, _op_7 (* @ *) _xs_17 _ys_16)

let _op_21 (* > *) (_x_22 : int) (_y_23 : int) = _op_0 (* < *) _y_23 _x_22

let _op_25 (* <> *) (_x_26 : int) (_y_27 : int) =
  match _op_1 (* = *) _y_27 _x_26 with true -> false | false -> true

let _abs_30 (_x_31 : int) =
  match _op_0 (* < *) _x_31 0 with
  | true -> _op_4 (* ~- *) _x_31
  | false -> _x_31

let _no_attack_34 (_q1_35 : queen) (_q2_36 : queen) =
  match _q1_35 with
  | Queen (_x_37, _y_38) -> (
      match _q2_36 with
      | Queen (_x'_39, _y'_40) -> (
          match _op_25 (* <> *) _x_37 _x'_39 with
          | true -> (
              match _op_25 (* <> *) _y_38 _y'_40 with
              | true ->
                  _op_25 (* <> *)
                    (_abs_30 (_op_2 (* - *) _x_37 _x'_39))
                    (_abs_30 (_op_2 (* - *) _y_38 _y'_40))
              | false -> false
              | false -> false)))

let rec _not_attacked_52 _x_60 (_qs_62 : solution) =
  match _qs_62 with
  | SolutionEmpty -> true
  | SolutionPlace (_x_64, _xs_63) -> (
      match _no_attack_34 _x_60 _x_64 with
      | true -> _not_attacked_52 _x_60 _xs_63
      | false -> false)

let _available_68 (_number_of_queens_69 : int) (_x_70 : int) (_qs_71 : solution)
    =
  let rec _loop_72 _x_83 =
    let _possible_73, _y_74 = _x_83 in
    match _op_0 (* < *) _y_74 1 with
    | true -> _possible_73
    | false -> (
        match _not_attacked_52 (Queen (_x_70, _y_74)) _qs_71 with
        | true ->
            _loop_72 (RowsCons (_y_74, _possible_73), _op_2 (* - *) _y_74 1)
        | false -> _loop_72 (_possible_73, _op_2 (* - *) _y_74 1))
  in
  _loop_72 (RowsEmpty, _number_of_queens_69)

let rec _choose_84 _x_94 =
  match _x_94 with
  | RowsEmpty ->
      Call
        ( Fail,
          (),
          fun (_y_102 : empty) -> Value (match _y_102 with _ -> assert false) )
  | RowsCons (_x_104, _xs'_103) ->
      Call
        ( Decide,
          (),
          fun (_y_105 : bool) ->
            match _y_105 with
            | true -> Value _x_104
            | false -> _choose_84 _xs'_103 )

let _queens_106 (_number_of_queens_107 : int) =
  let rec _place_108 _x_119 =
    let _x_109, _qs_110 = _x_119 in
    match _op_21 (* > *) _x_109 _number_of_queens_107 with
    | true -> Value _qs_110
    | false ->
        _choose_84 (_available_68 _number_of_queens_107 _x_109 _qs_110)
        >> fun _y_113 ->
        _place_108
          ( _op_3 (* + *) _x_109 1,
            SolutionPlace (Queen (_x_109, _y_113), _qs_110) )
  in
  _place_108 (1, SolutionEmpty)

let _queens_one_option_120 (_number_of_queens_121 : int) =
  let rec _queens_134 (_number_of_queens_107, _k_136) =
    let rec _place_108 _x_119 =
      let _x_109, _qs_110 = _x_119 in
      match _op_21 (* > *) _x_109 _number_of_queens_107 with
      | true -> Value _qs_110
      | false ->
          _choose_84 (_available_68 _number_of_queens_107 _x_109 _qs_110)
          >> fun _y_113 ->
          _place_108
            ( _op_3 (* + *) _x_109 1,
              SolutionPlace (Queen (_x_109, _y_113), _qs_110) )
    in
    let rec _place_137 (_x_119, _k_139) =
      let _x_109, _qs_110 = _x_119 in
      match _op_21 (* > *) _x_109 _number_of_queens_107 with
      | true -> _k_139 _qs_110
      | false ->
          let rec _choose_141 (_x_94, _k_143) =
            match _x_94 with
            | RowsEmpty -> None
            | RowsCons (_x_104, _xs'_103) -> (
                let _l_130 (_y_105 : bool) =
                  match _y_105 with
                  | true -> _k_143 _x_104
                  | false ->
                      _choose_141 (_xs'_103, fun (_x_142 : int) -> _k_143 _x_142)
                in
                match _l_130 true with
                | Some _x_125 -> Some _x_125
                | None -> _l_130 false)
          in
          _choose_141
            ( _available_68 _number_of_queens_107 _x_109 _qs_110,
              fun (_y_113 : int) ->
                _place_137
                  ( ( _op_3 (* + *) _x_109 1,
                      SolutionPlace (Queen (_x_109, _y_113), _qs_110) ),
                    fun (_x_138 : solution) -> _k_139 _x_138 ) )
    in
    _place_137 ((1, SolutionEmpty), fun (_x_135 : solution) -> _k_136 _x_135)
  in
  _queens_134 (_number_of_queens_121, fun (_x_127 : solution) -> Some _x_127)

let _queens_all_166 (_number_of_queens_167 : int) =
  let rec _queens_181 (_number_of_queens_107, _k_183) =
    let rec _place_108 _x_119 =
      let _x_109, _qs_110 = _x_119 in
      match _op_21 (* > *) _x_109 _number_of_queens_107 with
      | true -> Value _qs_110
      | false ->
          _choose_84 (_available_68 _number_of_queens_107 _x_109 _qs_110)
          >> fun _y_113 ->
          _place_108
            ( _op_3 (* + *) _x_109 1,
              SolutionPlace (Queen (_x_109, _y_113), _qs_110) )
    in
    let rec _place_184 (_x_119, _k_186) =
      let _x_109, _qs_110 = _x_119 in
      match _op_21 (* > *) _x_109 _number_of_queens_107 with
      | true -> _k_186 _qs_110
      | false ->
          let rec _choose_188 (_x_94, _k_190) =
            match _x_94 with
            | RowsEmpty -> SolutionsNil
            | RowsCons (_x_104, _xs'_103) ->
                let _l_177 (_y_105 : bool) =
                  match _y_105 with
                  | true -> _k_190 _x_104
                  | false ->
                      _choose_188 (_xs'_103, fun (_x_189 : int) -> _k_190 _x_189)
                in
                _op_7 (* @ *) (_l_177 true) (_l_177 false)
          in
          _choose_188
            ( _available_68 _number_of_queens_107 _x_109 _qs_110,
              fun (_y_113 : int) ->
                _place_184
                  ( ( _op_3 (* + *) _x_109 1,
                      SolutionPlace (Queen (_x_109, _y_113), _qs_110) ),
                    fun (_x_185 : solution) -> _k_186 _x_185 ) )
    in
    _place_184 ((1, SolutionEmpty), fun (_x_182 : solution) -> _k_183 _x_182)
  in
  _queens_181
    ( _number_of_queens_167,
      fun (_x_174 : solution) -> SolutionsCons (_x_174, SolutionsNil) )

let _queens_one_cps_214 (_number_of_queens_215 : int) =
  let _absurd_216 (_void_217 : empty) =
    match _void_217 with _ -> assert false
  in
  (let rec _queens_241 (_number_of_queens_107, _k_243) =
     let rec _place_108 _x_119 =
       let _x_109, _qs_110 = _x_119 in
       match _op_21 (* > *) _x_109 _number_of_queens_107 with
       | true -> Value _qs_110
       | false ->
           _choose_84 (_available_68 _number_of_queens_107 _x_109 _qs_110)
           >> fun _y_113 ->
           _place_108
             ( _op_3 (* + *) _x_109 1,
               SolutionPlace (Queen (_x_109, _y_113), _qs_110) )
     in
     let rec _place_244 (_x_119, _k_246) =
       let _x_109, _qs_110 = _x_119 in
       match _op_21 (* > *) _x_109 _number_of_queens_107 with
       | true -> _k_246 _qs_110
       | false ->
           let rec _choose_248 (_x_94, _k_250) =
             match _x_94 with
             | RowsEmpty ->
                 fun (_kf_224 : unit -> solution computation) -> _kf_224 ()
             | RowsCons (_x_104, _xs'_103) ->
                 let _l_230 (_y_105 : bool) =
                   match _y_105 with
                   | true -> _k_250 _x_104
                   | false ->
                       _choose_248
                         (_xs'_103, fun (_x_249 : int) -> _k_250 _x_249)
                 in
                 fun (_kf_220 : unit -> solution computation) ->
                   _l_230 true (fun (_ : unit) -> _l_230 false _kf_220)
           in
           _choose_248
             ( _available_68 _number_of_queens_107 _x_109 _qs_110,
               fun (_y_113 : int) ->
                 _place_244
                   ( ( _op_3 (* + *) _x_109 1,
                       SolutionPlace (Queen (_x_109, _y_113), _qs_110) ),
                     fun (_x_245 : solution) -> _k_246 _x_245 ) )
     in
     _place_244 ((1, SolutionEmpty), fun (_x_242 : solution) -> _k_243 _x_242)
   in
   _queens_241
     ( _number_of_queens_215,
       fun (_x_225 : solution) ->
         coer_arrow coer_refl_ty (coer_return coer_refl_ty)
           (fun (_ : unit -> solution computation) -> _x_225) ))
    (fun (() : unit) ->
      Call (Fail, (), fun (_y_240 : empty) -> Value (_absurd_216 _y_240)))
