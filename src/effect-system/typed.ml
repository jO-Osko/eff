open CoreUtils
(** Syntax of the core language. *)

open Types
module EffectMap = Map.Make (CoreTypes.Effect)

let add_to_constraints con constraints = con :: constraints

let add_list_to_constraints new_constraints old_constraints =
  new_constraints @ old_constraints

type variable = CoreTypes.Variable.t

type effect = CoreTypes.Effect.t * (Types.target_ty * Types.target_ty)

type pattern =
  | PVar of variable
  | PAs of pattern * variable
  | PTuple of pattern list
  | PRecord of (CoreTypes.Field.t, pattern) Assoc.t
  | PVariant of CoreTypes.Label.t * pattern
  | PConst of Const.t
  | PNonbinding

let rec pattern_vars = function
  | PVar x -> [ x ]
  | PAs (p, x) -> x :: pattern_vars p
  | PTuple lst -> List.fold_left (fun vs p -> vs @ pattern_vars p) [] lst
  | PRecord lst -> Assoc.fold_left (fun vs (_, p) -> vs @ pattern_vars p) [] lst
  | PVariant (_, p) -> pattern_vars p
  | PConst _ -> []
  | PNonbinding -> []

type ty_coercion =
  | ReflTy of Types.target_ty
  | ArrowCoercion of ty_coercion * dirty_coercion
  | HandlerCoercion of dirty_coercion * dirty_coercion
  | TyCoercionVar of CoreTypes.TyCoercionParam.t
  | SequenceTyCoer of ty_coercion * ty_coercion
  | ApplyCoercion of CoreTypes.TyName.t * ty_coercion list
  | TupleCoercion of ty_coercion list
  | LeftArrow of ty_coercion
  | PureCoercion of dirty_coercion
  | QualTyCoer of ct_ty * ty_coercion
  | QualDirtCoer of ct_dirt * ty_coercion
  | ApplyQualTyCoer of ty_coercion * ty_coercion
  | ApplyQualDirtCoer of ty_coercion * dirt_coercion

and dirt_coercion =
  | ReflDirt of dirt
  | DirtCoercionVar of CoreTypes.DirtCoercionParam.t
  | Empty of dirt
  | UnionDirt of (Types.effect_set * dirt_coercion)
  | SequenceDirtCoer of dirt_coercion * dirt_coercion
  | DirtCoercion of dirty_coercion

and dirty_coercion =
  | BangCoercion of ty_coercion * dirt_coercion
  | RightArrow of ty_coercion
  | RightHandler of ty_coercion
  | LeftHandler of ty_coercion
  | SequenceDirtyCoer of (dirty_coercion * dirty_coercion)

(** Pure expressions *)
type expression =
  | Var of variable
  | Const of Const.t
  | Tuple of expression list
  | Record of (CoreTypes.Field.t, expression) Assoc.t
  | Variant of CoreTypes.Label.t * expression
  | Lambda of abstraction_with_ty
  | Effect of effect
  | Handler of handler
  | CastExp of expression * ty_coercion
  | LambdaTyCoerVar of CoreTypes.TyCoercionParam.t * Types.ct_ty * expression
  | LambdaDirtCoerVar of
      CoreTypes.DirtCoercionParam.t * Types.ct_dirt * expression
  | ApplyTyCoercion of expression * ty_coercion
  | ApplyDirtCoercion of expression * dirt_coercion

(** Impure computations *)
and computation =
  | Value of expression
  | LetVal of expression * abstraction_with_ty
  | LetRec of letrec_abstraction list * computation
  (* Historical note: Previously LetRec looked like this:

       LetRec of (variable * Types.target_ty * expression) list * computation

     Unfortunately this shape forgets the source structure (where the
     abstraction is explicit) and thus makes translation to MulticoreOcaml
     impossible in the general case.
  *)
  | Match of expression * Types.target_dirty * abstraction list
  (* We need to keep the result type in the term, in case the match is empty *)
  | Apply of expression * expression
  | Handle of expression * computation
  | Call of effect * expression * abstraction_with_ty
  | Op of effect * expression
  | Bind of computation * abstraction
  | CastComp of computation * dirty_coercion
  | CastComp_ty of computation * ty_coercion
  | CastComp_dirt of computation * dirt_coercion

and handler = {
  effect_clauses : (effect, abstraction2) Assoc.t;
  value_clause : abstraction_with_ty;
}
(** Handler definitions *)

and abstraction = pattern * computation
(** Abstractions that take one argument. *)

and abstraction_with_ty = pattern * Types.target_ty * computation

and letrec_abstraction =
  variable * Types.target_ty * Types.target_dirty * abstraction
(** LetRec Abstractions: function name, argument type, result type, pattern,
    and right-hand side *)

and abstraction2 = pattern * pattern * computation
(** Abstractions that take two arguments. *)

let abstraction_with_ty_to_abstraction (p, _, c) = (p, c)

type omega_ct =
  | TyOmega of (CoreTypes.TyCoercionParam.t * Types.ct_ty)
  | DirtOmega of (CoreTypes.DirtCoercionParam.t * Types.ct_dirt)
  | DirtyOmega of
      ((CoreTypes.TyCoercionParam.t * CoreTypes.DirtCoercionParam.t)
      * Types.ct_dirty)
  | SkelEq of skeleton * skeleton
  | TyParamHasSkel of (CoreTypes.TyParam.t * skeleton)

type toplevel = plain_toplevel * Location.t

and plain_toplevel =
  (* | Tydef of (CoreTypes.tyname, Params.t * Tctx.tydef) CoreTypes.assoc *)
  (* | TopLet of (pattern * computation) list * (variable * Scheme.ty_scheme) list *)
  (* | TopLetRec of (variable * abstraction) list * (variable * Scheme.ty_scheme) list *)
  (* | External of variable * Type.ty * string *)
  | DefEffect of effect
  | Computation of computation
  | Use of string
  | Reset
  | Help
  | Quit

(* ************************************************************************* *)
(*                         COERCION VARIABLES OF                             *)
(* ************************************************************************* *)

module TyCoercionParamSet = Set.Make (CoreTypes.TyCoercionParam)
module DirtCoercionParamSet = Set.Make (CoreTypes.DirtCoercionParam)

let rec tyCoVarsOfExpression : expression -> TyCoercionParamSet.t = function
  (*
type expression =
  | Var of variable
  | BuiltIn of string * int
  | Const of Const.t
  | Tuple of expression list
  | Record of (CoreTypes.Field.t, expression) Assoc.t
  | Variant of CoreTypes.Label.t * expression
  | Lambda of abstraction_with_ty
  | Effect of effect
  | Handler of handler
  | BigLambdaTy of CoreTypes.TyParam.t * skeleton * expression
  | BigLambdaDirt of CoreTypes.DirtParam.t * expression
  | BigLambdaSkel of CoreTypes.SkelParam.t * expression
  | CastExp of expression * ty_coercion
  | ApplyTyExp of expression * Types.target_ty
  | LambdaTyCoerVar of CoreTypes.TyCoercionParam.t * Types.ct_ty * expression
  | LambdaDirtCoerVar of CoreTypes.DirtCoercionParam.t * Types.ct_dirt * expression
  | ApplyDirtExp of expression * Types.dirt
  | ApplySkelExp of expression * Types.skeleton
  | ApplyTyCoercion of expression * ty_coercion
  | ApplyDirtCoercion of expression * dirt_coercion
*)
  | _ -> failwith __LOC__

and dirtCoVarsOfExpression : expression -> DirtCoercionParamSet.t = function
  (*
type expression =
  | Var of variable
  | BuiltIn of string * int
  | Const of Const.t
  | Tuple of expression list
  | Record of (CoreTypes.Field.t, expression) Assoc.t
  | Variant of CoreTypes.Label.t * expression
  | Lambda of abstraction_with_ty
  | Effect of effect
  | Handler of handler
  | BigLambdaTy of CoreTypes.TyParam.t * skeleton * expression
  | BigLambdaDirt of CoreTypes.DirtParam.t * expression
  | BigLambdaSkel of CoreTypes.SkelParam.t * expression
  | CastExp of expression * ty_coercion
  | ApplyTyExp of expression * Types.target_ty
  | LambdaTyCoerVar of CoreTypes.TyCoercionParam.t * Types.ct_ty * expression
  | LambdaDirtCoerVar of CoreTypes.DirtCoercionParam.t * Types.ct_dirt * expression
  | ApplyDirtExp of expression * Types.dirt
  | ApplySkelExp of expression * Types.skeleton
  | ApplyTyCoercion of expression * ty_coercion
  | ApplyDirtCoercion of expression * dirt_coercion
*)
  | _ -> failwith __LOC__

and tyCoVarsOfComputation : computation -> TyCoercionParamSet.t = function
  (*
and computation =
  | Value of expression
  | LetVal of expression * abstraction_with_ty
  | LetRec of (variable * Types.target_ty * expression) list * computation
  | Match of expression * abstraction list
  | Apply of expression * expression
  | Handle of expression * computation
  | Call of effect * expression * abstraction_with_ty
  | Op of effect * expression
  | Bind of computation * abstraction
  | CastComp of computation * dirty_coercion
  | CastComp_ty of computation * ty_coercion
  | CastComp_dirt of computation * dirt_coercion
*)
  | _ -> failwith __LOC__

and dirtCoVarsOfComputation : computation -> DirtCoercionParamSet.t = function
  (*
and computation =
  | Value of expression
  | LetVal of expression * abstraction_with_ty
  | LetRec of (variable * Types.target_ty * expression) list * computation
  | Match of expression * abstraction list
  | Apply of expression * expression
  | Handle of expression * computation
  | Call of effect * expression * abstraction_with_ty
  | Op of effect * expression
  | Bind of computation * abstraction
  | CastComp of computation * dirty_coercion
  | CastComp_ty of computation * ty_coercion
  | CastComp_dirt of computation * dirt_coercion
*)
  | _ -> failwith __LOC__

(*
let rec state_free_dirt_vars st =
  List.fold_right
    (fun (_, ty) acc ->
      Types.DirtParamSet.union (Types.fdvsOfTargetValTy ty) acc )
    st Types.DirtParamSet.empty


  | TyOmega of (CoreTypes.TyCoercionParam.t * Types.ct_ty)
  | DirtOmega of (CoreTypes.DirtCoercionParam.t * Types.ct_dirt)
*)

(* ************************************************************************* *)
(* ************************************************************************* *)

(* | TypeOf of computation *)

let abstraction p c : abstraction = (p, c)

let abstraction_with_ty p ty c : abstraction_with_ty = (p, ty, c)

let abstraction2 p1 p2 c : abstraction2 = (p1, p2, c)

let print_effect (eff, _) ppf =
  Print.print ppf "Effect_%t" (CoreTypes.Effect.print eff)

let print_variable = CoreTypes.Variable.print ~safe:true

let rec print_pattern ?max_level p ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match p with
  | PVar x -> print "%t" (print_variable x)
  | PAs (p, x) -> print "%t as %t" (print_pattern p) (print_variable x)
  | PConst c -> Const.print c ppf
  | PTuple lst -> Print.tuple print_pattern lst ppf
  | PRecord lst -> Print.record CoreTypes.Field.print print_pattern lst ppf
  | PVariant (lbl, p) ->
      print ~at_level:1 "(%t @[<hov>%t@])"
        (CoreTypes.Label.print lbl)
        (print_pattern p)
  | PNonbinding -> print "_"

let rec print_expression ?max_level e ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match e with
  | Var x -> print "%t" (print_variable x)
  | Const c -> print "%t" (Const.print c)
  | Tuple lst -> Print.tuple print_expression lst ppf
  | Record lst -> Print.record CoreTypes.Field.print print_expression lst ppf
  | Variant (lbl, e) ->
      print ~at_level:1 "%t %t" (CoreTypes.Label.print lbl) (print_expression e)
  | Lambda (x, t, c) ->
      print "fun (%t:%t) -> (%t)" (print_pattern x) (Types.print_target_ty t)
        (print_computation c)
  | Handler h ->
      print
        "{@[<hov> value_clause = (@[fun %t@]);@ effect_clauses = (fun (type a) \
         (type b) (x : (a, b) effect) ->\n\
        \             ((match x with %t) : a -> (b -> _ computation) -> _ \
         computation)) @]}"
        (print_abstraction_with_ty h.value_clause)
        (print_effect_clauses (Assoc.to_list h.effect_clauses))
  | Effect eff -> print ~at_level:2 "effect %t" (print_effect eff)
  | CastExp (e1, tc) ->
      print "(%t) |> [%t]" (print_expression e1) (print_ty_coercion tc)
  | LambdaTyCoerVar (p, (tty1, tty2), e) ->
      print "/\\(%t:%t<=%t).( %t ) "
        (CoreTypes.TyCoercionParam.print p)
        (Types.print_target_ty tty1)
        (Types.print_target_ty tty2)
        (print_expression e)
  | LambdaDirtCoerVar (p, (tty1, tty2), e) ->
      print "/\\(%t:%t<=%t).( %t )"
        (CoreTypes.DirtCoercionParam.print p)
        (Types.print_target_dirt tty1)
        (Types.print_target_dirt tty2)
        (print_expression e)
  | ApplyTyCoercion (e, tty) ->
      print ~at_level:1 "%t@ %t"
        (print_expression ~max_level:1 e)
        (print_ty_coercion tty)
  | ApplyDirtCoercion (e, tty) ->
      print ~at_level:1 "%t@ %t"
        (print_expression ~max_level:1 e)
        (print_dirt_coercion tty)

and print_computation ?max_level c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match c with
  | Apply (e1, e2) ->
      print ~at_level:1 "((%t)@ (%t))"
        (print_expression ~max_level:1 e1)
        (print_expression ~max_level:0 e2)
  | Value e -> print ~at_level:1 "value (%t)" (print_expression ~max_level:0 e)
  | Match (e, _resTy, []) ->
      print ~at_level:2 "(match %t with _ -> assert false)" (print_expression e)
  | Match (e, _resTy, lst) ->
      print ~at_level:2 "(match %t with @[<v>| %t@])" (print_expression e)
        (Print.cases print_abstraction lst)
  | Handle (e, c) ->
      print ~at_level:1 "handle %t %t"
        (print_expression ~max_level:0 e)
        (print_computation ~max_level:0 c)
  | LetRec (lst, c) ->
      print ~at_level:2 "let rec @[<hov>%t@] in %t"
        (Print.sequence " and " print_let_rec_abstraction lst)
        (print_computation c)
  | Call (eff, e, a) ->
      print ~at_level:1 "call (%t) (%t) ((@[fun %t@]))" (print_effect eff)
        (print_expression ~max_level:0 e)
        (print_abstraction_with_ty a)
  | Op (eff, e) ->
      print ~at_level:1 "(#%t %t)" (print_effect eff) (print_expression e)
  | Bind (c1, a) ->
      print ~at_level:2 "@[<hov>%t@ >>@ @[fun %t@]@]"
        (print_computation ~max_level:0 c1)
        (print_abstraction a)
  | CastComp (c1, dc) ->
      print " ( (%t) |> [%t] ) " (print_computation c1)
        (print_dirty_coercion dc)
  | CastComp_ty (c1, dc) ->
      print " ( (%t) |> [%t] )" (print_computation c1) (print_ty_coercion dc)
  | CastComp_dirt (c1, dc) ->
      print "( (%t) |> [%t])" (print_computation c1) (print_dirt_coercion dc)
  | LetVal (e1, (p, ty, c1)) ->
      print "let (%t = (%t)) in (%t)" (print_pattern p) (print_expression e1)
        (print_computation c1)

and print_ty_coercion ?max_level c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match c with
  | ReflTy p -> print "<%t>" (Types.print_target_ty p)
  | ArrowCoercion (tc, dc) ->
      print "%t -> %t" (print_ty_coercion tc) (print_dirty_coercion dc)
  | HandlerCoercion (dc1, dc2) ->
      print "%t ==> %t" (print_dirty_coercion dc1) (print_dirty_coercion dc2)
  | TyCoercionVar tcp -> print "%t " (CoreTypes.TyCoercionParam.print tcp)
  | SequenceTyCoer (tc1, tc2) ->
      print "%t ; %t" (print_ty_coercion tc1) (print_ty_coercion tc2)
  | PureCoercion dtyco -> print "pure(%t)" (print_dirty_coercion dtyco)
  | ApplyCoercion (t, []) -> print "%t" (CoreTypes.TyName.print t)
  | ApplyCoercion (t, [ c ]) ->
      print ~at_level:1 "%t %t"
        (print_ty_coercion ~max_level:1 c)
        (CoreTypes.TyName.print t)
  | ApplyCoercion (t, cs) ->
      print ~at_level:1 "(%t) %t"
        (Print.sequence ", " print_ty_coercion cs)
        (CoreTypes.TyName.print t)
  | TupleCoercion [] -> print "unit"
  | TupleCoercion cos ->
      print ~at_level:2 "@[<hov>%t@]"
        (Print.sequence (Symbols.times ()) (print_ty_coercion ~max_level:1) cos)
  | LeftArrow co -> print "fst(%t)" (print_ty_coercion co)
  | _ -> failwith "Not yet implemented __LOC__"

(* THE FOLLOWING ARE UNEXPECTED. SOMETHING MUST BE WRONG TO GET THEM.
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  | ForallTy of CoreTypes.TyParam.t * ty_coercion
  | ApplyTyCoer of ty_coercion * target_ty

  | ForallDirt of CoreTypes.DirtParam.t * ty_coercion
  | ApplyDirCoer of ty_coercion * dirt

  | QualTyCoer of ct_ty * ty_coercion
  | ApplyQualTyCoer of ty_coercion * ty_coercion

  | QualDirtCoer of ct_dirt * ty_coercion
  | ApplyQualDirtCoer of ty_coercion * dirt_coercion

  | ForallSkel of CoreTypes.SkelParam.t * ty_coercion
  | ApplySkelCoer of ty_coercion * skeleton
*)
and print_dirty_coercion ?max_level c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match c with
  | BangCoercion (tc, dirtc) ->
      print "%t ! %t" (print_ty_coercion tc) (print_dirt_coercion dirtc)
  | LeftHandler tyco -> print "fst(%t)" (print_ty_coercion tyco)
  | RightHandler tyco -> print "snd(%t)" (print_ty_coercion tyco)
  | RightArrow tyco -> print "snd(%t)" (print_ty_coercion tyco)
  | SequenceDirtyCoer (c1, c2) ->
      print "(%t;%t)" (print_dirty_coercion c1) (print_dirty_coercion c2)

and print_dirt_coercion ?max_level c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match c with
  | ReflDirt p -> print "<%t>" (Types.print_target_dirt p)
  | DirtCoercionVar tcp -> print "%t" (CoreTypes.DirtCoercionParam.print tcp)
  | Empty d -> print "Empty__(%t)" (Types.print_target_dirt d)
  | UnionDirt (eset, dc) ->
      print "{%t} U %t" (Types.print_effect_set eset) (print_dirt_coercion dc)
  | DirtCoercion dtyco -> print "dirtOf(%t)" (print_dirty_coercion dtyco)
  | SequenceDirtCoer (dco1, dco2) ->
      print "(%t;%t)" (print_dirt_coercion dco1) (print_dirt_coercion dco2)

and print_omega_ct ?max_level c ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match c with
  | TyOmega (p, (ty1, ty2)) ->
      print "%t: (%t =< %t)"
        (CoreTypes.TyCoercionParam.print p)
        (Types.print_target_ty ty1)
        (Types.print_target_ty ty2)
  | DirtOmega (p, (ty1, ty2)) ->
      print "%t: (%t =< %t)"
        (CoreTypes.DirtCoercionParam.print p)
        (Types.print_target_dirt ty1)
        (Types.print_target_dirt ty2)
  | DirtyOmega ((p1, p2), (dirty1, dirty2)) ->
      print "%t ! %t: (%t =< %t)"
        (CoreTypes.TyCoercionParam.print p1)
        (CoreTypes.DirtCoercionParam.print p2)
        (Types.print_target_dirty dirty1)
        (Types.print_target_dirty dirty2)
  | SkelEq (sk1, sk2) ->
      print "%t ~ %t" (Types.print_skeleton sk1) (Types.print_skeleton sk2)
  | TyParamHasSkel (tp, sk1) ->
      print "%t : %t" (CoreTypes.TyParam.print tp) (Types.print_skeleton sk1)

and print_effect_clauses eff_clauses ppf =
  let print ?at_level = Print.print ?at_level ppf in
  match eff_clauses with
  | [] -> print "| eff' -> fun arg k -> Call (eff', arg, k)"
  | (((_, (t1, t2)) as eff), a2) :: cases ->
      print ~at_level:1 "| %t -> %t %t" (print_effect eff)
        (print_abstraction2 a2)
        (print_effect_clauses cases)

and print_abstraction (p, c) ppf =
  Format.fprintf ppf "%t ->@;<1 2> %t" (print_pattern p) (print_computation c)

and print_abstraction_with_ty (p, tty, c) ppf =
  Format.fprintf ppf "%t:%t ->@;<1 2> %t" (print_pattern p)
    (Types.print_target_ty tty)
    (print_computation c)

and print_abstraction2 (p1, p2, c) ppf =
  Format.fprintf ppf "(fun %t %t -> %t)" (print_pattern p1) (print_pattern p2)
    (print_computation c)

and print_pure_abstraction (p, e) ppf =
  Format.fprintf ppf "%t ->@;<1 2> %t" (print_pattern p) (print_expression e)

and print_let_abstraction (p, c) ppf =
  Format.fprintf ppf "%t = %t" (print_pattern p) (print_computation c)

and print_top_let_abstraction (p, c) ppf =
  match c with
  | Value e ->
      Format.fprintf ppf "%t = %t" (print_pattern p)
        (print_expression ~max_level:0 e)
  | _ ->
      Format.fprintf ppf "%t = run %t" (print_pattern p)
        (print_computation ~max_level:0 c)

and print_let_rec_abstraction (f, arg_ty, res_ty, abs) ppf =
  Format.fprintf ppf "(%t : %t) %t" (print_variable f)
    (print_target_ty (Types.Arrow (arg_ty, res_ty)))
    (print_let_abstraction abs)

let backup_location loc locs =
  match loc with None -> Location.union locs | Some loc -> loc

let rec refresh_pattern sbst = function
  | PVar x ->
      let x' = CoreTypes.Variable.refresh x in
      (Assoc.update x x' sbst, PVar x')
  | PAs (p, x) ->
      let x' = CoreTypes.Variable.refresh x in
      let sbst, p' = refresh_pattern (Assoc.update x x' sbst) p in
      (sbst, PAs (p', x'))
  | PTuple ps ->
      let sbst, ps' =
        List.fold_right
          (fun p (sbst, ps') ->
            let sbst, p' = refresh_pattern sbst p in
            (sbst, p' :: ps'))
          ps (sbst, [])
      in
      (sbst, PTuple ps')
  | PRecord flds ->
      let sbst, flds' =
        Assoc.fold_right
          (fun (lbl, p) (sbst, flds') ->
            let sbst, p' = refresh_pattern sbst p in
            (sbst, Assoc.update lbl p' flds'))
          flds (sbst, Assoc.empty)
      in
      (sbst, PRecord flds')
  | PVariant (lbl, p) ->
      let sbst, p' = refresh_pattern sbst p in
      (sbst, PVariant (lbl, p'))
  | (PConst _ | PNonbinding) as p -> (sbst, p)

let rec refresh_expr sbst = function
  | Var x as e -> (
      match Assoc.lookup x sbst with Some x' -> Var x' | None -> e)
  | Lambda abs -> Lambda (refresh_abs_with_ty sbst abs)
  | Handler h -> Handler (refresh_handler sbst h)
  | Tuple es -> Tuple (List.map (refresh_expr sbst) es)
  | Record flds -> Record (Assoc.map (refresh_expr sbst) flds)
  | Variant (lbl, e) -> Variant (lbl, refresh_expr sbst e)
  | CastExp (e1, tyco) -> CastExp (refresh_expr sbst e1, tyco)
  | (Const _ | Effect _) as e -> e
  | LambdaTyCoerVar (tycovar, ct, e) -> failwith __LOC__
  | LambdaDirtCoerVar (dcovar, ct, e) ->
      (* TODO: refresh dco var *)
      LambdaDirtCoerVar (dcovar, ct, refresh_expr sbst e)
  | ApplyTyCoercion (e, tyco) -> ApplyTyCoercion (refresh_expr sbst e, tyco)
  | ApplyDirtCoercion (e, dco) -> ApplyDirtCoercion (refresh_expr sbst e, dco)

and refresh_comp sbst = function
  | Bind (c1, c2) -> Bind (refresh_comp sbst c1, refresh_abs sbst c2)
  | LetRec (li, c1) ->
      let new_xs, sbst' =
        List.fold_right
          (fun (x, _, _, _) (new_xs, sbst') ->
            let x' = CoreTypes.Variable.refresh x in
            (x' :: new_xs, Assoc.update x x' sbst'))
          li ([], sbst)
      in
      let li' =
        List.map
          (fun (x', (argTy, resTy, abs)) -> (x', argTy, resTy, abs))
          (List.combine new_xs
             (List.map
                (fun (_, argTy, resTy, abs) ->
                  (argTy, resTy, refresh_abs sbst' abs))
                li))
      in
      LetRec (li', refresh_comp sbst' c1)
  | Match (e, resTy, li) ->
      Match (refresh_expr sbst e, resTy, List.map (refresh_abs sbst) li)
  | Apply (e1, e2) -> Apply (refresh_expr sbst e1, refresh_expr sbst e2)
  | Handle (e, c) -> Handle (refresh_expr sbst e, refresh_comp sbst c)
  | Call (eff, e, a) ->
      Call (eff, refresh_expr sbst e, refresh_abs_with_ty sbst a)
  | Value e -> Value (refresh_expr sbst e)
  | CastComp (c, dtyco) -> CastComp (refresh_comp sbst c, dtyco)

and refresh_handler sbst h =
  {
    effect_clauses = Assoc.map (refresh_abs2 sbst) h.effect_clauses;
    value_clause = refresh_abs_with_ty sbst h.value_clause;
  }

and refresh_abs sbst (p, c) =
  let sbst, p' = refresh_pattern sbst p in
  (p', refresh_comp sbst c)

and refresh_abs_with_ty sbst (p, ty, c) =
  let sbst, p' = refresh_pattern sbst p in
  (p', ty, refresh_comp sbst c)

and refresh_abs2 sbst (p1, p2, c) =
  let sbst, p1' = refresh_pattern sbst p1 in
  let sbst, p2' = refresh_pattern sbst p2 in
  let c' = refresh_comp sbst c in
  (p1', p2', c')

let rec subst_expr sbst = function
  | Var x as e -> ( match Assoc.lookup x sbst with Some e' -> e' | None -> e)
  | Lambda abs -> Lambda (subst_abs_with_ty sbst abs)
  | Handler h -> Handler (subst_handler sbst h)
  | Tuple es -> Tuple (List.map (subst_expr sbst) es)
  | Record flds -> Record (Assoc.map (subst_expr sbst) flds)
  | Variant (lbl, e) -> Variant (lbl, subst_expr sbst e)
  | (Const _ | Effect _) as e -> e
  | CastExp (e, tyco) -> CastExp (subst_expr sbst e, tyco)
  | LambdaTyCoerVar (tycovar, ct, e) ->
      LambdaTyCoerVar (tycovar, ct, subst_expr sbst e)
  | LambdaDirtCoerVar (dcovar, ct, e) ->
      LambdaDirtCoerVar (dcovar, ct, subst_expr sbst e)
  | ApplyTyCoercion (e, tyco) -> ApplyTyCoercion (subst_expr sbst e, tyco)
  | ApplyDirtCoercion (e, dco) -> ApplyDirtCoercion (subst_expr sbst e, dco)

and subst_comp sbst = function
  | Bind (c1, c2) -> Bind (subst_comp sbst c1, subst_abs sbst c2)
  | LetVal (e1, (x, ty, c1)) ->
      (* XXX Should we check that x does not appear in sbst? *)
      LetVal (subst_expr sbst e1, (x, ty, subst_comp sbst c1))
  | LetRec (li, c1) ->
      let li' =
        List.map
          (fun (x, argTy, resTy, abs) ->
            (* XXX Should we check that x does not appear in sbst? *)
            (x, argTy, resTy, subst_abs sbst abs))
          li
      in
      LetRec (li', subst_comp sbst c1)
  | Match (e, resTy, li) ->
      Match (subst_expr sbst e, resTy, List.map (subst_abs sbst) li)
  | Apply (e1, e2) -> Apply (subst_expr sbst e1, subst_expr sbst e2)
  | Handle (e, c) -> Handle (subst_expr sbst e, subst_comp sbst c)
  | Call (eff, e, a) -> Call (eff, subst_expr sbst e, subst_abs_with_ty sbst a)
  | Value e -> Value (subst_expr sbst e)
  | CastComp (c, dtyco) -> CastComp (subst_comp sbst c, dtyco)
  | other ->
      Print.debug "About to fail (subst_comp): %t" (print_computation other);
      failwith __LOC__

and subst_handler sbst h =
  {
    effect_clauses = Assoc.map (subst_abs2 sbst) h.effect_clauses;
    value_clause = subst_abs_with_ty sbst h.value_clause;
  }

and subst_abs sbst (p, c) =
  (* XXX We should assert that p & sbst have disjoint variables *)
  (p, subst_comp sbst c)

and subst_abs_with_ty sbst (p, ty, c) =
  (* XXX We should assert that p & sbst have disjoint variables *)
  (p, ty, subst_comp sbst c)

and subst_abs2 sbst (p1, p2, c) =
  (* XXX We should assert that p1, p2 & sbst have disjoint variables *)
  (p1, p2, subst_comp sbst c)

let assoc_equal eq flds flds' : bool =
  let rec equal_fields flds =
    match flds with
    | [] -> true
    | (f, x) :: flds -> (
        match Assoc.lookup f flds' with
        | Some x' when eq x x' -> equal_fields flds
        | _ -> false)
  in
  Assoc.length flds = Assoc.length flds' && equal_fields (Assoc.to_list flds)

let rec make_equal_pattern eqvars p p' =
  match (p, p') with
  | PVar x, PVar x' -> Some ((x, x') :: eqvars)
  | PAs (p, x), PAs (p', x') ->
      option_map
        (fun eqvars -> (x, x') :: eqvars)
        (make_equal_pattern eqvars p p')
  | PTuple ps, PTuple ps' ->
      List.fold_right2
        (fun p p' -> function
          | Some eqvars' -> make_equal_pattern eqvars' p p'
          | None -> None)
        ps ps' (Some eqvars)
  | PConst cst, PConst cst' when Const.equal cst cst' -> Some eqvars
  | PNonbinding, PNonbinding -> Some eqvars
  | PVariant (lbl, p), PVariant (lbl', p') when lbl = lbl' ->
      make_equal_pattern eqvars p p'
  | _, _ -> None

let rec alphaeq_expr eqvars e e' =
  match (e, e') with
  | Var x, Var y -> List.mem (x, y) eqvars || CoreTypes.Variable.compare x y = 0
  | Lambda a, Lambda a' -> alphaeq_abs_with_ty eqvars a a'
  | Handler h, Handler h' -> alphaeq_handler eqvars h h'
  | Tuple es, Tuple es' -> List.for_all2 (alphaeq_expr eqvars) es es'
  | Record flds, Record flds' -> assoc_equal (alphaeq_expr eqvars) flds flds'
  | Variant (lbl, e), Variant (lbl', e') ->
      lbl = lbl' && alphaeq_expr eqvars e e'
  | Const cst, Const cst' -> Const.equal cst cst'
  | Effect eff, Effect eff' -> eff = eff'
  | ApplyDirtCoercion (e, dco), ApplyDirtCoercion (e', dco') ->
      dco = dco' && alphaeq_expr eqvars e e'
  | _, _ -> false

and alphaeq_comp eqvars c c' =
  match (c, c') with
  | Bind (c1, c2), Bind (c1', c2') ->
      alphaeq_comp eqvars c1 c1' && alphaeq_abs eqvars c2 c2'
  | LetRec (li, c1), LetRec (li', c1') ->
      (* XXX Not yet implemented *)
      false
  | Match (e, _resTy1, li), Match (e', _resTy2, li') ->
      alphaeq_expr eqvars e e' && List.for_all2 (alphaeq_abs eqvars) li li'
  | Apply (e1, e2), Apply (e1', e2') ->
      alphaeq_expr eqvars e1 e1' && alphaeq_expr eqvars e2 e2'
  | Handle (e, c), Handle (e', c') ->
      alphaeq_expr eqvars e e' && alphaeq_comp eqvars c c'
  (* | Call (eff, e, a), Call (eff', e', a') ->
     eff = eff' && alphaeq_expr eqvars e e' && alphaeq_abs eqvars a a' *)
  | Value e, Value e' -> alphaeq_expr eqvars e e'
  | _, _ -> false

and alphaeq_handler eqvars h h' =
  alphaeq_abs_with_ty eqvars h.value_clause h'.value_clause
  && Assoc.length h.effect_clauses = Assoc.length h'.effect_clauses
  && List.for_all
       (fun (effect, abs2) ->
         match Assoc.lookup effect h'.effect_clauses with
         | Some abs2' -> alphaeq_abs2 eqvars abs2 abs2'
         | None -> false)
       (Assoc.to_list h.effect_clauses)

(*   assoc_equal (alphaeq_abs2 eqvars) h.effect_clauses h'.effect_clauses &&
  alphaeq_abs eqvars h.value_clause h'.value_clause *)
and alphaeq_abs eqvars (p, c) (p', c') =
  match make_equal_pattern eqvars p p' with
  | Some eqvars' -> alphaeq_comp eqvars' c c'
  | None -> false

and alphaeq_abs_with_ty eqvars (p, ty, c) (p', ty', c') =
  match make_equal_pattern eqvars p p' with
  | Some eqvars' -> alphaeq_comp eqvars' c c'
  | None -> false

and alphaeq_abs2 eqvars (p1, p2, c) (p1', p2', c') =
  (* alphaeq_abs eqvars (a22a a2) (a22a a2') *)
  match make_equal_pattern eqvars p1 p1' with
  | Some eqvars' -> (
      match make_equal_pattern eqvars' p2 p2' with
      | Some eqvars'' -> alphaeq_comp eqvars'' c c'
      | None -> false)
  | None -> false

let pattern_match p e =
  (* XXX The commented out part checked that p and e had matching types *)
  (* let _, ty_e, constraints_e = e.scheme
     and _, ty_p, constraints_p = p.scheme in
     let constraints =
       Constraints.union constraints_e constraints_p |>
       Constraints.add_ty_constraint ~loc:e.location ty_e ty_p
     in
     ignore constraints; *)
  let rec extend_subst p e sbst =
    match (p, e) with
    | PVar x, e -> Assoc.update x e sbst
    | PAs (p, x), e' ->
        let sbst = extend_subst p e sbst in
        Assoc.update x e' sbst
    | PNonbinding, _ -> sbst
    | PTuple ps, Tuple es -> List.fold_right2 extend_subst ps es sbst
    | PRecord ps, Record es ->
        let rec extend_record ps es sbst =
          match ps with
          | [] -> sbst
          | (f, p) :: ps ->
              let e = List.assoc f es in
              extend_record ps es (extend_subst p e sbst)
        in
        extend_record (Assoc.to_list ps) (Assoc.to_list es) sbst
    | PVariant (lbl, p), Variant (lbl', e) when lbl = lbl' ->
        extend_subst p e sbst
    | PConst c, Const c' when Const.equal c c' -> sbst
    | _, _ -> assert false
  in
  extend_subst p e Assoc.empty

let ( @@@ ) (inside1, outside1) (inside2, outside2) =
  (inside1 @ inside2, outside1 @ outside2)

let ( --- ) (inside, outside) bound =
  let remove_bound xs = List.filter (fun x -> not (List.mem x bound)) xs in
  (remove_bound inside, remove_bound outside)

let concat_vars vars = List.fold_right ( @@@ ) vars ([], [])

let rec free_vars_comp c =
  match c with
  | Value e -> free_vars_expr e
  | LetVal (e, abs) -> free_vars_expr e @@@ free_vars_abs_with_ty abs
  | LetRec (li, c1) ->
      let xs, vars =
        List.fold_right
          (fun (x, argTy, resTy, abs) (xs, vars) ->
            (x :: xs, free_vars_abs abs @@@ vars))
          li
          ([], free_vars_comp c1)
      in
      vars --- xs
  | Match (e, _resTy, li) ->
      free_vars_expr e @@@ concat_vars (List.map free_vars_abs li)
  | Apply (e1, e2) -> free_vars_expr e1 @@@ free_vars_expr e2
  | Handle (e, c1) -> free_vars_expr e @@@ free_vars_comp c1
  | Call (_, e1, a1) -> free_vars_expr e1 @@@ free_vars_abs_with_ty a1
  | Op (_, e) -> free_vars_expr e
  | Bind (c1, a1) -> free_vars_comp c1 @@@ free_vars_abs a1
  | CastComp (c1, dtyco) -> free_vars_comp c1
  | CastComp_ty (c1, _) -> free_vars_comp c1
  | CastComp_dirt (c1, _) -> free_vars_comp c1

and free_vars_expr e =
  match e with
  | Var v -> ([], [ v ])
  | Tuple es -> concat_vars (List.map free_vars_expr es)
  | Lambda a -> free_vars_abs_with_ty a
  | Handler h -> free_vars_handler h
  | Record flds ->
      Assoc.values_of flds |> List.map free_vars_expr |> concat_vars
  | Variant (_, e) -> free_vars_expr e
  | CastExp (e', tyco) -> free_vars_expr e'
  | Effect _ | Const _ -> ([], [])
  | LambdaTyCoerVar _ -> failwith __LOC__
  | LambdaDirtCoerVar _ -> failwith __LOC__
  | ApplyTyCoercion (e, tyco) -> free_vars_expr e
  | ApplyDirtCoercion (e, dco) -> free_vars_expr e

and free_vars_handler h =
  free_vars_abs_with_ty h.value_clause
  @@@ (Assoc.values_of h.effect_clauses
      |> List.map free_vars_abs2 |> concat_vars)

and free_vars_finally_handler (h, finally_clause) =
  free_vars_handler h @@@ free_vars_abs finally_clause

and free_vars_abs (p, c) =
  let inside, outside = free_vars_comp c --- pattern_vars p in
  (inside @ outside, [])

and free_vars_abs_with_ty (p, _, c) =
  let inside, outside = free_vars_comp c --- pattern_vars p in
  (inside @ outside, [])

and free_vars_abs2 (p1, p2, c) =
  let inside, outside =
    free_vars_comp c --- pattern_vars p2 --- pattern_vars p1
  in
  (inside @ outside, [])

let occurrences x (inside, outside) =
  let count ys = List.length (List.filter (fun y -> x = y) ys) in
  (count inside, count outside)

let fresh_dirt_coer cons =
  let param = CoreTypes.DirtCoercionParam.fresh () in
  (DirtCoercionVar param, DirtOmega (param, cons))

let fresh_ty_with_skel skel =
  let ty_var = CoreTypes.TyParam.fresh () in
  (Types.TyParam ty_var, TyParamHasSkel (ty_var, skel))

let fresh_dirty_with_skel skel =
  let ty, cons = fresh_ty_with_skel skel and drt = Types.fresh_dirt () in
  ((ty, drt), cons)

let fresh_ty_with_fresh_skel () =
  let skel_var = CoreTypes.SkelParam.fresh () in
  fresh_ty_with_skel (Types.SkelParam skel_var)

let fresh_dirty_with_fresh_skel () =
  let skel_var = CoreTypes.SkelParam.fresh () in
  fresh_dirty_with_skel (Types.SkelParam skel_var)

let fresh_ty_coer cons =
  let param = CoreTypes.TyCoercionParam.fresh () in
  (TyCoercionVar param, TyOmega (param, cons))

let fresh_dirty_coer cons =
  let ty_param = CoreTypes.TyCoercionParam.fresh () in
  let dirt_param = CoreTypes.DirtCoercionParam.fresh () in
  let coer =
    BangCoercion (TyCoercionVar ty_param, DirtCoercionVar dirt_param)
  in
  (coer, DirtyOmega ((ty_param, dirt_param), cons))

let cast_expression e ty1 ty2 =
  let omega, cons = fresh_ty_coer (ty1, ty2) in
  (CastExp (e, omega), cons)

let cast_computation c dirty1 dirty2 =
  let omega, cons = fresh_dirty_coer (dirty1, dirty2) in
  (CastComp (c, omega), cons)

(* ************************************************************************* *)
(*                         FREE VARIABLE COMPUTATION                         *)
(* ************************************************************************* *)

(* Compute the free type variables of a constraint *)
let ftvsOfOmegaCt : omega_ct -> TyParamSet.t = function
  | TyOmega (_, ct) -> ftvsOfValTyCt ct
  | DirtOmega (_, _) -> TyParamSet.empty
  | DirtyOmega ((_, _), ct) ->
      ftvsOfCmpTyCt ct (* GEORGE: Is anyone using "DirtyOmega"? *)
  | SkelEq (_, _) -> TyParamSet.empty
  | TyParamHasSkel (a, _) -> TyParamSet.singleton a

(* GEORGE: This is up to debate *)

(* Compute the free dirt variables of a constraint *)
let fdvsOfOmegaCt : omega_ct -> DirtParamSet.t = function
  | TyOmega (_, ct) -> fdvsOfValTyCt ct
  | DirtOmega (_, ct) -> fdvsOfDirtCt ct
  | DirtyOmega ((_, _), ct) ->
      fdvsOfCmpTyCt ct (* GEORGE: Is anyone using "DirtyOmega"? *)
  | SkelEq (_, _) -> DirtParamSet.empty
  | TyParamHasSkel (_, _) -> DirtParamSet.empty

(* ************************************************************************* *)

(* free dirt variables in target terms *)

let rec free_dirt_vars_ty_coercion = function
  | ReflTy ty -> fdvsOfTargetValTy ty
  | ArrowCoercion (tc, dc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_ty_coercion tc)
        (free_dirt_vars_dirty_coercion dc)
  | HandlerCoercion (dc1, dc2) ->
      Types.DirtParamSet.union
        (free_dirt_vars_dirty_coercion dc1)
        (free_dirt_vars_dirty_coercion dc2)
  | TyCoercionVar tcp -> DirtParamSet.empty
  | SequenceTyCoer (tc1, tc2) ->
      Types.DirtParamSet.union
        (free_dirt_vars_ty_coercion tc1)
        (free_dirt_vars_ty_coercion tc2)
  | TupleCoercion tcs ->
      List.fold_left
        (fun free tc ->
          Types.DirtParamSet.union free (free_dirt_vars_ty_coercion tc))
        Types.DirtParamSet.empty tcs
  | LeftArrow tc -> free_dirt_vars_ty_coercion tc
  | PureCoercion dc -> free_dirt_vars_dirty_coercion dc
  | QualTyCoer (ctty, tc) -> free_dirt_vars_ty_coercion tc
  | QualDirtCoer (ctd, tc) -> free_dirt_vars_ty_coercion tc
  | ApplyQualTyCoer (tc1, tc2) ->
      Types.DirtParamSet.union
        (free_dirt_vars_ty_coercion tc1)
        (free_dirt_vars_ty_coercion tc2)
  | ApplyQualDirtCoer (tc, dc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_ty_coercion tc)
        (free_dirt_vars_dirt_coercion dc)

and free_dirt_vars_dirt_coercion = function
  | ReflDirt d -> fdvsOfDirt d
  | DirtCoercionVar dcv -> Types.DirtParamSet.empty
  | Empty d -> fdvsOfDirt d
  | UnionDirt (_, dc) -> free_dirt_vars_dirt_coercion dc
  | SequenceDirtCoer (dc1, dc2) ->
      Types.DirtParamSet.union
        (free_dirt_vars_dirt_coercion dc1)
        (free_dirt_vars_dirt_coercion dc2)
  | DirtCoercion dc -> free_dirt_vars_dirty_coercion dc

and free_dirt_vars_dirty_coercion = function
  | BangCoercion (tc, dc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_ty_coercion tc)
        (free_dirt_vars_dirt_coercion dc)
  | RightArrow tc -> free_dirt_vars_ty_coercion tc
  | RightHandler tc -> free_dirt_vars_ty_coercion tc
  | LeftHandler tc -> free_dirt_vars_ty_coercion tc
  | SequenceDirtyCoer (dc1, dc2) ->
      Types.DirtParamSet.union
        (free_dirt_vars_dirty_coercion dc1)
        (free_dirt_vars_dirty_coercion dc2)

let rec free_dirt_vars_expression e =
  match e with
  | Var _ -> DirtParamSet.empty
  | Const _ -> DirtParamSet.empty
  | Tuple es ->
      List.fold_left
        (fun free e -> DirtParamSet.union free (free_dirt_vars_expression e))
        DirtParamSet.empty es
  | Record _ -> failwith __LOC__
  | Variant (_, e) -> free_dirt_vars_expression e
  | Lambda abs -> free_dirt_vars_abstraction_with_ty abs
  | Effect _ -> DirtParamSet.empty
  | Handler h -> free_dirt_vars_abstraction_with_ty h.value_clause
  | CastExp (e, tc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_expression e)
        (free_dirt_vars_ty_coercion tc)
  | LambdaTyCoerVar (tcp, ctty, e) -> free_dirt_vars_expression e
  | LambdaDirtCoerVar (dcp, ctd, e) -> free_dirt_vars_expression e
  | ApplyTyCoercion (e, tc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_expression e)
        (free_dirt_vars_ty_coercion tc)
  | ApplyDirtCoercion (e, dc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_expression e)
        (free_dirt_vars_dirt_coercion dc)

and free_dirt_vars_computation c =
  match c with
  | Value e -> free_dirt_vars_expression e
  | LetVal (e, abs) ->
      Types.DirtParamSet.union
        (free_dirt_vars_expression e)
        (free_dirt_vars_abstraction_with_ty abs)
  | LetRec ([ (f, argTy, resTy, abs) ], c) ->
      Types.DirtParamSet.union
        (Types.DirtParamSet.union (fdvsOfTargetValTy argTy)
           (fdvsOfTargetCmpTy resTy))
        (free_dirt_vars_abstraction abs)
      |> Types.DirtParamSet.union (free_dirt_vars_computation c)
  | Match (e, resTy, cases) ->
      List.fold_left
        (fun free case ->
          DirtParamSet.union free (free_dirt_vars_abstraction case))
        (DirtParamSet.union
           (free_dirt_vars_expression e)
           (fdvsOfTargetCmpTy resTy))
        cases
  | Apply (e1, e2) ->
      Types.DirtParamSet.union
        (free_dirt_vars_expression e1)
        (free_dirt_vars_expression e2)
  | Handle (e, c) ->
      Types.DirtParamSet.union
        (free_dirt_vars_expression e)
        (free_dirt_vars_computation c)
  | Call (_, e, awty) -> failwith __LOC__
  | Op (_, e) -> free_dirt_vars_expression e
  | Bind (c, a) ->
      Types.DirtParamSet.union
        (free_dirt_vars_computation c)
        (free_dirt_vars_abstraction a)
  | CastComp (c, dc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_computation c)
        (free_dirt_vars_dirty_coercion dc)
  | CastComp_ty (c, tc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_computation c)
        (free_dirt_vars_ty_coercion tc)
  | CastComp_dirt (c, dc) ->
      Types.DirtParamSet.union
        (free_dirt_vars_computation c)
        (free_dirt_vars_dirt_coercion dc)

and free_dirt_vars_abstraction (_, c) = free_dirt_vars_computation c

and free_dirt_vars_abstraction_with_ty (_, ty, c) =
  Types.DirtParamSet.union (fdvsOfTargetValTy ty) (free_dirt_vars_computation c)

let rec get_skel_vars_from_constraints = function
  | [] -> []
  | TyParamHasSkel (_, Types.SkelParam sv) :: xs ->
      sv :: get_skel_vars_from_constraints xs
  | _ :: xs -> get_skel_vars_from_constraints xs

(* Get all constraints of the form (alpha : skelvar) from a bag of constraints *)
(* (CoreTypes.TyParam.t, CoreTypes.SkelParam.t) *)
let rec getSkelVarAnnotationsFromCs = function
  | [] -> []
  | TyParamHasSkel (alpha, Types.SkelParam sv) :: cs ->
      (alpha, sv) :: getSkelVarAnnotationsFromCs cs
  | _ :: cs -> getSkelVarAnnotationsFromCs cs

(* Get all constraints of the form (alpha : skeleton) from a bag of constraints *)
(* (CoreTypes.TyParam.t, skeleton) *)
let rec getSkelAnnotationsFromCs = function
  | [] -> []
  | TyParamHasSkel (alpha, skeleton) :: cs ->
      (alpha, skeleton) :: getSkelAnnotationsFromCs cs
  | _ :: cs -> getSkelAnnotationsFromCs cs

let rec apply_sub_to_type ty_subs dirt_subs ty =
  match ty with
  | Types.TyParam p -> (
      match Assoc.lookup p ty_subs with
      | Some p' -> Types.TyParam p'
      | None -> ty)
  | Types.Arrow (a, (b, d)) ->
      Types.Arrow
        ( apply_sub_to_type ty_subs dirt_subs a,
          (apply_sub_to_type ty_subs dirt_subs b, apply_sub_to_dirt dirt_subs d)
        )
  | Types.Tuple ty_list ->
      Types.Tuple
        (List.map (fun x -> apply_sub_to_type ty_subs dirt_subs x) ty_list)
  | Types.Handler ((a, b), (c, d)) ->
      Types.Handler
        ( (apply_sub_to_type ty_subs dirt_subs a, apply_sub_to_dirt dirt_subs b),
          (apply_sub_to_type ty_subs dirt_subs c, apply_sub_to_dirt dirt_subs d)
        )
  | Types.PrimTy _ -> ty
  | Types.Apply (ty_name, tys) ->
      Types.Apply (ty_name, List.map (apply_sub_to_type ty_subs dirt_subs) tys)
  | _ -> failwith __LOC__

and apply_sub_to_dirt dirt_subs drt =
  match drt.row with
  | Types.ParamRow p -> (
      match Assoc.lookup p dirt_subs with
      | Some p' -> { drt with row = Types.ParamRow p' }
      | None -> drt)
  | Types.EmptyRow -> drt
