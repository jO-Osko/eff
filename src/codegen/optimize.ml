open Types
open Typed

type state =
  { fuel: int ref
  ; tc_state: TypeChecker.state
  ; recursive_functions:
      (variable, Types.target_ty * expression) Assoc.t
  ; knot_functions: (variable, expression * handler * variable) Assoc.t
  }

let inititial_state =
  { fuel= ref !Config.optimization_fuel
  ; tc_state= TypeChecker.initial_state
  ; recursive_functions= Assoc.empty
  ; knot_functions= Assoc.empty }


let extend_rec_fun st f ty e =
  {st with recursive_functions= Assoc.update f (ty, e) st.recursive_functions}


let extend_var_type st t_var ty =
  {st with tc_state= TypeChecker.extend_var_types st.tc_state t_var ty}


and extend_pat_type st p ty =
  {st with tc_state=TypeChecker.extendPatternTypes st.tc_state p ty}


let extend_ty_params st ty_var =
  {st with tc_state= TypeChecker.extend_ty_params st.tc_state ty_var}


let extend_dirt_params st t_var =
  {st with tc_state= TypeChecker.extend_dirt_params st.tc_state t_var}


let extend_skel_params st t_var =
  {st with tc_state= TypeChecker.extend_skel_params st.tc_state t_var}


let extend_ty_coer_types st omega ct =
  {st with tc_state= TypeChecker.extend_ty_coer_types st.tc_state omega ct}


let extend_dirt_coer_types st omega ct =
  {st with tc_state= TypeChecker.extend_dirt_coer_types st.tc_state omega ct}


let extend_ty_param_skeletons st omega ct =
  {st with tc_state= TypeChecker.extend_ty_param_skeletons st.tc_state omega ct}


let refresh_expr e =
  let res = Typed.refresh_expr Assoc.empty e in
  Print.debug "refresh_expr  : %t" (Typed.print_expression e) ;
  Print.debug "refresh_expr'd: %t" (Typed.print_expression res) ;
  res


let refresh_abs a = Typed.refresh_abs Assoc.empty a

let refresh_abs_with_ty a = Typed.refresh_abs_with_ty Assoc.empty a

let refresh_abs2 a2 =
  let res = Typed.refresh_abs2 Assoc.empty a2 in
  Print.debug "refresh_abs2  : %t" (Typed.print_abstraction2 a2) ;
  Print.debug "refresh_abs2'd: %t" (Typed.print_abstraction2 res) ;
  res


let is_relatively_pure st c h =
  match TypeChecker.typeOfComputation st.tc_state c with
  | ty, {Types.effect_set= ops; Types.row= Types.EmptyRow} ->
      let handled_ops =
        EffectSet.of_list
          (List.map (fun ((eff, _), _) -> eff) (Assoc.to_list h.effect_clauses))
      in
      Print.debug "is_relatively_pure: %t:  %t vs %t"
        (Typed.print_computation c)
        (Types.print_effect_set handled_ops)
        (Types.print_effect_set ops) ;
      if EffectSet.is_empty (EffectSet.inter handled_ops ops) then
        let Types.Handler (_, (_, output_dirt)) =
          TypeChecker.type_of_handler st.tc_state h
        in
        match output_dirt with
        | {Types.effect_set= ops'; Types.row= Types.EmptyRow} ->
            Some
              (BangCoercion
                 ( ReflTy ty
                 , UnionDirt
                     (*( EffectSet.inter ops ops'*)
                     ( ops
                     , Empty (Types.closed_dirt (EffectSet.diff ops' ops)) ) ))
        | {Types.effect_set= ops'; Types.row= Types.ParamRow var} ->
            Some
              (BangCoercion
                 ( ReflTy ty
                 , UnionDirt
                     (*( EffectSet.inter ops ops'*)
                     ( ops
                     , Empty
                         { Types.effect_set= EffectSet.diff ops' ops
                         ; Types.row= Types.ParamRow var } ) ))
      else None
  | _, _ -> None


(* var can be instantiated to anything *)

let is_atomic e = match e with Var _ -> true | Const _ -> true | _ -> false

type inlinability =
  (* Pattern variables occur more than once or inside a binder *)
  | NotInlinable
  (* Pattern variables are not present in the body *)
  | NotPresent
  (* Pattern variables occur each at most once outside a binder *)
  | Inlinable

let applicable_pattern p vars =
  let rec check_variables = function
    | [] -> NotPresent
    | x :: xs ->
        let inside_occ, outside_occ = Typed.occurrences x vars in
        if inside_occ > 0 || outside_occ > 1 then NotInlinable
        else
          match check_variables xs with
          | NotPresent -> if outside_occ = 0 then NotPresent else Inlinable
          | inlinability -> inlinability
  in
  check_variables (Typed.pattern_vars p)



(* Try to specialize a recursive function f with continuation cont.
 * If the body is an abstraction, substitute all applications f v of f in cont with
 * a new variable f'. Then prefix let-bindings f' = f v to cont for all
 * substitutions.
 * `specialize_letrec` first checks if the LetRec has the right form to be
 * specialized. The actual specialization is done in `specialize`.
 * The specialize_comp/expr/abs functions traverse the syntax tree, searching
 * for abstractions and applications. They create the specializations, add them
 * to the assoc and substitute a fresh variable. *)
let rec specialize_letrec
    ( cont : Typed.computation )
    ( (fvar : Typed.variable)
    , (argty : Types.target_ty)
    , ((resty, resdt) : Types.target_dirty)
    , ((p,fbody_c) : Typed.abstraction)
    )
  : Typed.computation =
  match fbody_c with
  | Value fbody -> (
      match (resty,fbody) with
      | ((TySchemeTy _)  , (BigLambdaTy _))       -> specialize fvar fbody resty cont
      | ((TySchemeDirt _), (BigLambdaDirt _))     -> specialize fvar fbody resty cont
      | ((TySchemeSkel _), (BigLambdaSkel _))     -> specialize fvar fbody resty cont
      | ((QualTy _)      , (LambdaTyCoerVar _))   -> specialize fvar fbody resty cont
      | ((QualDirt _)    , (LambdaDirtCoerVar _)) -> specialize fvar fbody resty cont
      | _ -> Print.debug "SPEC: letrec body is not an abstraction"; cont
    )
  | _ -> Print.debug "SPEC: letrec body is not a value"; cont

and specialize
  (fvar: Typed.variable)
  (fbody : Typed.expression)
  (resty : Types.target_ty)
  ( cont : Typed.computation )
  : Typed.computation =
  (* Define a referenced map that holds the substitutions. *)
  let assoc = ref Assoc.empty in

  let specialize_app (e:Typed.expression) : Typed.variable =
    (* check if the specialized version already exists *)
    match Assoc.lookup e !assoc with
    | Some f' -> f'            (* already exists, substituting *)
    | None -> (                     (* creating new specialization *)
      (* create a new variable name fv' *)
      let f' = CoreTypes.Variable.refresh fvar in
      (* add it to mapping *)
      assoc := Assoc.update e f' !assoc;
      f')
  in

  (* The following are tree traversal functions that look for the specializations *)
  (* Specialize computations. *)
  let rec specialize_comp c = 
    match c with 
    | Value e1 -> 
        let e1' = specialize_expr e1 in
        Value e1'
    | LetVal (e1,a1) ->
        let e1' = specialize_expr e1 in
        let a1' = specialize_abs_with_ty a1 in
        LetVal (e1',a1')
    | LetRec (bs1,c1) ->
        let bs1' = List.map (fun (p,ty,dty,a1) -> (p,ty,dty,specialize_abs a1)) bs1 in
        let c1' = specialize_comp c1 in
        LetRec (bs1',c1')
    | Match (e1,dty,abs1) ->
        let e1' = specialize_expr e1 in
        let abs1' = List.map (specialize_abs ) abs1 in 
        Match (e1',dty,abs1')
    | Apply (e1,e2) ->
        let e1' = specialize_expr e1 in
        let e2' = specialize_expr e2 in
        Apply (e1',e2')
    | Handle (e1,c1) ->
        let e1' = specialize_expr e1 in
        let c1' = specialize_comp c1 in
        Handle (e1',c1')
    | Call (eff,e1,a1) ->
        let e1' = specialize_expr e1 in
        let a1' = specialize_abs_with_ty a1 in
        Call (eff,e1',a1')
    | Op (eff,e1) -> 
        let e1' = specialize_expr e1 in
        Op (eff,e1') 
    | Bind (c1,a1) -> 
        let c1' = specialize_comp c1 in
        let a1' = specialize_abs a1 in
        Bind (c1',a1') 
    | CastComp (c1,dtyco) -> 
        let c1' = specialize_comp c1 in
        CastComp (c1',dtyco) 
    | CastComp_ty (c1,tyco) -> 
        let c1'  = specialize_comp c1  in
        CastComp_ty (c1',tyco) 
    | CastComp_dirt (c1,dco) -> 
        let c1' = specialize_comp c1 in
        CastComp_dirt (c1',dco) 

  and specialize_expr (e:expression) : expression = 
    match e with
    | Var _ -> e
    | BuiltIn _ -> e
    | Const _ -> e
    | Tuple es -> 
        let es' = List.map specialize_expr es in
        Tuple es'
    | Record _ -> failwith __LOC__
    | Variant (lab,e1) ->
        let e1' = specialize_expr e1 in
        Variant (lab,e1')
    | Lambda abs -> 
        let abs' = specialize_abs_with_ty abs in
        Lambda abs'
    | Effect _ -> e
    | Handler _ -> e
    | BigLambdaTy (tvar,sk,e1) -> 
        let e1' = specialize_expr e1 in
        BigLambdaTy (tvar,sk,e1') 
    | BigLambdaDirt (dvar,e1) -> 
        let e1' = specialize_expr e1 in
        BigLambdaDirt (dvar,e1') 
    | BigLambdaSkel (evar,e1) -> 
        let e1' = specialize_expr e1 in
        BigLambdaSkel (evar,e1') 
    | CastExp (e1,tyco) -> 
        let e1' = specialize_expr e1 in
        CastExp (e1',tyco) 
    | ApplyTyExp (e1,ty1) ->(
        match ty1 with
          | TyParam _ -> (
              (* Don't specialize with typarams *)
              let e1' = specialize_expr e1 in
              ApplyTyExp (e1',ty1))
          | _ -> (
              match (e1, fbody) with
              | (Var g, BigLambdaTy (tyvar,evar,v)) when g = fvar ->
                  Var (specialize_app e)
              | _ -> (
                  let e1' = specialize_expr e1 in
                  ApplyTyExp (e1',ty1)
                )
            )
      )
    | LambdaTyCoerVar (ctvar,ctty,e1) -> 
        let e1' = specialize_expr e1 in
        LambdaTyCoerVar (ctvar,ctty,e1') 
    | LambdaDirtCoerVar (ctvar,ctty,e1) -> 
        let e1' = specialize_expr e1 in
        LambdaDirtCoerVar (ctvar,ctty,e1') 
    | ApplyDirtExp (e1,d) -> (
        match d.row with
        | ParamRow _ -> (
            (* Don't specialize with params *)
            let e1' = specialize_expr e1 in
            ApplyDirtExp (e1',d))
        | _ -> (
            match (e1, fbody) with
            | (Var g, BigLambdaDirt (dvar,v)) when g = fvar ->
                Var (specialize_app e)
            | _ -> (
                let e1' = specialize_expr e1 in
                ApplyDirtExp (e1',d)
              )
          )
      )
    | ApplySkelExp (e1,sk) -> (
        match sk with
        | SkelParam _ -> (
            (* Don't specialize with params *)
            let e1' = specialize_expr e1 in
            ApplySkelExp (e1',sk))
        | _ -> (
            match (e1, fbody) with
            | (Var g, BigLambdaSkel (evar,v)) when g = fvar ->
              Var (specialize_app e)
            | _ -> (
                let e1' = specialize_expr e1 in
                ApplySkelExp (e1',sk)
              )
          )
      )
    | ApplyTyCoercion (e1,tyco) -> (
        match tyco with
        | TyCoercionVar _ -> (
            (* Don't specialize with params *)
            let e1' = specialize_expr e1 in
            ApplyTyCoercion (e1',tyco))
        | _ -> (
            match (e1, fbody) with
            | (Var g, LambdaTyCoerVar (ctvar,_,v)) when g = fvar ->
              Var (specialize_app e)
            | _ -> (
                let e1' = specialize_expr e1 in
                ApplyTyCoercion (e1',tyco)
              )
          )
      )
    | ApplyDirtCoercion (e1,dco) -> (
        match dco with
        | DirtCoercionVar _ -> (
            (* Don't specialize with params *)
            let e1' = specialize_expr e1 in
            ApplyDirtCoercion (e1',dco))
        | _ -> (
            match (e1, fbody) with
            | (Var g, LambdaDirtCoerVar (ctvar,_,v)) when g = fvar ->
              Var (specialize_app e)
            | _ -> (
                let e1' = specialize_expr e1 in
                ApplyDirtCoercion (e1',dco)
              )
          )
      )

  and specialize_abs (p,c) = 
    (p,specialize_comp c)

  and specialize_abs_with_ty (p,ty,c) =
    (p,ty,specialize_comp c)
  in 
  (* End of the tree traversal functions *)

  (* This function prefixes the specializations in cont, and applies some necessary substitutions. *)
  let subst_spec
      (cont:Typed.computation)
      ((app:Typed.expression),(fvar':Typed.variable))
    : Typed.computation =
    match (fbody,resty,app) with
    | (BigLambdaTy (typaram,_sk,v), TySchemeTy (typaram',_sk',ty1), ApplyTyExp (_var,ty2)) ->
      assert (typaram = typaram');
      assert (_sk = _sk');
      Print.debug "BigLambdaTy: replacing %t by %t" (CoreTypes.TyParam.print typaram) (Types.print_target_ty ty2);
      let sub = Substitution.add_type_substitution_e typaram ty2 in
      (* v' = [T2/a]v *)
      let v' = Substitution.apply_substitutions_to_expression sub v in
      (* T1' = [T2/a]T1 *)
      let ty1' = Substitution.apply_substitutions_to_type sub ty1 in
      LetVal (v',(PVar fvar',ty1',cont))
    | (BigLambdaDirt (dirtparam,v), TySchemeDirt (dirtparam',ty), ApplyDirtExp (_var,dirt)) ->
      assert (dirtparam = dirtparam');
      Print.debug "BigLambdaDirt: replacing %t by %t" (CoreTypes.DirtParam.print dirtparam) (Types.print_target_dirt dirt);
      let sub = Substitution.add_dirt_substitution_e dirtparam dirt in
      (* v' = [D/d]v *)
      let v' = Substitution.apply_substitutions_to_expression sub v in
      (* T' = [D/d]T *)
      let ty' = Substitution.apply_substitutions_to_type sub ty in
      LetVal (v',(PVar fvar',ty',cont))
    | (BigLambdaSkel (skparam,v), TySchemeSkel (skparam',ty), ApplySkelExp (_var,sk)) ->
      assert (skparam = skparam');
      Print.debug "BigLambdaSkel: replacing %t by %t" (CoreTypes.SkelParam.print skparam) (Types.print_skeleton sk);
      let sub = Substitution.add_skel_param_substitution_e skparam sk in
      (* v' = [t/s]v *)
      let v' = Substitution.apply_substitutions_to_expression sub v in
      (* T' = [t/s]T *)
      let ty' = Substitution.apply_substitutions_to_type sub ty in
      LetVal (v',(PVar fvar',ty',cont))
    | (LambdaTyCoerVar (coparam,_ct_ty,v), QualTy (_ct_ty',ty), ApplyTyCoercion (_var,tyco)) ->
      assert (_ct_ty = _ct_ty');
      Print.debug "LambdaTyCoerVar: replacing %t by %t" (CoreTypes.TyCoercionParam.print coparam) (Typed.print_ty_coercion tyco);
      let sub = Substitution.add_type_coercion_e coparam tyco in
      (* v' = [y/w]v *)
      let v' = Substitution.apply_substitutions_to_expression sub v in
      (* T' = [y/w]T *)
      let ty' = Substitution.apply_substitutions_to_type sub ty in
      LetVal (v',(PVar fvar',ty',cont))
    | (LambdaDirtCoerVar (coparam,_ct_dirt,v), QualDirt (_ct_dirt',ty), ApplyDirtCoercion (_var,dco)) ->
      assert (_ct_dirt = _ct_dirt');
      Print.debug "LambdaDirtCoerVar: replacing %t by %t" (CoreTypes.DirtCoercionParam.print coparam) (Typed.print_dirt_coercion dco);
      let sub = Substitution.add_dirt_var_coercion_e coparam dco in
      (* v' = [y/w]v *)
      let v' = Substitution.apply_substitutions_to_expression sub v in
      (* T' = [y/w]T *)
      let ty' = Substitution.apply_substitutions_to_type sub ty in
      LetVal (v',(PVar fvar',ty',cont))
    | _ -> 
      (* This should not happen, it should be handled by the match in `specialize_letrec`. *)
      failwith __LOC__
  in
  (* Gather and substitute specializations *)
  Print.debug "SPEC: specializing a letrec";
  let cont' = specialize_comp cont in
  (* Prefix the new specializations as let-bindings for cont. *)
  Assoc.fold_left subst_spec cont' !assoc

(* Drop unused bindings *)
let letrec_drop_unused_bindings (c:computation) : computation =
  match c with
  | LetRec (bindings,c1) -> (
      (* Get the free variables (function defs are the first part of the tuple)*)
      let (free_vars_c1,_) = Typed.free_vars_comp c1 in
      let free_vars_bindings = List.map (fun (_,_,_,a) -> fst (free_vars_abs a)) bindings in
      let free_vars = List.flatten (free_vars_c1 :: free_vars_bindings) in
      let used_bindings = List.filter (fun (var,_,_,_) -> List.mem var free_vars) bindings
      in
      (* if no bindings used, leave this LetRec out *)
      match used_bindings with
      | [] ->
        Print.debug "Dropping letrec bindings";
        c1
      | _  -> LetRec (used_bindings, c1)
    )
  | _ -> (
      Print.debug "This function can only be applied on LetRecs";
      failwith __LOC__
    )


let rec optimize_ty_coercion st tyco =
  reduce_ty_coercion st (optimize_sub_ty_coercion st tyco)


and optimize_dirty_coercion st dtyco =
  reduce_dirty_coercion st (optimize_sub_dirty_coercion st dtyco)


and optimize_dirt_coercion st dco =
  optimize_dirt_coercion' st EffectSet.empty dco


and optimize_dirt_coercion' st ops dco =
  reduce_dirt_coercion st ops (optimize_sub_dirt_coercion st ops dco)


and optimize_sub_ty_coercion st tyco =
  match tyco with
  | ReflTy ty -> tyco
  | ArrowCoercion (tyco1, dtyco2) ->
      ArrowCoercion
        (optimize_ty_coercion st tyco1, optimize_dirty_coercion st dtyco2)
  | HandlerCoercion (dtyco1, dtyco2) ->
      HandlerCoercion
        (optimize_dirty_coercion st dtyco1, optimize_dirty_coercion st dtyco2)
  | TyCoercionVar tycovar -> TyCoercionVar tycovar
  | SequenceTyCoer (tyco1, tyco2) ->
      SequenceTyCoer
        (optimize_ty_coercion st tyco1, optimize_ty_coercion st tyco2)
  | TupleCoercion tycos -> TupleCoercion tycos
  | LeftArrow tyco1 -> LeftArrow (optimize_ty_coercion st tyco1)
  | ForallTy (tv, tyco1) ->
      ForallTy (tv, optimize_ty_coercion (extend_ty_params st tv) tyco1)
  | ApplyTyCoer (tyco1, ty) -> ApplyTyCoer (optimize_ty_coercion st tyco1, ty)
  | ForallDirt (dv, tyco1) -> ForallDirt (dv, optimize_ty_coercion st tyco1)
  | ApplyDirCoer (tyco1, d) -> ApplyDirCoer (optimize_ty_coercion st tyco1, d)
  | PureCoercion dtyco1 -> PureCoercion (optimize_dirty_coercion st dtyco1)
  | QualTyCoer (ct_ty, tyco1) ->
      QualTyCoer (ct_ty, optimize_ty_coercion st tyco1)
  | QualDirtCoer (ct_dirt, tyco1) ->
      QualDirtCoer (ct_dirt, optimize_ty_coercion st tyco1)
  | ApplyQualTyCoer (tyco1, tyco2) ->
      ApplyQualTyCoer
        (optimize_ty_coercion st tyco1, optimize_ty_coercion st tyco2)
  | ApplyQualDirtCoer (tyco1, dco) ->
      ApplyQualDirtCoer
        (optimize_ty_coercion st tyco1, optimize_dirt_coercion st dco)
  | ForallSkel (sv, tyco1) -> ForallSkel (sv, optimize_ty_coercion st tyco1)
  | ApplySkelCoer (tyco1, sk) ->
      ApplySkelCoer (optimize_ty_coercion st tyco1, sk)
  | _ -> tyco


and optimize_sub_dirty_coercion st dtyco =
  match dtyco with
  | BangCoercion (tyco1, dco2) ->
      BangCoercion
        (optimize_ty_coercion st tyco1, optimize_dirt_coercion st dco2)
  | RightArrow tyco1 -> RightArrow (optimize_ty_coercion st tyco1)
  | RightHandler tyco1 -> RightHandler (optimize_ty_coercion st tyco1)
  | LeftHandler tyco1 -> LeftHandler (optimize_ty_coercion st tyco1)
  | SequenceDirtyCoer (dtyco1, dtyco2) ->
      SequenceDirtyCoer
        (optimize_dirty_coercion st dtyco1, optimize_dirty_coercion st dtyco2)


and optimize_sub_dirt_coercion st p_ops dco =
  match dco with
  | ReflDirt d -> dco
  | DirtCoercionVar dcov -> dco
  | Empty d ->
      if dirts_are_equal d empty_dirt then ReflDirt empty_dirt else dco
  | UnionDirt (ops, dco1) ->
      UnionDirt
        (ops, optimize_dirt_coercion' st (EffectSet.union p_ops ops) dco1)
  | SequenceDirtCoer (dco1, dco2) ->
      SequenceDirtCoer
        ( optimize_dirt_coercion' st p_ops dco1
        , optimize_dirt_coercion' st p_ops dco2 )
  | DirtCoercion dtyco -> DirtCoercion (optimize_dirty_coercion st dtyco)


and reduce_ty_coercion st tyco =
  Print.debug "reduce_ty_coercion: %t" (Typed.print_ty_coercion tyco) ;
  match tyco with
  | ReflTy ty -> tyco
  | ArrowCoercion (tyco1, dtyco2) -> (
    match (tyco1, dtyco2) with
    | ReflTy ty1, BangCoercion (ReflTy ty2, ReflDirt d) ->
        ReflTy (Arrow (ty1, (ty2, d)))
    | _ -> tyco )
  | HandlerCoercion (dtyco1, dtyco2) -> tyco
  | TyCoercionVar tycovar -> tyco
  | SequenceTyCoer (tyco1, tyco2) -> (
    match (tyco1, tyco2) with
    | ReflTy _, _ -> tyco2
    | _, ReflTy _ -> tyco1
    | _ -> tyco )
  | TupleCoercion tycos -> tyco
  | LeftArrow tyco1 -> tyco
  | ForallTy (tv, tyco1) -> tyco
  | ApplyTyCoer (tyco1, ty) -> tyco
  | ForallDirt (dv, tyco1) -> tyco
  | ApplyDirCoer (tyco1, d) -> tyco
  | PureCoercion dtyco1 -> tyco
  | QualTyCoer (ct_ty, tyco1) -> tyco
  | QualDirtCoer (ct_dirt, tyco1) -> tyco
  | ApplyQualTyCoer (tyco1, tyco2) -> tyco
  | ApplyQualDirtCoer (tyco1, dco) -> tyco
  | ForallSkel (sv, tyco1) -> tyco
  | ApplySkelCoer (tyco1, sk) -> tyco
  | _ -> tyco


and reduce_dirty_coercion st dtyco =
  Print.debug "reduce_dirty_coercion: %t" (Typed.print_dirty_coercion dtyco) ;
  match dtyco with
  | BangCoercion (tyco1, dco2) -> dtyco
  | RightArrow tyco1 -> (
    match tyco1 with ArrowCoercion (tyco11, dtyco12) -> dtyco12 | _ -> dtyco )
  | RightHandler tyco1 -> (
    match tyco1 with
    | HandlerCoercion (dtyco11, dtyco12) -> dtyco12
    | _ -> dtyco )
  | LeftHandler tyco1 -> (
    match tyco1 with
    | HandlerCoercion (dtyco11, dtyco12) -> dtyco11
    | _ -> dtyco )
  | SequenceDirtyCoer (dtyco1, dtyco2) ->
    match (dtyco1, dtyco2) with
    | BangCoercion (tyco1, dco1), BangCoercion (tyco2, dco2) ->
        BangCoercion
          ( reduce_ty_coercion st (SequenceTyCoer (tyco1, tyco2))
          , optimize_dirt_coercion st (SequenceDirtCoer (dco1, dco2)) )
    | _ -> dtyco


and reduce_dirt_coercion st p_ops dco =
  match dco with
  | ReflDirt d -> dco
  | DirtCoercionVar dcov -> dco
  | Empty d ->
      let d' = Types.remove_effects p_ops d in
      if dirts_are_equal d' empty_dirt then ReflDirt empty_dirt else Empty d'
  | UnionDirt (ops, dco1) -> (
    match dco1 with
    | ReflDirt d -> ReflDirt (add_effects ops d)
    | _ ->
        let d1, d2 = TypeChecker.tcDirtCo st.tc_state dco1 in
        let ops' =
          EffectSet.diff ops
            (EffectSet.inter d1.Types.effect_set d2.Types.effect_set)
        in
        if EffectSet.is_empty ops' then dco1 else UnionDirt (ops', dco1) )
  | SequenceDirtCoer (dco1, dco2) -> (
    match (dco1, dco2) with
    | ReflDirt _, _ -> dco2
    | _, ReflDirt _ -> dco1
    | _ -> dco )
  | DirtCoercion dtyco ->
    match dtyco with BangCoercion (_, dco1) -> dco1 | _ -> dco


let rec substitute_pattern_comp st c p exp =
  optimize_comp st (Typed.subst_comp (Typed.pattern_match p exp) c)


and beta_reduce st ((p, ty, c) as a) e =
  match applicable_pattern p (Typed.free_vars_comp c) with
  | Inlinable -> substitute_pattern_comp st c p e
  | NotPresent -> c
  | NotInlinable when is_atomic e ->
      Print.debug "beta_reduce not-inlinable is_atomc" ;
      substitute_pattern_comp st c p e
  | NotInlinable -> LetVal (e, a)


(*
            let a =
              begin match p with
                | {term = Typed.PVar x} ->
                  let st = {st with stack = Common.update x e st.stack} in
                  abstraction p (optimize_comp st c)
                | _ ->
                  a
              end
            in
            let_in e a
             *)
and optimize_comp st c = reduce_comp st (optimize_sub_comp st c)

and optimize_expr st e = reduce_expr st (optimize_sub_expr st e)

and optimize_abs st ty (p, c) =
  let st' = optimize_pattern st ty p in
  (p, optimize_comp st' c)


and optimize_sub_expr st e =
  let plain_e' =
    match e with
    | Var v -> Var v
    | BuiltIn (s, i) -> BuiltIn (s, i)
    | Const c -> Const c
    | Tuple es -> Tuple (List.map (optimize_expr st) es)
    | Record _r -> failwith __LOC__
    | Variant (l,e1) -> Variant (l,optimize_expr st e1)
    | Lambda plain_a_w_ty ->
      Lambda (optimize_abstraction_with_ty st plain_a_w_ty)
    | Effect op -> Effect op
    | Handler h -> Handler (optimize_sub_handler st h)
    | BigLambdaTy (ty_var, sk, e) ->
        let st' =
          extend_ty_param_skeletons (extend_ty_params st ty_var) ty_var sk
        in
        BigLambdaTy (ty_var, sk, optimize_expr st' e)
    | BigLambdaDirt (dirt_var, e) ->
        let st' = extend_dirt_params st dirt_var in
        BigLambdaDirt (dirt_var, optimize_expr st' e)
    | BigLambdaSkel (sk_var, e) ->
        let st' = extend_skel_params st sk_var in
        BigLambdaSkel (sk_var, optimize_expr st' e)
    | CastExp (e1, tyco1) ->
      CastExp (optimize_expr st e1, optimize_ty_coercion st tyco1)
    | ApplyTyExp (e, ty) -> ApplyTyExp (optimize_expr st e, ty)
    | LambdaTyCoerVar (tyco_var, ct_ty, e) ->
        let st' = extend_ty_coer_types st tyco_var ct_ty in
        LambdaTyCoerVar (tyco_var, ct_ty, optimize_expr st' e)
    | LambdaDirtCoerVar (dco_var, ct_dirt, e) ->
        let st' = extend_dirt_coer_types st dco_var ct_dirt in
        LambdaDirtCoerVar (dco_var, ct_dirt, optimize_expr st' e)
    | ApplyDirtExp (e, dirt) -> ApplyDirtExp (optimize_expr st e, dirt)
    | ApplySkelExp (e, sk) -> ApplySkelExp (optimize_expr st e, sk)
    | ApplyTyCoercion (e, tyco) -> ApplyTyCoercion (optimize_expr st e, tyco)
    | ApplyDirtCoercion (e, dco) -> ApplyDirtCoercion (optimize_expr st e, dco)
  in
  plain_e'


and match_recursive_function st e =
  match e with
  | Var fvar -> (
    match Assoc.lookup fvar st.recursive_functions with
    | None -> None
    | Some (fty, fbody) -> Some (fvar, fty, fbody) )
  | ApplyTyExp (e, ty) -> match_recursive_function st e
  | ApplyDirtExp (e, dirt) -> match_recursive_function st e
  | ApplySkelExp (e, sk) -> match_recursive_function st e
  | ApplyDirtCoercion (e, dco) -> match_recursive_function st e
  | ApplyTyCoercion (e, tyco) -> match_recursive_function st e
  | _ -> None


and match_knot_function st e h = match_knot_function' st e e h

and match_knot_function' st e e' h =
  match e with
  | Var fvar -> (
    match Assoc.lookup fvar st.knot_functions with
    | None -> None
    | Some (ef, hf, fvar') ->
        if alphaeq_expr [] e' ef && alphaeq_handler [] hf h then Some fvar'
        else None )
  | ApplyTyExp (e, ty) -> match_knot_function' st e e' h
  | ApplyDirtExp (e, dirt) -> match_knot_function' st e e' h
  | ApplySkelExp (e, sk) -> match_knot_function' st e e' h
  | ApplyDirtCoercion (e, dco) -> match_knot_function' st e e' h
  | ApplyTyCoercion (e, tyco) -> match_knot_function' st e e' h
  | _ -> None


and optimize_sub_handler st {effect_clauses= ecs; value_clause= vc} =
  let vc' = optimize_abstraction_with_ty st vc in
  let _, dty = TypeChecker.type_of_abstraction_with_ty st.tc_state vc in
  let ecs' = Assoc.kmap (optimize_abstraction2 st dty) ecs in
  {effect_clauses= ecs'; value_clause= vc'}


and optimize_abstraction_with_ty st a_w_ty =
  optimize_plain_abstraction_with_ty st a_w_ty


and optimize_plain_abstraction_with_ty st (p, ty, c) =
  (p, ty, optimize_comp (extend_pat_type st p ty) c)


and optimize_abstraction st ty a =
  let p, c = a in
  let st' = optimize_pattern st ty p in
  (p, optimize_comp st' c)


and optimize_pattern st ty p =
  extend_pat_type st p ty


and optimize_abstraction2 st (dty:target_dirty) (effect, a2) =
  let op, (op_ty_in, op_ty_out) = effect in
  let p1, p2, c = a2 in
  let st' = extend_pat_type st p1 op_ty_in in
  let st'' = extend_pat_type st' p2 (Types.Arrow (op_ty_out, dty)) in
  (effect, (p1, p2, optimize_comp st'' c))


and optimize_sub_comp st c =
  Print.debug "optimize_sub_comp: %t" (Typed.print_computation c) ;
  let plain_c' =
    match c with
    | Value e1 -> Value (optimize_expr st e1)
    | LetVal (e1, abs) ->
        LetVal (optimize_expr st e1, optimize_abstraction_with_ty st abs)
    | LetRec ([(var, argTy, resTy, (p,rhs))], c) ->
        let st' = extend_var_type st var (Types.Arrow (argTy, resTy)) in
        let e1  = Lambda (p,argTy,rhs) in
        let st'' = extend_rec_fun st' var (Types.Arrow (argTy, resTy)) e1 in
        LetRec ([(var, argTy, resTy, optimize_abstraction st' argTy (p,rhs))], optimize_comp st'' c)
    | Match (e1, resTy, abstractions) ->
        let ty = TypeChecker.typeOfExpression st.tc_state e1 in
        Match
          ( optimize_expr st e1
          , resTy
          , List.map (optimize_abstraction st ty) abstractions )
    | Apply (e1, e2) -> Apply (optimize_expr st e1, optimize_expr st e2)
    | Handle (e1, c1) -> Handle (optimize_expr st e1, optimize_comp st c1)
    | Call (op, e1, a_w_ty) ->
        Call (op, optimize_expr st e1, optimize_abstraction_with_ty st a_w_ty)
    | Op (op, e1) ->
        Print.debug "optimize_sub_comp Op" ;
        Op (op, optimize_expr st e1)
    | Bind (c1, abstraction) ->
        let ty, _ = TypeChecker.typeOfComputation st.tc_state c1 in
        Bind (optimize_comp st c1, optimize_abs st ty abstraction)
    | CastComp (c1, dirty_coercion) ->
        CastComp
          (optimize_comp st c1, optimize_dirty_coercion st dirty_coercion)
    | CastComp_ty (c1, ty_coercion) -> failwith __LOC__
    | CastComp_dirt (c1, dirt_coercion) -> failwith __LOC__
  in
  plain_c'


and reduce_expr st e =
  Print.debug "reduce_exp: %t" (Typed.print_expression e) ;
  match e with
  (*
          | Var of variable
          | BuiltIn of string * int
          | Const of Const.t
          | Tuple of expression list
          | Record of (CoreTypes.field, expression) CoreTypes.assoc
          | Variant of CoreTypes.label * expression option
          | Lambda of (pattern * Types.target_ty * computation)
          | Handler of handler
          | BigLambdaTy of CoreTypes.TyParam.t * skeleton * expression
          | BigLambdaDirt of CoreTypes.DirtParam.t * expression
          | BigLambdaSkel of CoreTypes.SkelParam.t * expression
          | LambdaTyCoerVar of CoreTypes.TyCoercionParam.t * Types.ct_ty * expression
          | LambdaDirtCoerVar of CoreTypes.DirtCoercionParam.t * Types.ct_dirt * expression
          | ApplySkelExp of expression * Types.skeleton
          | ApplyTyCoercion of expression * ty_coercion
          | ApplyTyCoercion (e1,ty_co) ->
          *)
  | ApplySkelExp (e1, sk) -> (
    match e1 with
    | BigLambdaSkel (skvar, e11) ->
      Substitution.apply_substitutions_to_expression (Substitution.add_skel_param_substitution_e skvar sk) e11
    | _ -> e )
  | ApplyTyExp (e1, ty) -> (
    match e1 with
    | BigLambdaTy (tyvar, sk, e11) ->
      Substitution.apply_substitutions_to_expression (Substitution.add_type_substitution_e tyvar ty) e11
    | _ -> e )
  | ApplyDirtCoercion (e1, dco) -> (
    match e1 with
    | LambdaDirtCoerVar (dcovar, ctd, e11) ->
      Substitution.apply_substitutions_to_expression
          (Substitution.add_dirt_var_coercion_e dcovar dco) e11
    | _ -> e )
  | ApplyDirtExp (e1, d) -> (
    match e1 with
    | BigLambdaDirt (dvar, e11) ->
      Substitution.apply_substitutions_to_expression (Substitution.add_dirt_substitution_e dvar d) e11
    | _ -> e )
  | Effect op -> e
  | CastExp (e1, ty_co) ->
      let ty1, ty2 = TypeChecker.tcValTyCo st.tc_state ty_co in
      if (Print.debug "HERE1"; Types.types_are_equal ty1 ty2) then e1 else e
  | plain_e -> e


and reduce_comp st c =
  Print.debug "reduce_comp: %t" (Typed.print_computation c) ;
  match c with
  | Value _ -> c
  | LetVal (e1, abs) -> beta_reduce st abs e1
  | LetRec (bs, cont) ->
    let cont' = List.fold_left specialize_letrec cont bs in
    letrec_drop_unused_bindings (LetRec (bs, cont'))
  | Match (e1, resTy, abstractions) -> c
  | Apply (e1, e2) -> (
    match e1 with
    | Lambda abs -> beta_reduce st abs e2
    | Effect op ->
      c
        (* BRECHT: Removing opts that create Calls for use with erasureUntyped
         * as UntypedSyntax cannot represent them. Maybe they can be
         * reintroduced with the SkelEff backend.
         * Print.debug "Op -> Call" ;
         * let var = CoreTypes.Variable.fresh "call_var" in
         * let eff, (ty_in, ty_out) = op in
         * let c_cont =
         *   CastComp
         *     ( Value (Var var)
         *     , BangCoercion
         *         ( ReflTy ty_out
         *         , Empty (Types.closed_dirt (EffectSet.singleton eff)) ) )
         * in
         * let a_w_ty = (PVar var, ty_out, c_cont) in
         * Call (op, e2, a_w_ty) *)
    | _ ->
        Print.debug "e1 is not a lambda" ;
        (* TODO: support case where it's a cast of a lambda *)
        c )
  | Handle (e1, c1) -> (
    match e1 with
    | CastExp (e11, tyco1) ->
        let c1' = CastComp (c1, LeftHandler tyco1) in
        let c' = Handle (e11, c1') in
        optimize_comp st (CastComp (c', RightHandler tyco1))
    | Handler h -> (
      match c1 with
      | Value e1 ->
          (* special case that happens when the handler has no effect clauses *)
          optimize_comp st (beta_reduce st h.value_clause e1)
      | CastComp (c1', dtyco1) -> (
        match is_relatively_pure st c1' h with
        | Some dtyco ->
            optimize_comp st
              (Bind
                 ( CastComp (c1', dtyco)
                 , Typed.abstraction_with_ty_to_abstraction h.value_clause ))
        | None -> c )
      | Bind (c11, a1) -> (
        match c11 with
        | CastComp (c111, _) -> (
          match (* TODO: Fix *)
                is_relatively_pure st c111 h with
          | Some dtyco ->
              let p12, c12 = a1 in
              optimize_comp st
                (Bind
                   ( CastComp (c111, dtyco)
                   , abstraction p12 (Handle (refresh_expr e1, c12)) ))
          | None -> c )
        | _ -> (
          let tv = match TypeChecker.type_of_handler st.tc_state h with
            | Types.Handler ((x,_), _) -> x
            | _ -> failwith __LOC__
          in
          let p12, c12 = a1 in
          let c_r' = (p12, tv, (Handle (e1, c12))) in
          let h' = { h with value_clause = c_r'} in
          optimize_comp st
            ( Handle (Handler h', c11) )))
      | Call (eff, e11, k_abs) -> (
          (* handle call(eff,e11,y:ty.c) with H@{eff xi ki -> ci}
             >-->
              ci [(fun y:ty -> handle c with H)/ki, e11 / xi]
           *)
          let (k_pat, k_ty, k_c) as k_abs' = refresh_abs_with_ty k_abs in
          let handled_k =
            abstraction_with_ty k_pat k_ty
              (reduce_comp
                 (extend_pat_type st k_pat k_ty)
                 (Handle (refresh_expr e1, k_c)))
          in
          match Assoc.lookup eff h.effect_clauses with
          | Some eff_clause ->
              let p1, p2, c = refresh_abs2 eff_clause in
              (* Shouldn't we check for inlinability of p1 and p2 here? *)
              substitute_pattern_comp st
                (Typed.subst_comp (Typed.pattern_match p1 e11) c)
                p2 (Lambda handled_k)
          | None ->
              let k_abs'' = (k_pat, k_ty, Handle (e1, k_c)) in
              let res = Call (eff, e11, k_abs'') in
              reduce_comp st res )
      | Apply (e11, e12) -> (
          Print.debug "Looking for recursive function name" ;
          match match_recursive_function st e11 with
          | Some (fvar, fty, fbody) ->
              (*
                   handle C[f] e12 with H
                   >->
                   let rec f' : ty_{e12} -> dty_{handle C[f] e12 with H}
                        = fun x : ty_{e12} -> handle C[fbody] x with H
                   in f' e12
                *)
              Print.debug "Found recursive function call" ;
              let dty_c = TypeChecker.typeOfComputation st.tc_state c in
              let ty_e12 = TypeChecker.typeOfExpression st.tc_state e12 in
              let fvar' = CoreTypes.Variable.refresh fvar in
              let xvar = CoreTypes.Variable.new_fresh () "__x_of_rec__" in
              let fty' = Arrow (ty_e12, dty_c) in
              let st' =
                { st with
                  recursive_functions=
                    Assoc.remove fvar st.recursive_functions
                ; knot_functions= Assoc.update fvar (e11, h, fvar') st.knot_functions
                }
              in
              let st'' = extend_var_type st' fvar' fty' in
              let fbody' =
                optimize_abstraction st'' ty_e12
                  ( PVar xvar
                  , Handle
                      ( e1
                      , Apply
                          ( Typed.subst_expr (Assoc.of_list [(fvar, refresh_expr fbody)])
                              e11
                          , Var xvar ) )
                  )
              in
              LetRec ([(fvar', ty_e12, dty_c, fbody')], Apply (Var fvar', e12))
          | None ->
            match match_knot_function st e11 h with
            | Some fvar' -> Apply (Var fvar', e12)
            | None -> c )
      | Match (e, _resTy, branches) ->
          (*
             handle (match e with {pi -> ci} ) with H
             >-->
             match e with {pi -> handle ci with H}
           *)
          let ty_e = TypeChecker.typeOfExpression st.tc_state e in
          let handResTy = (match TypeChecker.typeOfExpression st.tc_state e1 with
                           | Handler (_,handResTy) -> handResTy
                           | _other -> failwith __LOC__ (* impossible *)
                          ) in
          Match
            ( e
            , handResTy
            , List.map
                (fun (pi, ci) ->
                  optimize_abs st ty_e (abstraction pi (Handle (e1, ci))) )
                branches )
      | _ -> c )
    | _ -> c )
  | Call (op, e1, a_w_ty) -> c
  | Op (op, e1) -> failwith __LOC__
  | Bind (c1, a2) -> (
    match c1 with
    | Bind (c11, (p1, c12)) ->
        let ty1, _ = TypeChecker.typeOfComputation st.tc_state c11 in
        let st' = extend_pat_type st p1 ty1 in
        let c2' = reduce_comp st' (Bind (c12, a2)) in
        reduce_comp st (Bind (c11, (p1, c2')))
    | Value e11 ->
        let ty11 = TypeChecker.typeOfExpression st.tc_state e11 in
        let p2, c2 = a2 in
        beta_reduce st (p2, ty11, c2) e11
    | Call (op, e11, (p12, ty12, c12)) ->
        let st' = extend_pat_type st p12 ty12 in
        let c12' = reduce_comp st' (Bind (c12, a2)) in
        Call (op, e11, (p12, ty12, c12'))
    | CastComp (c11, dtyco) -> (
      match c11 with
      | Value e111 ->
          let p_e111' = CastExp (e111, PureCoercion dtyco) in
          let ty111 = TypeChecker.typeOfExpression st.tc_state p_e111' in
          let p2, c2 = a2 in
          beta_reduce st (p2, ty111, c2) p_e111'
      | Bind (c111, abs112) ->
          let p112, c112 = abs112 in
          let ty111, _ = TypeChecker.typeOfComputation st.tc_state c111 in
          let c112' = CastComp (c112, dtyco) in
          let st' = extend_pat_type st p112 ty111 in
          let c2' = reduce_comp st' (Bind (c112', a2)) in
          let dtyco' =
            optimize_dirty_coercion st
              (BangCoercion (ReflTy ty111, DirtCoercion dtyco))
          in
          let c111' = reduce_comp st (CastComp (c111, dtyco')) in
          reduce_comp st (Bind (c111', (p112, c2')))
      | _ -> c )
    | _ -> c )
  | CastComp (c1, dtyco) -> (
      let dty1, dty2 = TypeChecker.tcCmpTyCo st.tc_state dtyco in
      match c1 with
      | _ when (Print.debug "HERE2"; Types.dirty_types_are_equal dty1 dty2) -> c1
      | CastComp (c11, dtyco12) ->
          CastComp
            ( c11
            , optimize_dirty_coercion st (SequenceDirtyCoer (dtyco12, dtyco))
            )
      | Call (op, e11, (p12, ty12, c12)) ->
          let st' = extend_pat_type st p12 ty12 in
          let c12' = reduce_comp st' (CastComp (c12, dtyco)) in
          Call (op, e11, (p12, ty12, c12'))
      | CastComp_ty (c1, ty_coercion) -> c
      | CastComp_dirt (c1, dirt_coercion) -> c
      | _ -> c
    )
  | _ -> c


(*
  | _ when outOfFuel st -> c

  | Match ({term = Const cst}, cases) ->
    let rec find_const_case = function
      | [] -> c
      | ({term = {term = PConst cst'}, c'}) :: _ when Const.equal cst cst'
         -> useFuel st; c'
      | _ :: cases -> find_const_case cases
    in
    find_const_case cases


  | Bind ({term = Call (eff, param, k)}, c) ->
    useFuel st;
    let {term = (k_pat, k_body)} = refresh_abs k in
    let bind_k_c = reduce_comp st (bind k_body c) in
    let res =
      call eff param (abstraction k_pat bind_k_c)
    in
    reduce_comp st res

  | Handle (h, {term = LetRec (defs, co)}) ->
    useFuel st;
    st.optimization_handler_With_LetRec := !(st.optimization_handler_With_LetRec) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    let handle_h_c = reduce_comp st (handle h co) in
    let res =
      let_rec' defs handle_h_c
    in
    reduce_comp st res

  | Handle ({term = Handler h} as handler, {term = Bind (c1, {term = (p1, c2)})})
        when (is_pure_for_handler c1 h.effect_clauses) ->
    useFuel st;
    st.optimization_handler_with_do := !(st.optimization_handler_with_do) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    (* Print.debug "Remove handler of outer Bind, since no effects in common with computation"; *)
    reduce_comp st (bind (reduce_comp st c1) (abstraction p1 (reduce_comp st (handle (refresh_expr handler) c2))))

  | Handle ({term = Handler h}, {term = Bind (c1, {term = (p1, c2)})})
        when (is_pure_for_handler c2 h.effect_clauses) ->
    useFuel st;
    st.optimization_handler_With_Ret := !(st.optimization_handler_With_Ret) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    (* Print.debug "Move inner bind into the value case"; *)
    let new_value_clause = optimize_abs st (abstraction p1 (bind (reduce_comp st c2) (refresh_abs h.value_clause))) in
    let hdlr = handler {
      effect_clauses = h.effect_clauses;
      value_clause = refresh_abs new_value_clause;
    } in
    reduce_comp st (handle (refresh_expr hdlr) c1)

  | Handle ({term = Handler h} as h2, {term = Bind (c1, {term = (p, c2)})}) ->
    useFuel st;
    st.optimization_handler_With_Ret := !(st.optimization_handler_With_Ret) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    (* Print.debug "Move (dirty) inner bind into the value case"; *)
    let new_value_clause = optimize_abs st (abstraction p (handle (refresh_expr h2) (refresh_comp (reduce_comp st c2) ))) in
    let hdlr = handler {
      effect_clauses = h.effect_clauses;
      value_clause = refresh_abs new_value_clause;
    } in
    reduce_comp st (handle (refresh_expr hdlr) (refresh_comp c1))

  | Handle ({term = Handler h} as handler, {term = Call (eff, param, k)}) ->
    useFuel st;
    let {term = (k_pat, k_body)} = refresh_abs k in
    let handled_k =
      abstraction k_pat
        (reduce_comp st (handle (refresh_expr handler) k_body))
    in
    begin match Common.lookup eff h.effect_clauses with
      | Some eff_clause ->
      st.optimization_handler_With_Handled_Op := !(st.optimization_handler_With_Handled_Op) + 1;
      st.optimization_total := !(st.optimization_total) + 1;
        let {term = (p1, p2, c)} = refresh_abs2 eff_clause in
        (* Shouldn't we check for inlinability of p1 and p2 here? *)
        substitute_pattern_comp st (substitute_pattern_comp st c p1 param) p2 (lambda handled_k)
      | None ->
        let res =
          call eff param handled_k
        in
        reduce_comp st res
    end

  | Handle (e1, {term = Apply (ae1, ae2)}) ->
    useFuel st;
    begin match ae1 with
      | Var v ->
            begin match (find_in_handlers_func_mem st v e1) with
             (*function exist,Same handler, same value clause*)
             | (true,Some new_f_exp,None) ->
                                let res = apply new_f_exp ae2
                                in reduce_comp st res
             | (true,Some special_f_exp, Some original_val_clause) ->
                  let Handler h1 = e1 in
                  let h1_v_clause = h1.value_clause in
                  let orig_vc_lambda = optimize_expr st (lambda (h1_v_clause)) in
                  let res = apply special_f_exp (tuple [ae2;orig_vc_lambda]) in
                  reduce_comp st res

             (*function exist,Same handler, different value clause*)
             | (false, Some new_f_exp,Some original_val_clause)->
               begin match (find_in_let_rec_mem st v) with
                | Some abs ->
                  st.optimization_function_specialization := !(st.optimization_function_specialization ) + 1;
                  st.optimization_total := !(st.optimization_total) + 1;
                  let (let_rec_p,let_rec_c) = abs in
                  (* Print.debug "THE ABSTRACTION OF SAME HANDLER DIFF VALUE :- %t" (Typed.print_abstraction abs); *)
                  let Handler ha = e1 in
                  (* Print.debug "THE VALUE CLAUSE :- %t" (Typed.print_abstraction ha.value_clause); *)
                  let ctx_val, (tyin_val , (tyout_val,drt_val)), cnstrs_val = ha.value_clause.scheme in
                  let continuation_var_scheme = (ctx_val, Type.Arrow(tyin_val , Type.fresh_dirty ()), cnstrs_val) in
                  let k_var, k_pat = make_var "k_val"  continuation_var_scheme in
                  let ctx1, ty1, cnstrs1 = let_rec_p.scheme in
                  let newf_input_tuple_pat = {
                    term = PTuple [let_rec_p; k_pat];
                    scheme = (
                      ctx_val @ ctx1,
                      Type.Tuple [ty1 ; Type.Arrow(tyin_val , Type.fresh_dirty ())],
                      Constraints.union cnstrs_val cnstrs1
                    );
                    location = ae1hh;
                  } in
                  let(newf_ctx,newf_ty,newf_const) = newf_input_tuple_pat.scheme in
                  let (f_ctx,ae1Ty,f_const) = ae1.scheme in
                  let Type.Arrow(f_ty_in, f_ty_out ) = Constraints.expand_ty ae1Ty in
                  let newf_scheme = Scheme.clean_ty_scheme ~loc:chh (newf_ctx , Type.Arrow (newf_ty, (tyout_val,drt_val)), newf_const) in
                  let newf_var, newf_pat = make_var "new_special_var"  newf_scheme in
                  let Var newfvar = newf_var in
                  let Handler hndlr = e1 in
                  let vc_var_scheme = (ctx_val,tyin_val,cnstrs_val) in
                  let vc_var, vc_pat = make_var "vcvar"  vc_var_scheme in
                  let new_value_clause = abstraction vc_pat (apply k_var vc_var) in
                  let new_handler =  handler {
                                      effect_clauses = hndlr.effect_clauses;
                                      value_clause =  new_value_clause;
                                    } in
                  let st = {st with handlers_functions_cont_mem = (new_handler, v, newf_var ) :: (st.handlers_functions_cont_mem)} in
                  let new_handler_call = reduce_comp st (handle new_handler let_rec_c) in
                  let newf_body = abstraction newf_input_tuple_pat new_handler_call in
                  let defs = [(newfvar, newf_body)] in
                  let orig_vc_lambda = optimize_expr st (lambda (hndlr.value_clause)) in
                  let res = let_rec' defs @@  apply newf_var  ( tuple [ae2; orig_vc_lambda] ) in
                  (* Print.debug "THE resulting computation :-  %t" (Typed.print_computation res); *)
                   optimize_comp st res
                | _ -> c
               end
             | (true, None,_) ->
                  c
             | _ ->
               begin match find_in_stack st v with
               | Some ({term = Lambda k}) ->
                  st.optimization_function_specialization := !(st.optimization_function_specialization ) + 1;
                  st.optimization_total := !(st.optimization_total) + 1;
                  let {term = (newdp, newdc)} = refresh_abs k in
                  let (h_ctx,Type.Handler(h_ty_in, (ty_out, drt_out)),h_const) = e1.scheme in
                  let (f_ctx,ae1Ty,f_const) = ae1.scheme in
                  let Type.Arrow(f_ty_in, f_ty_out ) = Constraints.expand_ty ae1Ty in
                  let constraints = Constraints.list_union [h_const; f_const]
                                    |> Constraints.add_dirty_constraint ~loc:chh f_ty_out h_ty_in in
                  let sch = (h_ctx @ f_ctx, (Type.Arrow(f_ty_in,(ty_out,drt_out))), constraints) in
                  let function_scheme = Scheme.clean_ty_scheme ~loc:chh sch in
                  let f_var, f_pat = refresh_var v function_scheme in
                  let f_def =
                    lambda @@
                    abstraction newdp @@
                    handle e1 newdc in
                  let res =
                    let_in f_def @@
                    abstraction f_pat @@
                    apply f_var ae2
                  in
                  optimize_comp st res
                | _ ->
                       begin match (find_in_let_rec_mem st v) with
                       | Some abs ->
                            st.optimization_function_specialization := !(st.optimization_function_specialization ) + 1;
                            st.optimization_total := !(st.optimization_total) + 1;
                            let (let_rec_p,let_rec_c) = abs in
                            let (h_ctx,Type.Handler(h_ty_in, (ty_out, drt_out)),h_const) = e1.scheme in
                            let (f_ctx,ae1Ty,f_const) = ae1.scheme in
                            let Type.Arrow(f_ty_in, f_ty_out ) = Constraints.expand_ty ae1Ty in
                            let constraints = Constraints.list_union [h_const; f_const]
                                  |> Constraints.add_dirty_constraint ~loc:chh f_ty_out h_ty_in in
                            let sch = (h_ctx @ f_ctx, (Type.Arrow(f_ty_in,(ty_out,drt_out))), constraints) in
                            let function_scheme = Scheme.clean_ty_scheme ~loc:chh sch in
                            let new_f_var, new_f_pat = refresh_var v function_scheme in
                            let new_handler_call = handle e1 let_rec_c in
                            let Var newfvar = new_f_var in
                            let defs = [(newfvar, (abstraction let_rec_p new_handler_call ))] in
                            let st = {st with handlers_functions_mem = (e1,v,new_f_var) :: st.handlers_functions_mem} in
                            st.handlers_functions_ref_mem := (e1,v,new_f_var) :: !(st.handlers_functions_ref_mem) ;
                            let res =
                              let_rec' defs @@
                              apply new_f_var ae2
                            in
                            optimize_comp st res
                       | _ ->
                        (* Print.debug "Its a none"; *)
                                    (* Print.debug "The handle exp : %t" (Typed.print_expression ae1); *)
                                    c
                       end
               end
        end
      | _ -> c
    end

| Handle (e1, {term = Match (e2, cases)}) ->
    useFuel st;
    let push_handler = fun {term = (p, c)} ->
      abstraction p (reduce_comp st (handle (refresh_expr e1) c))
    in
    let res =
      match' e2 (List.map push_handler cases)
    in
    res

(*
    (*
      let f = \p. val lambda in c
       ~~> (append f := f1 to impure_wrappers)
      let f1 = \*p. lambda
      let f = \new_p. val (f1 new_p) in
      c
    *)
  | LetIn ({term = Lambda ({term = (p, {term = Value ({term = Lambda _ } as in_lambda)} )})}, ({term = ({term = PVar f} as f_pat,_)} as a) )->
    Print.debug "We are now in the let in 4 for %t" (Typed.print_pattern f_pat);
    let f1_var, f1_pat = make_var "f1" f_pat.scheme in
    let new_p_var, new_p_pat = make_var "new_p" p.scheme in
    let first_fun =
      pure_lambda @@
      pure_abstraction p @@
      in_lambda
    and second_fun =
      lambda @@
      abstraction new_p_pat @@
      value (pure_apply f1_var new_p_var)
    in
    let st = {st with impure_wrappers = (f, f1_var) :: st.impure_wrappers} in
    let res =
      let_in first_fun @@
      abstraction f1_pat @@
      let_in second_fun @@
      a
    in
    optimize_comp st res
*)

  (* XXX simplify *)
  | LetRec (defs, co) ->
    useFuel st;
    (*Print.debug "the letrec comp  %t" (Typed.print_computation co);*)
    let st =
    List.fold_right (fun (var,abs) st ->
            (*Print.debug "ADDING %t and %t to letrec" (Typed.print_variable var) (Typed.print_abstraction abs);*)
            {st with letrec_memory = (var,abs) :: st.letrec_memory}) defs st in
    let_rec' defs (reduce_comp st co)


  | _ -> c

  in
  (*
  if c <> c' then
   Print.debug ~loc:c.Typedhh "%t : %t@.~~~>@.%t : %t@.\n"
    (Typed.print_computation c) (Scheme.print_dirty_scheme c.Typed.scheme)
    (Typed.print_computation c') (Scheme.print_dirty_scheme c'.Typed.scheme);*)
  c'
*)

let optimize_main_comp tc_state c =
  optimize_comp {inititial_state with tc_state} c

(*
 To Do list for optimization :

  ==> Handlers inline.

*)
(* open Typed *)
(*

let x = Types.PrimTy BoolTy;;

type state = {
  inlinable : (Typed.variable, unit -> Typed.expression) Common.assoc;
  stack : (Typed.variable, Typed.expression) Common.assoc;
  letrec_memory : (Typed.variable, Typed.abstraction) Common.assoc;
  handlers_functions_mem : (Typed.expression * Typed.variable * Typed.expression) list;
  handlers_functions_ref_mem : ((Typed.expression * Typed.variable * Typed.expression) list) ref;
  handlers_functions_cont_mem : ((Typed.expression * Typed.variable * Typed.expression) list);
  impure_wrappers : (Typed.variable, Typed.expression) Common.assoc;
  fuel : int ref;
  optimization_total : int ref;
  optimization_App_Fun : int ref;
  optimization_Do_Ret : int ref ;
  optimization_Do_Op : int ref;
  optimization_handler_With_LetRec : int ref;
  optimization_handler_With_Ret : int ref;
  optimization_handler_With_Handled_Op : int ref;
  optimization_handler_With_Pure : int ref;
  optimization_handler_with_do : int ref;
  optimization_function_specialization : int ref;
}



let initial = {
  inlinable = [];
  stack = [];
  letrec_memory = [];
  handlers_functions_mem = [];
  impure_wrappers = [];
  handlers_functions_ref_mem = ref [];
  handlers_functions_cont_mem =[];
  fuel = ref (!(Config.optimization_fuel));
  optimization_total = ref 0;
  optimization_App_Fun = ref 0;
  optimization_Do_Ret = ref 0;
  optimization_Do_Op = ref 0;
  optimization_handler_With_LetRec = ref 0;
  optimization_handler_With_Ret = ref 0;
  optimization_handler_With_Handled_Op = ref 0;
  optimization_handler_With_Pure = ref 0;
  optimization_handler_with_do = ref 0;
  optimization_function_specialization = ref 0;
}


(* -------------------------------------------------------------------------- *)
(* OPTIMIZATION FUEL                                                          *)
(* -------------------------------------------------------------------------- *)

let refuel st =
  st.fuel := !(Config.optimization_fuel)

let outOfFuel st =
  (* print_string "outOfFuel: "; print_int (!(st.fuel)); print_newline (); *)
  !(st.fuel) < 1

let useFuel st =
  st.fuel := !(st.fuel) - 1

(* -END OPTIMIZATION FUEL --------------------------------------------------- *)

let find_inlinable st x =
  match Common.lookup x st.inlinable with
  | Some e -> Some (e ())
  | None -> None

let find_in_stack st x = Common.lookup x st.stack

let find_in_let_rec_mem st v = Common.lookup v st.letrec_memory

(*let specialized_counter = ref []

 let specialized_count v =
  match Common.lookup v !specialized_counter with
  | Some n -> n
  | None -> 0

let incr_specialized_count v =
  let n = specialized_count v in
  specialized_counter := Common.update v (n + 1) !specialized_counter
 *)

let alphaeq_handler_no_vc eqvars h h'=
let (Handler ht) = h in
let (Handler h't) = h' in
 assoc_equal (alphaeq_abs2 eqvars) ht.effect_clauses h't.effect_clauses

let is_pure c = false
(*   Scheme.is_surely_pure c.Typed.scheme *)

let is_pure_for_handler c clauses = false
(*   Scheme.is_surely_pure_for_handler c.Typed.scheme (List.map (fun ((eff, _), _) -> eff) clauses) *)

let find_in_handlers_func_mem st f_name h_exp =
  let loc = h_exphh in
  let findres_cont_list = List.filter
                  (fun (h,old_f,new_f) -> (f_name == old_f) ) (st.handlers_functions_cont_mem) in
  let findres = List.filter
                  (fun (h,old_f,new_f) -> (f_name == old_f) ) st.handlers_functions_mem in
  begin match findres_cont_list with
  |(h,_,newf):: _ ->
                if (alphaeq_handler_no_vc [] h h_exp)
                then begin
                     let Handler hh = h in
                     (true, Some newf, Some hh.value_clause)
                     end
                else begin
                (true,None,None)
              end
  | [] ->
        begin match findres with
        | [] -> (false,None,None)
        | [(h,_,newf)] ->
            if (alphaeq_expr [] h h_exp)
            then
              (true,Some newf,None)
            else begin
              if (alphaeq_handler_no_vc [] h h_exp)
              then begin
                (* Print.debug ~loc:h_exp.Typedhh"ONLY VALUE CLAUSE IS DIFFERENT !! %t" (Typed.print_expression h_exp); *)
                let Handler hh = h in
                (false,Some newf,Some hh.value_clause)
              end
              else
                begin
                (* Print.debug ~loc:h_exp.Typedhh"Conflicting specialization call on\n %t \n=====================================\n %t "  (Typed.print_expression h_exp) (Typed.print_expression h); *)
                (true,None,None)
                end
            end

        end
  end


let different_branch_specialized defs st =
  (* Print.debug "\n\nthe letrec defs size:- %i \n" (List.length defs); *)
  (* Print.debug "\n\nthe global size:- %i \n" (List.length !(st.handlers_functions_ref_mem)); *)
  let findresinlocal = fun f_name -> (
                  List.filter
                  (fun (h,old_f,new_f) ->
                      let Var vv = new_f in
                      (f_name == vv) )   (st.handlers_functions_mem)) in
  let findresinglobal = fun f_name -> (
                  List.filter
                  (fun (h,old_f,new_f) ->
                      let Var vv = new_f in
                      (f_name == vv) )   !(st.handlers_functions_ref_mem)) in
  let globalboollist =
      (List.map (fun (var,abs) ->
            begin match findresinglobal var with
            | [] -> false
            | _ -> true
            end) defs ) in
  let global_bool = List.fold_right (||) globalboollist false in
  let localboollist =
      (List.map (fun (var,abs) ->
            begin match findresinlocal var with
            | [] -> false
            | (h,old_f,new_f) :: _ -> (* Print.debug "\n my old function :- %t \n" (Typed.print_variable old_f); *)
                                      (* Print.debug "\n my new function :- %t \n" (Typed.print_expression new_f);  *)
                                      true
            end) defs ) in
  let local_bool = List.fold_right (||) localboollist false in
  (* Print.debug "LOCAL BOOL :- %b \n Global Bool :- %b\n\n" (local_bool) (global_bool); *)
  (global_bool && ( not local_bool) )



let inlinable_definitions =
  let unary_builtin f ty1 ty2 =
    let drt = Type.fresh_dirt () in
    built_in f 1 (Scheme.simple (Type.Arrow (ty1, (ty2, drt))))
  and binary_builtin f ty1 ty2 ty =
    let drt = Type.fresh_dirt ()
    and drt2 = Type.fresh_dirt () in
    built_in f 2 (Scheme.simple (Type.Arrow (ty1, (Type.Arrow (ty2, (ty, drt)), drt2))))
  and polymorphic expr_of_ty = fun () -> expr_of_ty (Type.fresh_ty ())
  and monomorphic expr = fun () -> expr in
  [
    ("=", polymorphic @@ fun t -> binary_builtin "(=)" t t Type.bool_ty);
    ("<", polymorphic @@ fun t -> binary_builtin "(<)" t t Type.bool_ty);
    ("<>", polymorphic @@ fun t -> binary_builtin "(<>)" t t Type.bool_ty);
    (">", polymorphic @@ fun t -> binary_builtin "(>)" t t Type.bool_ty);
    (">=", polymorphic @@ fun t -> binary_builtin "(>=)" t t Type.bool_ty);
    ("<=", polymorphic @@ fun t -> binary_builtin "(<=)" t t Type.bool_ty);
    ("!=", polymorphic @@ fun t -> binary_builtin "(!=)" t t Type.bool_ty);
    ("~-", monomorphic @@ unary_builtin "(~-)" Type.int_ty Type.int_ty);
    ("+", monomorphic @@ binary_builtin "(+)" Type.int_ty Type.int_ty Type.int_ty);
    ("*", monomorphic @@ binary_builtin "( * )" Type.int_ty Type.int_ty Type. int_ty);
    ("-", monomorphic @@ binary_builtin "(-)" Type.int_ty Type.int_ty Type.int_ty);
    ("mod", monomorphic @@ binary_builtin "(mod)" Type.int_ty Type.int_ty Type.int_ty);
    ("~-.", monomorphic @@ unary_builtin "(~-.)" Type.float_ty Type.float_ty);
    ("+.", monomorphic @@ binary_builtin "(+.)" Type.float_ty Type.float_ty Type.float_ty);
    ("*.", monomorphic @@ binary_builtin "( *. )" Type.float_ty Type.float_ty Type.float_ty);
    ("-.", monomorphic @@ binary_builtin "(-.)" Type.float_ty Type.float_ty Type.float_ty);
    ("/.", monomorphic @@ binary_builtin "(/.)" Type.float_ty Type.float_ty Type.float_ty);
    ("/", monomorphic @@ binary_builtin "(/)" Type.int_ty Type.int_ty Type.int_ty);
    ("float_of_int", monomorphic @@ unary_builtin "float_of_int" Type.int_ty Type.float_ty);
    ("^", monomorphic @@ binary_builtin "(^)" Type.string_ty Type.string_ty Type.string_ty);
    ("string_length", monomorphic @@ unary_builtin "string_length" Type.string_ty Type.int_ty)
  ]


let make_var ?(loc=Location.unknown) ann scheme =
  let x = Typed.Variable.fresh ann in
  let x_var = var ~loc x scheme
  and x_pat = {
    term = Typed.PVar x;
    location = loc;
    scheme = scheme
  } in
  x_var, x_pat

let refresh_var ?(loc=Location.unknown) oldvar scheme =
  let x = Typed.Variable.refresh oldvar in
  let x_var = var ~loc x scheme
  and x_pat = {
    term = Typed.PVar x;
    location = loc;
    scheme = scheme
  } in
  x_var, x_pat

type inlinability =
  | NotInlinable (* Pattern variables occur more than once or inside a binder *)
  | NotPresent (* Pattern variables are not present in the body *)
  | Inlinable (* Pattern variables occur each at most once outside a binder *)

let applicable_pattern p vars =
  let rec check_variables = function
    | [] -> NotPresent
    | x :: xs ->
      let inside_occ, outside_occ = Typed.occurrences x vars in
      if inside_occ > 0 || outside_occ > 1 then
        NotInlinable
      else
        begin match check_variables xs with
          | NotPresent -> if outside_occ = 0 then NotPresent else Inlinable
          | inlinability -> inlinability
        end
  in
  check_variables (Typed.pattern_vars p)

let is_atomic e =
  match e with | Var _ -> true | Const _ -> true | _ -> false

let unused x c =
  let vars = Typed.free_vars_comp  c in
  let inside_occ, outside_occ = Typed.occurrences x vars in
  inside_occ == 0 && outside_occ == 0

let refresh_comp c = Typed.refresh_comp [] c
let refresh_handler h = Typed.refresh_handler [] h

let substitute_var_comp comp vr exp = Typed.subst_comp [(vr, exp)] comp

let rec substitute_pattern_comp st c p exp =
  optimize_comp st (Typed.subst_comp (Typed.pattern_match p exp) c)
and substitute_pattern_expr st e p exp =
  optimize_expr st (Typed.subst_expr (Typed.pattern_match p exp) e)

and beta_reduce st ({term = (p, c)} as a) e =
  (* Print.debug  "Inlining? %t[%t -> %t]" (Typed.print_computation c) (Typed.print_pattern p) (Typed.print_expression e) ; *)
  match applicable_pattern p (Typed.free_vars_comp c) with
  | NotInlinable when is_atomic e -> substitute_pattern_comp st c p e
  | Inlinable -> substitute_pattern_comp st c p e
  | NotPresent -> c
  | _ ->
    let a =
      begin match p with
        | {term = Typed.PVar x} ->
          (* Print.debug "Added to stack ==== %t" (Typed.print_variable x); *)
          let st = {st with stack = Common.update x e st.stack} in
          abstraction p (optimize_comp st c)
        | _ ->
          (* Print.debug "We are now in the let in 5 novar for %t" (Typed.print_pattern p); *)
          a
      end
    in
    let_in e a

and optimize_expr st e = reduce_expr st (optimize_sub_expr st e)
and optimize_comp st c = reduce_comp st (optimize_sub_comp st c)

and optimize_sub_expr st e =
  let loc = ehh in
  match e with
  | Record lst ->
    record ~loc (Common.assoc_map (optimize_expr st) lst)
  | Variant (lbl, e) ->
    variant ~loc (lbl, (Common.option_map (optimize_expr st) e))
  | Tuple lst ->
    tuple ~loc (List.map (optimize_expr st) lst)
  | Lambda a ->
    lambda ~loc (optimize_abs st a)
  | Handler h ->
    handler ~loc {
      effect_clauses = Common.assoc_map (optimize_abs2 st) h.effect_clauses;
      value_clause = optimize_abs st h.value_clause;
    }
  | (Var _ | Const _ | BuiltIn _ | Effect _) -> e
and optimize_sub_comp st c =
  let loc = chh in
  match c with
  | Value e ->
    value ~loc (optimize_expr st e)

  | LetRec (defs, c1) when different_branch_specialized defs st ->
    (* List.fold_right (fun (var,abs) st ->
      {st with letrec_memory = (var,abs) :: st.letrec_memory}) defs st; *)
      let [(var,abst)] = defs in
      (* Print.debug "\nst out length %i\n" (List.length (st.handlers_functions_mem) ); *)
      let findresinglobal = fun f_name -> (
                  List.filter
                  (fun (h,old_f,new_f) ->
                      let Var vv = new_f in
                      (f_name == vv) )   !(st.handlers_functions_ref_mem)) in
      begin match findresinglobal var with
      | [] -> let_rec' ~loc (Common.assoc_map (optimize_abs st) defs) (optimize_comp st c1)
      | (h,old_f,new_f) :: _ ->
      (* Print.debug "\nold st length %i\n" (List.length (st.handlers_functions_mem) ); *)
            let st = {st with handlers_functions_mem = (h,old_f,new_f) :: st.handlers_functions_mem} in
            (* Print.debug "\nnew st length %i\n" (List.length (st.handlers_functions_mem) );  *)
            let_rec' ~loc (Common.assoc_map (optimize_abs st) defs) (optimize_comp st c1)
      end


  | LetRec ( [(var,abst)], c1)
      when unused var c1 ->
    (* Print.debug "dropping unused let-rec definition"; *)
    c1
  | LetRec (li, c1) ->
    let_rec' ~loc (Common.assoc_map (optimize_abs st) li) (optimize_comp st c1)
  | Match (e, li) ->
    match' ~loc (optimize_expr st e) (List.map (optimize_abs st) li)
  | Apply (e1, e2) ->
    apply ~loc (optimize_expr st e1) (optimize_expr st e2)
  | Handle (e, c1) ->
    handle ~loc (optimize_expr st e) (optimize_comp st c1)
  | Call (eff, e1, a1) ->
    call ~loc eff (optimize_expr st e1) (optimize_abs st a1)
  | Bind (c1, a1) ->
    bind ~loc (optimize_comp st c1) (optimize_abs st a1)
and optimize_abs st {term = (p, c); location = loc} =
  abstraction ~loc p (optimize_comp st c)
and optimize_abs2 st a2 = a2a2 @@ optimize_abs st @@ a22a @@ a2

and reduce_expr st e =
  let e' = match e with

  | _ when outOfFuel st -> e

  | Var x ->
    begin match find_inlinable st x with
      | Some ({term = Handler _} as d) -> reduce_expr st (refresh_expr d)
      | Some d -> reduce_expr st d
      | _ -> e
    end

  | Effect eff ->
    let (eff_name, (ty_par, ty_res)) = eff in
    let param_var, param_pat = make_var "param" (Scheme.simple ty_par) in
    let result_var, result_pat = make_var "result" (Scheme.simple ty_res) in
    let k = abstraction result_pat (value result_var) in
    let call_eff_param_var_k = reduce_comp st (call eff param_var k) in
    let res =
      lambda (abstraction param_pat call_eff_param_var_k)
    in
    (* Body is already reduced and it's a lambda *)
    res

  | _ -> e
  in
  (* if e <> e' then *)
(*   Print.debug ~loc:e.Typedhh "%t : %t@.~~~>@.%t : %t@.\n"
    (Typed.print_expression e) (Scheme.print_ty_scheme e.Typed.scheme)
    (Typed.print_expression e') (Scheme.print_ty_scheme e'.Typed.scheme); *)
  e'


and reduce_comp st c =
  let c' = match c with

  | _ when outOfFuel st -> c

  | Match ({term = Const cst}, cases) ->
    let rec find_const_case = function
      | [] -> c
      | ({term = {term = PConst cst'}, c'}) :: _ when Const.equal cst cst'
         -> useFuel st; c'
      | _ :: cases -> find_const_case cases
    in
    find_const_case cases

(*   | Bind (c1, c2) when is_pure c1 ->
    useFuel st;
    st.optimization_Do_Ret := !(st.optimization_Do_Ret ) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    beta_reduce st c2 (reduce_expr st ( c1)) *)

  | Bind ({term = Bind (c1, {term = (p1, c2)})}, c3) ->
    useFuel st;
    st.optimization_Do_Op := !(st.optimization_Do_Op) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    let bind_c2_c3 = reduce_comp st (bind c2 c3) in
    let res =
      bind c1 (abstraction p1 bind_c2_c3)
    in
    reduce_comp st res

  | Bind ({term = Call (eff, param, k)}, c) ->
    useFuel st;
    let {term = (k_pat, k_body)} = refresh_abs k in
    let bind_k_c = reduce_comp st (bind k_body c) in
    let res =
      call eff param (abstraction k_pat bind_k_c)
    in
    reduce_comp st res

  | Handle (h, {term = LetRec (defs, co)}) ->
    useFuel st;
    st.optimization_handler_With_LetRec := !(st.optimization_handler_With_LetRec) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    let handle_h_c = reduce_comp st (handle h co) in
    let res =
      let_rec' defs handle_h_c
    in
    reduce_comp st res

  | Handle ({term = Handler h}, c1)
        when (is_pure_for_handler c1 h.effect_clauses) ->
    useFuel st;
    st.optimization_handler_With_Pure := !(st.optimization_handler_With_Pure) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    (* Print.debug "Remove handler, since no effects in common with computation"; *)
    reduce_comp st (bind c1 h.value_clause)

  | Handle ({term = Handler h} as handler, {term = Bind (c1, {term = (p1, c2)})})
        when (is_pure_for_handler c1 h.effect_clauses) ->
    useFuel st;
    st.optimization_handler_with_do := !(st.optimization_handler_with_do) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    (* Print.debug "Remove handler of outer Bind, since no effects in common with computation"; *)
    reduce_comp st (bind (reduce_comp st c1) (abstraction p1 (reduce_comp st (handle (refresh_expr handler) c2))))

  | Handle ({term = Handler h}, {term = Bind (c1, {term = (p1, c2)})})
        when (is_pure_for_handler c2 h.effect_clauses) ->
    useFuel st;
    st.optimization_handler_With_Ret := !(st.optimization_handler_With_Ret) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    (* Print.debug "Move inner bind into the value case"; *)
    let new_value_clause = optimize_abs st (abstraction p1 (bind (reduce_comp st c2) (refresh_abs h.value_clause))) in
    let hdlr = handler {
      effect_clauses = h.effect_clauses;
      value_clause = refresh_abs new_value_clause;
    } in
    reduce_comp st (handle (refresh_expr hdlr) c1)

  | Handle ({term = Handler h} as h2, {term = Bind (c1, {term = (p, c2)})}) ->
    useFuel st;
    st.optimization_handler_With_Ret := !(st.optimization_handler_With_Ret) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    (* Print.debug "Move (dirty) inner bind into the value case"; *)
    let new_value_clause = optimize_abs st (abstraction p (handle (refresh_expr h2) (refresh_comp (reduce_comp st c2) ))) in
    let hdlr = handler {
      effect_clauses = h.effect_clauses;
      value_clause = refresh_abs new_value_clause;
    } in
    reduce_comp st (handle (refresh_expr hdlr) (refresh_comp c1))

(*   | Handle ({term = Handler h}, c) when is_pure c ->
    useFuel st;
    st.optimization_handler_With_Pure := !(st.optimization_handler_With_Pure) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    beta_reduce st h.value_clause (reduce_expr st (pure c)) *)

  | Handle ({term = Handler h} as handler, {term = Call (eff, param, k)}) ->
    useFuel st;
    let {term = (k_pat, k_body)} = refresh_abs k in
    let handled_k =
      abstraction k_pat
        (reduce_comp st (handle (refresh_expr handler) k_body))
    in
    begin match Common.lookup eff h.effect_clauses with
      | Some eff_clause ->
      st.optimization_handler_With_Handled_Op := !(st.optimization_handler_With_Handled_Op) + 1;
      st.optimization_total := !(st.optimization_total) + 1;
        let {term = (p1, p2, c)} = refresh_abs2 eff_clause in
        (* Shouldn't we check for inlinability of p1 and p2 here? *)
        let PVar v
        substitute_pattern_comp st (substitute_pattern_comp st c p1 param) p2 (lambda handled_k)
      | None ->
        let res =
          call eff param handled_k
        in
        reduce_comp st res
    end

  | Apply ({term = Lambda a}, e) ->
    useFuel st;
    st.optimization_App_Fun := !(st.optimization_App_Fun ) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    beta_reduce st a e

(*   | Apply ({term = Pure {term = LetRec (defs,c)}}, e) ->
    useFuel st;
    st.optimization_App_Fun := !(st.optimization_App_Fun ) + 1;
    st.optimization_total := !(st.optimization_total) + 1;
    let_rec' defs (apply (pure c) e)
 *)
(*   | Apply ({term = Var v}, e2) ->
    begin match Common.lookup v st.impure_wrappers with
      | Some f ->
        useFuel st;
        st.optimization_App_Fun := !(st.optimization_App_Fun ) + 1;
        st.optimization_total := !(st.optimization_total) + 1;
        let res =
          value (pure (apply f e2))
        in
        reduce_comp st res
      | None -> c
    end
 *)

  | Handle (e1, {term = Apply (ae1, ae2)}) ->
    useFuel st;
    begin match ae1 with
      | Var v ->
            begin match (find_in_handlers_func_mem st v e1) with
             (*function exist,Same handler, same value clause*)
             | (true,Some new_f_exp,None) ->
                                let res = apply new_f_exp ae2
                                in reduce_comp st res
             | (true,Some special_f_exp, Some original_val_clause) ->
                  let Handler h1 = e1 in
                  let h1_v_clause = h1.value_clause in
                  let orig_vc_lambda = optimize_expr st (lambda (h1_v_clause)) in
                  let res = apply special_f_exp (tuple [ae2;orig_vc_lambda]) in
                  reduce_comp st res

             (*function exist,Same handler, different value clause*)
             | (false, Some new_f_exp,Some original_val_clause)->
               begin match (find_in_let_rec_mem st v) with
                | Some abs ->
                  st.optimization_function_specialization := !(st.optimization_function_specialization ) + 1;
                  st.optimization_total := !(st.optimization_total) + 1;
                  let (let_rec_p,let_rec_c) = abs in
                  (* Print.debug "THE ABSTRACTION OF SAME HANDLER DIFF VALUE :- %t" (Typed.print_abstraction abs); *)
                  let Handler ha = e1 in
                  (* Print.debug "THE VALUE CLAUSE :- %t" (Typed.print_abstraction ha.value_clause); *)
                  let ctx_val, (tyin_val , (tyout_val,drt_val)), cnstrs_val = ha.value_clause.scheme in
                  let continuation_var_scheme = (ctx_val, Type.Arrow(tyin_val , Type.fresh_dirty ()), cnstrs_val) in
                  let k_var, k_pat = make_var "k_val"  continuation_var_scheme in
                  let ctx1, ty1, cnstrs1 = let_rec_p.scheme in
                  let newf_input_tuple_pat = {
                    term = PTuple [let_rec_p; k_pat];
                    scheme = (
                      ctx_val @ ctx1,
                      Type.Tuple [ty1 ; Type.Arrow(tyin_val , Type.fresh_dirty ())],
                      Constraints.union cnstrs_val cnstrs1
                    );
                    location = ae1hh;
                  } in
                  let(newf_ctx,newf_ty,newf_const) = newf_input_tuple_pat.scheme in
                  let (f_ctx,ae1Ty,f_const) = ae1.scheme in
                  let Type.Arrow(f_ty_in, f_ty_out ) = Constraints.expand_ty ae1Ty in
                  let newf_scheme = Scheme.clean_ty_scheme ~loc:chh (newf_ctx , Type.Arrow (newf_ty, (tyout_val,drt_val)), newf_const) in
                  let newf_var, newf_pat = make_var "new_special_var"  newf_scheme in
                  let Var newfvar = newf_var in
                  let Handler hndlr = e1 in
                  let vc_var_scheme = (ctx_val,tyin_val,cnstrs_val) in
                  let vc_var, vc_pat = make_var "vcvar"  vc_var_scheme in
                  let new_value_clause = abstraction vc_pat (apply k_var vc_var) in
                  let new_handler =  handler {
                                      effect_clauses = hndlr.effect_clauses;
                                      value_clause =  new_value_clause;
                                    } in
                  let st = {st with handlers_functions_cont_mem = (new_handler, v, newf_var ) :: (st.handlers_functions_cont_mem)} in
                  let new_handler_call = reduce_comp st (handle new_handler let_rec_c) in
                  let newf_body = abstraction newf_input_tuple_pat new_handler_call in
                  let defs = [(newfvar, newf_body)] in
                  let orig_vc_lambda = optimize_expr st (lambda (hndlr.value_clause)) in
                  let res = let_rec' defs @@  apply newf_var  ( tuple [ae2; orig_vc_lambda] ) in
                  (* Print.debug "THE resulting computation :-  %t" (Typed.print_computation res); *)
                   optimize_comp st res
                | _ -> c
               end
             | (true, None,_) ->
                  c
             | _ ->
               begin match find_in_stack st v with
               | Some ({term = Lambda k}) ->
                  st.optimization_function_specialization := !(st.optimization_function_specialization ) + 1;
                  st.optimization_total := !(st.optimization_total) + 1;
                  let {term = (newdp, newdc)} = refresh_abs k in
                  let (h_ctx,Type.Handler(h_ty_in, (ty_out, drt_out)),h_const) = e1.scheme in
                  let (f_ctx,ae1Ty,f_const) = ae1.scheme in
                  let Type.Arrow(f_ty_in, f_ty_out ) = Constraints.expand_ty ae1Ty in
                  let constraints = Constraints.list_union [h_const; f_const]
                                    |> Constraints.add_dirty_constraint ~loc:chh f_ty_out h_ty_in in
                  let sch = (h_ctx @ f_ctx, (Type.Arrow(f_ty_in,(ty_out,drt_out))), constraints) in
                  let function_scheme = Scheme.clean_ty_scheme ~loc:chh sch in
                  let f_var, f_pat = refresh_var v function_scheme in
                  let f_def =
                    lambda @@
                    abstraction newdp @@
                    handle e1 newdc in
                  let res =
                    let_in f_def @@
                    abstraction f_pat @@
                    apply f_var ae2
                  in
                  optimize_comp st res
                | _ ->
                       begin match (find_in_let_rec_mem st v) with
                       | Some abs ->
                            st.optimization_function_specialization := !(st.optimization_function_specialization ) + 1;
                            st.optimization_total := !(st.optimization_total) + 1;
                            let (let_rec_p,let_rec_c) = abs in
                            let (h_ctx,Type.Handler(h_ty_in, (ty_out, drt_out)),h_const) = e1.scheme in
                            let (f_ctx,ae1Ty,f_const) = ae1.scheme in
                            let Type.Arrow(f_ty_in, f_ty_out ) = Constraints.expand_ty ae1Ty in
                            let constraints = Constraints.list_union [h_const; f_const]
                                  |> Constraints.add_dirty_constraint ~loc:chh f_ty_out h_ty_in in
                            let sch = (h_ctx @ f_ctx, (Type.Arrow(f_ty_in,(ty_out,drt_out))), constraints) in
                            let function_scheme = Scheme.clean_ty_scheme ~loc:chh sch in
                            let new_f_var, new_f_pat = refresh_var v function_scheme in
                            let new_handler_call = handle e1 let_rec_c in
                            let Var newfvar = new_f_var in
                            let defs = [(newfvar, (abstraction let_rec_p new_handler_call ))] in
                            let st = {st with handlers_functions_mem = (e1,v,new_f_var) :: st.handlers_functions_mem} in
                            st.handlers_functions_ref_mem := (e1,v,new_f_var) :: !(st.handlers_functions_ref_mem) ;
                            let res =
                              let_rec' defs @@
                              apply new_f_var ae2
                            in
                            optimize_comp st res
                       | _ ->
                        (* Print.debug "Its a none"; *)
                                    (* Print.debug "The handle exp : %t" (Typed.print_expression ae1); *)
                                    c
                       end
               end
        end
(*
      | PureApply ({term = Var fname}, pae2)->
        begin match find_in_stack st fname with
          | Some {term = PureLambda {term = (dp1, {term = Lambda ({term = (dp2,dc)})})}} ->
            let f_var, f_pat = make_var "newvar" ae1.scheme in
            let f_def =
              pure_lambda @@
              pure_abstraction dp1 @@
              lambda @@
              abstraction dp2 @@
              (* Why don't we refresh dc here? *)
              handle e1 dc
            in
            let res =
              let_in f_def @@
              abstraction f_pat @@
              apply (pure_apply f_var pae2) ae2
            in
            optimize_comp st res
          | _ -> c
        end
*)
      | _ -> c
    end

| Handle (e1, {term = Match (e2, cases)}) ->
    useFuel st;
    let push_handler = fun {term = (p, c)} ->
      abstraction p (reduce_comp st (handle (refresh_expr e1) c))
    in
    let res =
      match' e2 (List.map push_handler cases)
    in
    res

(*
    (*
      let f = \p. val lambda in c
       ~~> (append f := f1 to impure_wrappers)
      let f1 = \*p. lambda
      let f = \new_p. val (f1 new_p) in
      c
    *)
  | LetIn ({term = Lambda ({term = (p, {term = Value ({term = Lambda _ } as in_lambda)} )})}, ({term = ({term = PVar f} as f_pat,_)} as a) )->
    Print.debug "We are now in the let in 4 for %t" (Typed.print_pattern f_pat);
    let f1_var, f1_pat = make_var "f1" f_pat.scheme in
    let new_p_var, new_p_pat = make_var "new_p" p.scheme in
    let first_fun =
      pure_lambda @@
      pure_abstraction p @@
      in_lambda
    and second_fun =
      lambda @@
      abstraction new_p_pat @@
      value (pure_apply f1_var new_p_var)
    in
    let st = {st with impure_wrappers = (f, f1_var) :: st.impure_wrappers} in
    let res =
      let_in first_fun @@
      abstraction f1_pat @@
      let_in second_fun @@
      a
    in
    optimize_comp st res
*)

  (* XXX simplify *)
  | LetRec (defs, co) ->
    useFuel st;
    (*Print.debug "the letrec comp  %t" (Typed.print_computation co);*)
    let st =
    List.fold_right (fun (var,abs) st ->
            (*Print.debug "ADDING %t and %t to letrec" (Typed.print_variable var) (Typed.print_abstraction abs);*)
            {st with letrec_memory = (var,abs) :: st.letrec_memory}) defs st in
    let_rec' defs (reduce_comp st co)


  | _ -> c

  in
  (*
  if c <> c' then
   Print.debug ~loc:c.Typedhh "%t : %t@.~~~>@.%t : %t@.\n"
    (Typed.print_computation c) (Scheme.print_dirty_scheme c.Typed.scheme)
    (Typed.print_computation c') (Scheme.print_dirty_scheme c'.Typed.scheme);*)
  c'


let optimize_command st =
  refuel st;
  function
  | Typed.Computation c ->
    st, Typed.Computation (optimize_comp st c)
  | Typed.TopLet (defs, vars) ->
    let defs' = Common.assoc_map (optimize_comp st) defs in
    let st' = begin match defs' with
      (* If we define a single simple handler, we inline it *)
      | [({ term = Typed.PVar x}, { term = Value ({ term = Handler _ } as e)})] ->
        {st with inlinable = Common.update x (fun () -> (optimize_expr st e)) st.inlinable}
      | [({ term = Typed.PVar x}, ({ term = Value ({term = Lambda _ } as e )} ))] ->
        {st with stack = Common.update x e st.stack}
      | _ -> st
    end
    in
    st', Typed.TopLet (defs', vars)
  | Typed.TopLetRec (defs, vars) ->
    let defs' = Common.assoc_map (optimize_abs st) defs in
    let st' =
    List.fold_right (fun (var,abs) st ->
            (* Print.debug "ADDING %t and %t to letrec" (Typed.print_variable var) (Typed.print_abstraction abs); *)
            {st with letrec_memory = (var,abs) :: st.letrec_memory}) defs st in
    st', Typed.TopLetRec (defs', vars)

  | Typed.External (x, _, f) as cmd ->
    let st' =
      begin match Common.lookup f inlinable_definitions with
        (* If the external function is one of the predefined inlinables, we inline it *)
        | Some e -> {st with inlinable = Common.update x e st.inlinable}
        | None -> st
      end
    in
    st', cmd
  | Typed.DefEffect _ | Typed.Reset | Typed.Quit | Typed.Use _
  | Typed.Tydef _ | Typed.TypeOf _ | Typed.Help as cmd -> st, cmd

let optimize_commands cmds =
  refuel initial;
  let _, cmds =
  List.fold_left (fun (st, cmds) (cmd, loc) ->
    let st', cmd' = optimize_command st cmd in
    st', (cmd', loc) :: cmds
  ) (initial, []) cmds
in
(* Print.debug "The optimization total %i" !(initial.optimization_total);
Print.debug "The optimization App-Fun %i" !(initial.optimization_App_Fun);
Print.debug "The optimization Do-Ret %i" !(initial.optimization_Do_Ret);
Print.debug "The optimization Do-Op %i" !(initial.optimization_Do_Op);
Print.debug "The optimization With-Ret %i" !(initial.optimization_handler_With_Ret);
Print.debug "The optimization With-Pure %i" !(initial.optimization_handler_With_Pure);
Print.debug "The optimization With-do %i" !(initial.optimization_handler_with_do);
Print.debug "The optimization handled-op %i" !(initial.optimization_handler_With_Handled_Op);
Print.debug "The optimization With-LetRec %i" !(initial.optimization_handler_With_LetRec);
Print.debug "The optimization function Specilization  %i" !(initial.optimization_function_specialization);
Print.debug "Simplifications %i"
  ( !(initial.optimization_App_Fun)+ !(initial.optimization_Do_Ret)+ !(initial.optimization_Do_Op) );
Print.debug "Handler Reductions %i"
 ( !(initial.optimization_handler_With_Ret)+ !(initial.optimization_handler_With_Pure)
  + !(initial.optimization_handler_with_do) + !(initial.optimization_handler_With_Handled_Op)
  +  !(initial.optimization_handler_With_LetRec));
Print.debug "Specialization %i" !(initial.optimization_function_specialization); *)
List.rev cmds;
*)
