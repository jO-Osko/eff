open SkelEffSyntax
module SkelEff = SkelEffSyntax

let rec typed_to_erasure_ty sub typed_ty =
  match typed_ty with
  | Types.TyParam p -> (
      match Assoc.lookup p sub with Some x' -> x' | None -> assert false)
  | Types.Arrow (t1, (t2, drt)) ->
      let t1' = typed_to_erasure_ty sub t1 in
      let t2' = typed_to_erasure_ty sub t2 in
      Types.SkelArrow (t1', t2')
  | Types.Tuple tys -> Types.SkelTuple (List.map (typed_to_erasure_ty sub) tys)
  | Types.Handler ((t1, drt1), (t2, drt2)) ->
      let t1' = typed_to_erasure_ty sub t1 in
      let t2' = typed_to_erasure_ty sub t2 in
      Types.SkelHandler (t1', t2')
  | Types.PrimTy p -> Types.PrimSkel p
  | Types.QualTy (_, tty) -> typed_to_erasure_ty sub tty
  | Types.QualDirt (_, tty) -> typed_to_erasure_ty sub tty

and typed_to_erasure_cmp_ty sub (ty, _drt) = typed_to_erasure_ty sub ty

let rec typed_to_erasure_exp sub tt =
  match tt with
  | Typed.Var v -> SkelEff.EVar v
  | Typed.Const c -> SkelEff.EConst c
  | Typed.Tuple elist ->
      SkelEff.ETuple (List.map (fun x -> typed_to_erasure_exp sub x) elist)
  | Typed.Lambda abs -> SkelEff.ELambda (typed_to_erasure_abs_with_ty sub abs)
  | Typed.Effect e -> SkelEff.EEffect e
  | Typed.Handler h ->
      let e_pat, tty, v_comp = h.value_clause in
      let op_c = h.effect_clauses in
      let new_vc = typed_to_erasure_abs_with_ty sub h.value_clause in
      let new_op_c =
        Assoc.kmap
          (fun (eff, e_a2) ->
            let new_e_a2 = typed_to_erasure_abs_2 sub e_a2 in
            (eff, new_e_a2))
          op_c
      in
      let new_h = { effect_clauses = new_op_c; value_clause = new_vc } in
      SkelEff.EHandler new_h
  | CastExp (e, _) -> typed_to_erasure_exp sub e
  | LambdaTyCoerVar (_, _, e) -> typed_to_erasure_exp sub e
  | LambdaDirtCoerVar (_, _, e) -> typed_to_erasure_exp sub e
  | ApplyTyCoercion (e, _) -> typed_to_erasure_exp sub e
  | ApplyDirtCoercion (e, _) -> typed_to_erasure_exp sub e

and typed_to_erasure_comp sub tt =
  match tt with
  | Typed.Value e -> SkelEff.EValue (typed_to_erasure_exp sub e)
  | Typed.LetVal (e, (p, _, c)) ->
      let p' = typed_to_erasure_pattern p in
      let e' = typed_to_erasure_exp sub e in
      let c' = typed_to_erasure_comp sub c in
      SkelEff.ELetVal (p', e', c')
  | Typed.Apply (e1, e2) ->
      let e1' = typed_to_erasure_exp sub e1 in
      let e2' = typed_to_erasure_exp sub e2 in
      SkelEff.EApply (e1', e2')
  | Typed.Handle (e, c) ->
      let e' = typed_to_erasure_exp sub e in
      let c' = typed_to_erasure_comp sub c in
      SkelEff.EHandle (e', c')
  | Typed.Call (eff, e, abs) ->
      let e' = typed_to_erasure_exp sub e in
      let abs' = typed_to_erasure_abs_with_ty sub abs in
      SkelEff.ECall (eff, e', abs')
  | Typed.Op (eff, e) -> failwith __LOC__
  | Typed.Bind (c, a) ->
      let c' = typed_to_erasure_comp sub c in
      let a' = typed_to_erasure_abs sub a in
      SkelEff.EBind (c', a')
  | Typed.Match (e, ty, alist, loc) ->
      let e' = typed_to_erasure_exp sub e in
      let ty' = typed_to_erasure_cmp_ty sub ty in
      let alist' = List.map (typed_to_erasure_abs sub) alist in
      SkelEff.EMatch (e', ty', alist', loc)
  | Typed.CastComp (c, _) -> typed_to_erasure_comp sub c
  | Typed.CastComp_ty (c, _) -> typed_to_erasure_comp sub c
  | Typed.CastComp_dirt (c, _) -> typed_to_erasure_comp sub c
  | Typed.LetRec ([ (var, argTy, resTy, abs) ], c1) ->
      SkelEff.ELetRec
        ( [
            ( var,
              typed_to_erasure_ty sub argTy,
              typed_to_erasure_cmp_ty sub resTy,
              typed_to_erasure_abs sub abs );
          ],
          typed_to_erasure_comp sub c1 )

and typed_to_erasure_abs_with_ty sub (e_p, e_ty, e_c) =
  ( typed_to_erasure_pattern e_p,
    typed_to_erasure_ty sub e_ty,
    typed_to_erasure_comp sub e_c )

and typed_to_erasure_abs sub (e_p, e_c) =
  (typed_to_erasure_pattern e_p, typed_to_erasure_comp sub e_c)

and typed_to_erasure_abs_2 sub (e_p1, e_p2, e_c) =
  ( typed_to_erasure_pattern e_p1,
    typed_to_erasure_pattern e_p2,
    typed_to_erasure_comp sub e_c )

and typed_to_erasure_pattern = function
  | Typed.PVar x -> SkelEff.PEVar x
  | Typed.PAs (p, x) -> SkelEff.PEAs (typed_to_erasure_pattern p, x)
  | Typed.PTuple ps -> SkelEff.PETuple (List.map typed_to_erasure_pattern ps)
  | Typed.PRecord ass ->
      SkelEff.PERecord (Assoc.map typed_to_erasure_pattern ass)
  | Typed.PVariant (lbl, p) ->
      SkelEff.PEVariant (lbl, typed_to_erasure_pattern p)
  | Typed.PConst const -> SkelEff.PEConst const
  | Typed.PNonbinding -> SkelEff.PENonbinding
