open Apron

let test_assign name man =
  let env = Environment.make (Array.map Var.of_string [|"x"; "y"; "z"|]) [||] in
  let top = Abstract1.top man env in
  let e = Texpr1.binop Sub
      (Texpr1.unop Neg (Texpr1.cst env (Coeff.s_of_mpq (Mpq.of_string "9223372036854775807")))  Int Zero)
      (Texpr1.cst env (Coeff.s_of_int 1))
      Double Zero in
  let a = Abstract1.assign_texpr man top (Apron.Var.of_string "x") e None in
  Format.printf "[%s] after x = %a, a = %a@." name Texpr1.print e Abstract1.print a

let () = test_assign "elina_poly" (Elina_poly.manager_alloc_loose ())
let () = test_assign "elina_oct" (Elina_oct.manager_alloc ())
let () = test_assign "apron_oct" (Oct.manager_alloc ())
let () = test_assign "apron_poly" (Polka.manager_alloc_loose ())

let test_assume name man =
  let env = Environment.make (Array.map Var.of_string [|"x"; "y"; "z"|]) [||] in
  let a = Abstract1.top man env in 
  (* assume x <= -9223372036854775808 /\ x >= -9223372036854775808,
     but in two steps (no issue otherwise) *)
  let e =
    Texpr1.binop Sub
      (Texpr1.var env (Var.of_string "x"))
      (Texpr1.cst env (Coeff.neg @@ Coeff.s_of_mpq (Mpq.of_string "9223372036854775808")))
      Double Near in
  let arr = Tcons1.array_make env 1 in
  let () = Tcons1.array_set arr 0 (Tcons1.make e Tcons1.SUPEQ) in
  let a = Abstract1.meet_tcons_array man a arr in
  let () = Format.printf "[%s] after meeting with constraint %a, a = %a@." name
    (fun fmt x -> Tcons1.array_print fmt x)
    arr Abstract1.print a in 
  let e' =
    Texpr1.binop Sub
      (Texpr1.cst env (Coeff.neg @@ Coeff.s_of_mpq (Mpq.of_string "9223372036854775808")))
      (Texpr1.var env (Var.of_string "x"))
      Double Near
  in
  let () = Tcons1.array_set arr 0 (Tcons1.make e' Tcons1.SUPEQ) in 
  let a = Abstract1.meet_tcons_array man a arr in
  Format.printf "[%s] after meeting with constraint %a, a = %a@." name
    (fun fmt x -> Tcons1.array_print fmt x)
    arr Abstract1.print a


let () = test_assume "apron_oct" (Oct.manager_alloc ())
let () = test_assume "apron_poly" (Polka.manager_alloc_loose ())
let () = test_assume "elina_oct" (Elina_oct.manager_alloc ())
let () = test_assume "elina_poly" (Elina_poly.manager_alloc_loose ())
