module Tests.test_choice

open System
open Xunit

module C = FSharpPlus.Choice
module OUnit=
  let inline assert_equal expected actual=Assert.Same (expected, actual)

let (>>=) x f = C.bind f x

[<Fact>]
let test_return () =
  let c = C.mplus (C.Return 1) (C.Return 2) in
  let l = List.sort (C.run_all c) in
  OUnit.assert_equal [1;2] l
  
[<Fact>]
let test_delay () =
  let r = ref 0 in
  let rec generate () =
    incr r;
    C.Return !r
  in
  let c = C.delay generate in
  incr r;
  let l = List.sort (C.run_n 3 c) in
  OUnit.assert_equal [2] l

[<Fact>]
let test_from_fun () =
  let r = ref 0 in
  let generate () =
    incr r;
    Some !r
  in
  let c = C.from_fun generate in
  let l = List.sort (C.run_n 5 c) in
  OUnit.assert_equal [1;2;3;4;5] l

[<Fact>]
let test_bind () =
  let c1 = C.of_list [1;3] in
  let c2 = c1 >>= fun x -> C.of_list [x; x+1] in
  let l = List.sort (C.run_all c2) in
  OUnit.assert_equal [1;2;3;4] l

(*
[<Fact>]
let test_interleave () =
  let c1 = C.of_list [1;3;5] in
  let c2 = C.of_list [2;4;6] in
  let l = ref [] in
  C.iter (C.interleave c1 c2) (fun x -> l := !l @ [x]; true);
  OUnit.assert_equal [1;2;3;4;5;6] !l
  *)

[<Fact>]
let test_ite1 () =
  let c = C.of_list [1;2] in
  let c' = C.ite c (fun x -> C.Return (x+1)) (C.Return 42) in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [2;3] l

[<Fact>]
let test_ite2 () =
  let c = C.fail in
  let c' = C.ite c (fun x -> C.Return (x+1)) (C.Return 42) in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [42] l

[<Fact>]
let test_map () =
  let c = C.of_list [1;2;3;4;5] in
  let succ a=a+1
  let c' = C.map succ c in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [2;3;4;5;6] l

[<Fact>]
let test_once () =
  let c = C.of_list [1;2;3] in
  let c' = C.once c in
  let l = List.sort (C.run_all c') in
  OUnit.assert_equal [1] l

[<Fact>]
let test_guard () = 
  let computation = 
      C.of_list [1;2;3] >>= fun x ->
      C.guard (x <> 2) >>= fun () ->
      C.Return x
  in
  let l = List.sort (C.run_all computation) in
  OUnit.assert_equal [1;3] l

