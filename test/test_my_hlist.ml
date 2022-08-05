open Lib
open OUnit

module Simple = struct
  open My_hlist.Simple

  let sample =
    [ (* 0 *)
      123
    ; (* 1 *)
      "abc"
    ; (* 2 *)
      3.14
    ; (* 3 *)
      false
    ; (* 4 *)
      List.[ 1; -3; 44 ]
    ; (* 5 *)
      [ `Foo
      ; object
          method bar = "bar"
        end
      ; (fun x -> x + 1)
      ]
    ]

  let one = Succ Zero

  let two = Succ one

  let three = Succ two

  let four = Succ three

  let five = Succ four

  let test_get () =
    OUnit.assert_equal (get Zero sample) 123;
    OUnit.assert_equal (get (Succ Zero) sample) "abc";
    OUnit.assert_equal (get (Succ (Succ (Succ Zero))) sample) false
end

module Complex = struct
  open My_hlist.Complex

  let sample =
    [ (* 0 *)
      123
    ; (* 1 *)
      "abc"
    ; (* 2 *)
      3.14
    ; (* 3 *)
      false
    ; (* 4 *)
      List.[ 1; -3; 44 ]
    ; (* 5 *)
      [ `Foo
      ; object
          method bar = "bar"
        end
      ; (fun x -> x + 1)
      ]
    ]

  let one = Succ Zero

  let two = Succ one

  let three = Succ two

  let four = Succ three

  let five = Succ four

  let test_get () =
    OUnit.assert_equal (get Zero sample) 123;
    OUnit.assert_equal (get (Succ Zero) sample) "abc";
    OUnit.assert_equal (get (Succ (Succ (Succ Zero))) sample) false

  let test_put_get () =
    OUnit.assert_equal (get Zero (put Zero sample `Hello)) `Hello;
    OUnit.assert_equal (get Zero (put five sample `Hello)) 123;
    OUnit.assert_equal (get one (put five sample `Hello)) "abc"
end

let simple_suite = "Test simple module" >::: [ "test_get" >:: Simple.test_get ]

let complex_suite =
  "Test complex module"
  >::: [ "test_get" >:: Complex.test_get
       ; "test_put_get" >:: Complex.test_put_get
       ]
