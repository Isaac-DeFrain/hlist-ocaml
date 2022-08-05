open OUnit

let _ =
  run_test_tt_main Test_og_hlist.suite
  @ run_test_tt_main Test_my_hlist.simple_suite
  @ run_test_tt_main Test_my_hlist.complex_suite
