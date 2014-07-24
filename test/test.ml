open OUnit

let _ = run_test_tt_main
    ("Lifter" >:::
     [
       TestArch_i386.tests;
       TestArch_i386_64.tests;
       TestArch_arm.tests;
     ])
