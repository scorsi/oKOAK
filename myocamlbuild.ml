open Ocamlbuild_plugin;;

ocaml_lib ~extern:true "llvm";;
ocaml_lib ~extern:true "llvm_analysis";;
ocaml_lib ~extern:true "llvm_executionengine";;
ocaml_lib ~extern:true "llvm_target";;
ocaml_lib ~extern:true "llvm_scalar_opts";;

flag ["link"; "ocaml"; "g++"] (S[A"-cc"; A"g++"]);;
dep ["link"; "ocaml"; "use_c_cast"] ["cast.o"];;
dep ["link"; "ocaml"; "use_c_put"] ["put.o"];;