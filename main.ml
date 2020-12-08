open Origami;;

(* #use "origami.ml";; *)

let op=[((1.0,8.0),(5.0,10.0));((2.0,7.0),(9.0,3.0));((0.0,10.0),(7.0,0.0));((10.0,5.0),(6.0,9.0));((0.0,1.0),(8.0,2.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test88=skladaj op kartka;;

Printf.printf "przebicia %d\n" (test88 (1., 9.));;
Printf.printf "przebicia %d\n" (test88 (1.1, 9.));;
Printf.printf "przebicia %d\n" (test88 (1., 9.1));;
Printf.printf "przebicia %d\n" (test88 (0.9, 9.));;
Printf.printf "przebicia %d\n" (test88 (1., 8.9));;


assert (test88 (1.0,9.0)=1);;

