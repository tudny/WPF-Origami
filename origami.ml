(******************************)
(**         ORIGAMI          **)
(** Autor: Aleksander Tudruj **)
(******************************)


type point = float * float

type kartka = point -> int

let int_of_bool = function
  | true -> 1
  | false -> 0

let square x = x *. x

let prostokat (x1, y1 : point) (x2, y2 : point) (a, b : point) =
  int_of_bool
    (x1 <= a && a <= x2 &&
    y1 <= b && b <= y2)

let kolko (x, y) r (a, b) =
  let dx = (x -. a)
  and dy = (y -. b) in
  int_of_bool
    (square dx +. square dy <= square r)

let det (x, y) (a, b) =
  x *. b -. y *. a

let cmp (x1, y1) (x2, y2) (a, b) =
  det (x2 -. x1, y2 -. y1) (a -. x1, b -. y1)

let zloz ((x1, y1) as p1) ((x2, y2) as p2) (kar : kartka) ((a, b) as q) =
  match cmp p1 p2 q with
  | 0. -> kar q
  | w when w < 0. -> 0
  | _ ->
    let bB = x2 -. x1
    and aA = y1 -. y2
    and cC = x1 *. (y2 -. y1) -. y1 *. (x2 -. x1) in
      let sum = square aA +. square bB
      and diff = square aA -. square bB
      and dab = 2. *. aA *. bB in
        let u_m = (-.diff) *. a -. dab *. b -. 2. *. aA *. cC
        and v_m = diff *. b -. dab *. a -. 2. *. bB *. cC in
          let u = u_m /. sum
          and v = v_m /. sum in
            kar q + kar (u, v)

let skladaj li kar =
  List.fold_left (fun acc (p, q) -> zloz p q acc) kar li
