type 'a my_list =
| Nil
| Cons of 'a * 'a my_list


let string_of_list str_fun l =
  let rec string_content = function
    | Nil  -> ""
    | Cons(x, Nil)  -> (str_fun x)
    | Cons(x,q) -> (str_fun x) ^ ", " ^ (string_content q)
  in "[" ^ (string_content l) ^ "]" ;;

let hd l =
  match l with
  | Nil -> None
  | Cons ( x , _ ) -> Some( x );;

let tl l =
  match l with
  | Nil -> None
  | Cons ( _ , Nil ) -> None
  | Cons ( _ , q ) -> Some(q);;

let rec length l =
  match l with
  | Nil -> 0
  | Cons( _ , q)  -> 1 + length q;;

let rec map f l  =
  match l with
  | Nil -> Nil
  | Cons ( x , q ) -> Cons ( f x , map f q );;


