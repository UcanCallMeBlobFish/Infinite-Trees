type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree) ;;
type 'a tree = 
  |Empty
  |Node of 'a * 'a tree * 'a tree
;;

let rec layer_tree r = 
  let rec acc root n = LNode(root+n, (fun() -> acc (root)(n+1)),(fun()-> acc (root)(n+1))  )
  in acc r 0 ;;

let rec interval_tree l h = 
  LNode( 
    (l,h),
    (fun () -> interval_tree l ( (l +. h) /.2.0) ) ,  
    (fun () -> interval_tree ((l+.h) /.2.0)  h) 
  );;

let rec rational_tree n d = 
  
  LNode( (n,d),
         ( fun () -> rational_tree n (d+1) ) 
       
       , (fun () -> rational_tree (n+1) d )
       );; 



let rec map f t =
  let  LNode(v,left,right) = t in 
  LNode(f v, (fun () -> map f (left())), (fun () -> map f (right()) ) );;  


let rec top n t = 
  let  LNode(v,left,right) = t in 
  if n = 0 then Empty else 
    Node(v,top (n-1) (left()), top (n-1) (right()))
;;
  
let  enqueue side list = list@[side];;

let dequeue list = match list with 
    [] -> [] |
    h::t -> t
;;

                                 
                                 
                            
let rec find p tree =
  
  let rec acc treelist p stack = 
    
    match List.hd treelist with 
    
      LNode(v,left,right) as k -> if p v then k else
          
          let l = enqueue (right())  treelist in

          let l = enqueue (left()) l in 
          
          acc (dequeue l) p (l::stack)
          
  
  in acc [tree] p [];;



