(* Explaining Your Solution

I am discussing the solution to color_options

1. What is the search space that your solution explores?
 Specifically, explain what decisions are made at each 
step in exploring this space. In the case of the 
subset-sum problem, we made a decision about including, 
or not including, an element from the original set in the
partial subset we were constructing. Explain the decisions 
being made in your solution, with references to those parts
 of your solution.

   My Answer:
I created a helper function that takes in a graph and
initially an empty coloring. Inside the helper, I 
determine what values the graph holds.If the graph has no
nodes, then the coloring passed in is returned. Otherwise,
for every node in the node list:
   1. an adjacency list is created for the node
   2. based on the adjacency list created, the safe colors 
   list is created. Safe colors are ones that won't violate
   the graph coloring rule of no adjacent nodes having the 
   same coloring.
   3. if there are no safe colors for the node, then helper 
   returns [] indicating that there are no solutions.
   Search is no longer continued.
   4. if there are safe colors, then take the head safe color 
   and add the (node, head safe color) to the coloring
   5. recursively call the helper, with the rest of the nodes 
   and the new coloring
   6. once done, either an empty list is returned indicating 
   that there is no coloring possible (returns None) or a 
   coloring is returned from the helper, which is then 
   returned as Some coloring by the color_option function.

There are two conditions when search terminates:

  1: All nodes in the graph have been examined and
  helper_option is called on graph with no more nodes

  2: No coloring exists for the graph i.e. two or more nodes
  have same coloring, violating graph coloring rules.


2.
In exploring the potential coloring of a graph, your solution
must not continue searching along that path if two adjacent 
nodes are colored with the same color. Explain how your 
solution avoids this potential inefficiency.

Note that we did not have this concern in the subset-sum problem. 
In choosing certain elements of the set, we could not tell if 
this was a dead end because the last element in the set could
result in getting a sum of 0. In this problem, that is not the 
case. If we color adjacent nodes with the same color early in 
the process then there is no point in continuing.

   My Answer:
 The helping function called ok_colors takes in list of "safe colors", 
and moves through all adjacent nodes determining the colors that are
associated to the node. If the safe colors list is reduced to [] then there 
are no possible colorings for a node. If that is the case, in the helper 
function, if the safe colors list is indeed [], then the coloring returns []
meaning there is no solution. This prevents further searching for an answer.

*)


(*
The code below is included in Hwk_07 and is not the work of Connor Hanlon
*)

type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list

let g1 = ( [N 1; N 2; N 3; N 4],[(N 1, N 2);(N 1, N 3);(N 2, N 3);(N 3, N 4)])
let g1_coloring = [ (N 1, C 1); (N 2, C 2); (N 3, C 3); (N 4, C 2) ]


  
(* The code below was written by Connor Hanlon*)
type adj = node list list


(*finds the color associated to the input node. If input
node does not have a color association, then none is returned.
Otherwise, Some color is returned*)
let rec lookup_color (n:node)(c:coloring): color option=
  match n, c with
  | N input, [] -> None
  | N input, (N to_check, C n_color)::rest ->
     if input = to_check then (Some (C n_color))
     else
       lookup_color n rest

(* Determines the adjacent nodes to input node*)
let rec make_adj (n:node)(el:edge list):node list =
  match n, el with
  | _, [] -> []
  |(N input), (N origin, N dest)::rest_edges ->
     if input = origin then (N dest)::(make_adj (N input) rest_edges)
     else if input = dest then (N origin)::(make_adj (N input) rest_edges)
     else make_adj (N input) rest_edges
       
let three_colors = [C 1; C 2; C 3]

(*Removes a color the ok color list*)
let rec rem_color (c:color)(clst:color list):color list=
  match c, clst with
  | _, [] -> []
  | (C to_check), (C color_hd)::rest_nodes ->
    if to_check = color_hd then (rem_color c rest_nodes)
    else (C color_hd)::(rem_color c rest_nodes)


(*Generates a list of safe colors to choose based on adjacent node colors*)
let rec ok_colors(adj_nodes:node list)(c:coloring)(clst:color list):color list=
  match adj_nodes with
  | []-> clst
  | (N adj_hd)::rest_nodes ->
     let lookd = lookup_color (N adj_hd) c in
     if lookd = None then ok_colors rest_nodes c clst
     else
       (match lookd with
       | None -> []
       | Some (C t) -> ok_colors rest_nodes c (rem_color (C t) clst)
       )
(*
Uses a helper_option function that takes graph and a coloring as input.
The graph is first checked to see if there are any nodes in it.
If not, the coloring is returned and search stops. Otherwise,
the first node in the graph's node list is examined.

For the first node in the graph's node list, and adjacency list
is created usingthe helper function make_adj, listing all nodes 
that it is connected to. Afterwards, a list of safe colors is
created using the ok_colors helper method. If there are
no safe colors, then search ends and no coloring exists. If
there are safe colors, the head of the safe colors list
is paired with the node head and added to the coloring,
and the helper_option method is called recursively.

There are two conditions when search terminates:

  1: All nodes in the graph have been examined and
  helper_option is called on graph with no more nodes

  2: No coloring exists for the graph i.e. two nodes
  have same coloring.

The helper_option function is then called with the input graph
and an initial empty coloring, which is then matched to either 
an empty coloring or some coloring, and the coloring option
is returned
*)
let color_option (g:graph): coloring option=
  let rec helper_option (gr:graph)(c:coloring): coloring=
    match gr with
    | ([], _) ->  c
    | ((N node_hd)::rest_nodes, edges)->
       let adjc = make_adj (N node_hd) edges in
       let safe_cols = ok_colors adjc c three_colors in
       match safe_cols with
       | [] -> []
       | (C color_hd)::_ ->
	  let new_coloring = (N node_hd, C color_hd)::c in
	  helper_option (rest_nodes, edges) (new_coloring)
  in
  match helper_option g [] with
  | [] -> None
  | ans -> Some (List.rev ans)
  
   
exception FoundColoring of coloring

(*See color_options function description, this method works on the
same principle. However, instead of the helper_exc function 
returning an empty list or a populated list, and exception is
raised. No matching is then required when helper_excn is called on 
the input graph with initial empty coloring*)        
let color_exception (g:graph): unit=
  let rec helper_exc (gr:graph)(c:coloring):unit=
    match gr with
    | ([], _) -> raise (FoundColoring (List.rev c))
    | ((N node_hd)::rest_nodes, edges) ->
       let adjc = make_adj (N node_hd) edges in
       let safe_cols = ok_colors adjc c three_colors in
       match safe_cols with
       | [] -> raise (FoundColoring [])
       | (C color_hd)::_->
	  let new_coloring = (N node_hd, C color_hd)::c in
	  helper_exc (rest_nodes, edges) (new_coloring)
  in helper_exc g []

  
(* Testing 
let ta = [[N 1;N 2;N 3]]
let te = (N 2, N 3)
let elist = [ (N 1, N 2); (N 1, N 3); (N 2, N 3); (N 3, N 4) ]

let test1_option = color_option g1
let test2_option = 
   color_option ( [N 1; N 2; N 3; N 4], [(N 1, N 2); (N 1, N 3); (N 1, N 4)])

let to_fail = ([N 1; N 2; N 3; N 4], 
[ (N 1, N 2); (N 1, N 3); (N 1, N 4); (N 3, N 4); (N 3, N 2); (N 2, N 4) ])
*)

  
  (*Below functions not used in answers to homework*)
  
(*
(*Creates an adjacency matrix, where the first element of each 
node list is the head of the edge, and the following
elements are the tail vertex of edge. If head node is not found amongst
ind. lists, then a new list is created with a new head and tail
vertex*)
let rec place (e:edge)(a:adj):adj =
  match e, a with
  | (N a, N b), [] -> [[N a;N b]]
  | (N a, N b), x::xs ->
     (match x with
     | [] -> [N a; N b] :: (xs@[])
     | (N y)::ys -> if a = y
       then (x @ [N b]) :: (xs@[])
       else
	x::(place e xs)
     )
      
(* Makes an adjacency list from a list of edges*)      
let rec make_adj (e:edge list)(a:adj):adj =
  match e with
  | [] -> a
  | x::xs -> make_adj xs (place x a)
 
*)

  let g2_color_exc=  ( [N 1;N 2;N 3;N 4], [(N 1,N 2); (N 1,N 3); (N 1,N 4); (N 2,N 3); (N 2,N 4); (N 3,N 4)] )
