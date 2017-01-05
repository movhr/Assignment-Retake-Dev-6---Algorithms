namespace Assignments

open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections
open Microsoft.Xna.Framework

// A set of unvisited nodes
// A set of visited nodes
// If a node has been checked, remove from unvisited and add to visited nodes
// The checked node contains a tentative distance to the destination node

// Things to keep in mind:
// - (edge) routes are double sided
// - dont forget to put back the minimum in closestNeighbor when another minimum has been found
// - filtering out visited neighbors 

module internal Helper3 = 
    [<AllowNullLiteral>]
    type Node = 
        val Location : Vector2
        val mutable Visited : bool
        val mutable Neighbors : list<Node>
        val mutable Weight : float32
        
        new(l) = 
            { 
                Location = l
                Visited = false
                Neighbors = List.empty<Node>
                Weight = infinityf 
            }
    
    type Edge = 
        val Source : Node
        val Dest : Node
        new(s, d) = 
            { 
                Source = new Node(s)
                Dest = new Node(d) 
            }
    
    let Dijkstra (start : Vector2) (dest : Vector2) (roadList : list<Edge>) = 
        let fillMatrix (l : list<Edge>) = 
            let rec add (m : Node [,]) (l : list<Edge>) = 
                match l with
                | [] -> m
                | v :: vs -> 
                    if isNull m.[int v.Source.Location.X, int v.Source.Location.Y] then 
                        m.[int v.Source.Location.X, int v.Source.Location.Y] <- new Node(v.Source.Location)
                    if isNull m.[int v.Dest.Location.X, int v.Dest.Location.Y] then 
                        m.[int v.Dest.Location.X, int v.Dest.Location.Y] <- new Node(v.Dest.Location)
                    m.[int v.Source.Location.X, int v.Source.Location.Y].Neighbors <- (m.[int v.Dest.Location.X, 
                                                                                          int v.Dest.Location.Y] 
                                                                                       :: m.[int v.Source.Location.X, 
                                                                                             int v.Source.Location.Y].Neighbors)
                    m.[int v.Dest.Location.X, int v.Dest.Location.Y].Neighbors <- (m.[int v.Source.Location.X, 
                                                                                      int v.Source.Location.Y] 
                                                                                   :: m.[int v.Dest.Location.X, 
                                                                                         int v.Dest.Location.Y].Neighbors)
                    add m vs
            
            let len = List.length l
            add (Array2D.create len len null) l
        
        let rec closestNeighbor (l : list<Node>) (min : Node) (rest : list<Node>) = 
            match l with
            | [] -> min, rest
            | x :: xs -> 
                if x.Weight < min.Weight then closestNeighbor xs x (min :: rest)
                else closestNeighbor xs min (x :: rest)
        
        let traceback (destNode : Node) = 
            let rec shortestPath (cur : Node) (prev : Node) (acc : list<Vector2 * Vector2>) = 
                let closest, rest = closestNeighbor (List.tail cur.Neighbors) (List.head cur.Neighbors) []
                if closest.Weight >= cur.Weight then acc
                else shortestPath closest cur ((prev.Location, cur.Location) :: acc)
            shortestPath destNode destNode []
        
        let rec setNeighborWeight (c : Node) (l : list<Node>) (acc : list<Node>) : list<Node> = 
            match l with
            | [] -> acc
            | x :: xs -> 
                x.Weight <- Math.Min(Vector2.Distance(x.Location, c.Location) + c.Weight, x.Weight)
                setNeighborWeight c xs (x :: acc)
        
        let rec trace (m : Node [,]) (q : list<Node>) = 
            match q with
            | [] -> ()
            | _ ->
                let (cur, rest) = closestNeighbor (List.tail q) (List.head q) [] // Take node with smallest distance
                if not cur.Visited then 
                    cur.Visited <- true
                    setNeighborWeight cur cur.Neighbors [] // Determine tentative distances
                    |> List.filter (fun (node : Node) -> not node.Visited)
                    |> List.append rest
                    |> trace m
                else trace m rest
        
        let m = fillMatrix roadList
        m.[int start.X, int start.Y].Weight <- float32 0
        trace m [ m.[int start.X, int start.Y] ] 
        traceback m.[int dest.X, int dest.Y]

module public Assignment3 = 
    let FindRoute (start : Vector2) (dest : Vector2) (roads : IEnumerable<Tuple<Vector2, Vector2>>) : IEnumerable<Vector2 * Vector2> = 
        List.ofSeq roads
        |> List.map (fun (tpl : Tuple<Vector2, Vector2>) -> new Helper3.Edge(tpl.Item1, tpl.Item2))
        |> Helper3.Dijkstra start dest
        |> Enumerable.AsEnumerable
