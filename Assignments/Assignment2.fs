namespace Assignments
open System
open System.Linq
open System.Collections.Generic
open Microsoft.Xna.Framework

module internal Helper2 =  

    let getAxis d (v:Vector2) = if d % 2 = 0 then v.X else v.Y
    let getInvAxis d (v:Vector2) = getAxis (d+1) v

    [<AllowNullLiteral>] // A node is null if and only if the parent has zero or one branches
    type Node = 
        val Value:Vector2
        val Left:Node
        val Right:Node
        val Depth: int
        
        new(v,l,r,d) = {Value = v; Left = l; Right=r; Depth=d}

        static member Construct (pList:list<Vector2>) : Node =
            let takeMedian alist = 
                let l,m = List.splitAt (List.length alist / 2 ) alist
                l, List.head m, List.tail m

            let rec add pList depth = 
                if List.isEmpty pList then null
                else
                    let l,h,r = pList
                                |> List.sortBy ( fun (e:Vector2) -> getAxis depth e ) 
                                |> takeMedian
                    new Node(h, add l (depth+1) , add r (depth+1), depth)
            add pList 0
                        
        ///Corecursive divide and conquer approach on post-order kdtree traversal
        member Node.RangeSearch distance target : list<Vector2> = 
            let rec search (node:Node) (acc:list<Vector2>) depth  : list<Vector2> =
                if isNull node then acc
                else
                    let l = (search node.Left List.empty<Vector2> (depth+1) )
                    let r = (search node.Right List.empty<Vector2> (depth+1) )
                    //Textbook: doesn't have any additional value as the course guide asks to check for euclidian distance
                    //match Math.Abs(getAxis depth target - getAxis depth node.Value) <= distance, Math.Abs(getInvAxis (depth) target - getInvAxis (depth) node.Value) <= distance with
                    //| true, true -> ( node.Value::(l @ r) )
                    //| _ -> (l @ r)
                    if Vector2.Distance(target, node.Value) <= distance then
                        ( node.Value::(l @ r) )
                    else
                        (l @ r)
            
            search Node [] 0

module public Assignment2 = 
    let FindSpecialBuildingsWithinDistanceFromHouse (specialBuildings:IEnumerable<Vector2>) (housesAndDistances:IEnumerable<Tuple<Vector2, Single>>) : IEnumerable<IEnumerable<Vector2>> = 
        let tree = Helper2.Node.Construct ( List.ofSeq specialBuildings )
        let result = List.map ( fun (tpl:Tuple<Vector2,Single>) -> tree.RangeSearch tpl.Item2 tpl.Item1 ) ( List.ofSeq housesAndDistances )
        Enumerable.AsEnumerable ( List.map Enumerable.AsEnumerable result )