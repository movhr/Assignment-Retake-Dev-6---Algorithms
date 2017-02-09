namespace Assignments
open System
open System.Linq
open System.Collections.Generic
open Microsoft.Xna.Framework

module internal Helper1 =
    let rec merge t a b r = 
        match a, b with
        | [], _ -> List.rev ( (List.rev b) @ r )
        | _, [] -> List.rev ( (List.rev a) @ r )
        | x::xs, y::ys -> 
            if Vector2.Distance(x,t) <= Vector2.Distance(y,t) 
            then ( merge t xs b (x::r) ) 
            else ( merge t a ys (y::r) ) //goes from biggest to smallest
        
    let rec split target veclist = 
        match List.length veclist with
        | 0 | 1 -> veclist
        | _ ->
            let l,r = List.splitAt ( List.length veclist / 2) veclist
            let x = split target l
            let y = split target r
            merge target x y List.empty<Vector2>
    
module public Assignment1 =
    let SortSpecialBuildingsByDistance (house:Vector2) (specialBuildings:IEnumerable<Vector2>) = 
        Enumerable.AsEnumerable ( Helper1.split house (List.ofSeq specialBuildings) )