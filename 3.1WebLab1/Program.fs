open System
open System.Collections.Generic
open System.Linq

type Point = {
    x : float
    y : float
    Class : string
}

let map = Set.empty<Point>

let x = Convert.ToDouble(Console.ReadLine())

let rec floatInput label =
    printf "%s" label 
    let input = Console.ReadLine()
    
    match Double.TryParse(input) with
    | true, value -> 
        value 
    | false, _ -> 
        printfn "Неверный ввод, целое число или число с запятой"
        floatInput label 

let rec intInput label =
    printfn "%s" label
    let input = Console.ReadLine()
    
    match Int32.TryParse(input) with
    | true, value ->
        value
    | false, _ ->
        printfn "Неверный вывод, принимаются только целые числа"
        intInput label

let rec inputPoints pList =
    printfn "Введите данные точки"
    let nX = floatInput "X: "
    let nY = floatInput "Y: "
    let nClass = Console.ReadLine()
    let point = {x = nX; y = nY; Class = nClass}
    let nPList = point :: pList
    printfn "Хотите продолжить? (y/n)"
    match Console.ReadLine().Trim() with
    | "y" | "Y" | "yes" ->
        inputPoints nPList
    | _ ->
        nPList
        
let calcDistance Point1 Point2 =
    sqrt((Point1.x - Point2.x)**2 + (Point1.y - Point2.y))
        
let calcMap point k pointList =
    pointList
    |> List.map (fun p -> (p, calcDistance point p))
    |> List.sortBy snd
    |> List.truncate k
        
        



[<EntryPoint>]
let main argv=
    let PointList = inputPoints list.Empty
    let k = intInput "Введите кол-во точек для определения типа:"
    let uX = floatInput "X:"
    let uY = floatInput "Y:"
    let answ = calcMap {x = 0; y = 1; Class = "non"} k PointList 
    0;