open System
open System.Collections.Generic
open System.IO
open System.Linq

type Point = {
    x : float
    y : float
    Class : string
}

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

let writeToFile pointList =
    printfn "Сохранить точки в файл? (y/n)"
    match Console.ReadLine().ToLower().Trim() with
    | "y" ->
        let writeData = pointList
                        |> List.map (fun p -> $"{p.x}|{p.y}|{p.Class}")
        File.WriteAllLines("savedPoints", writeData)
        pointList
    | _ -> pointList

let parcePoint (str: string[]) =
    printfn $"{str[0]} {str[1]} {str[2]}" 
    try
        let sX = Double.Parse str[0]
        let sY = Double.Parse str[1]
        let Class = str[2]
        let point = {x = sX; y = sY; Class = Class}
        point
    with
    | ex ->
        failwith $"Ошибка при извлечении данных из файла {ex.Message}"


let readFromFile =
    match File.Exists("savedPoints") with
    | true ->
        printfn "Обнаружены сохраненные точки, загрузить их? (y/n)"
        match Console.ReadLine().ToLower().Trim() with
        | "y" | "yes" ->
            let data =  File.ReadAllLines("savedPoints")
                        |> Array.toList
                        |> List.filter (fun s -> not (isNull s))
                        |> List.map (fun s -> s.Split("|"))
                        |> List.map (fun sa -> parcePoint sa)
            data
        | _ ->
            list.Empty
    | false ->
        list.Empty

let rec inputPoints pList =
    printfn "Введите данные точки"
    let nX = floatInput "X: "
    let nY = floatInput "Y: "
    printf "Класс: "
    let nClass = Console.ReadLine()
    let point = {x = nX; y = nY; Class = nClass}
    let nPList = point :: pList
    if List.length nPList > 2 then
        printfn "Хотите продолжить? (y/n)"
        match Console.ReadLine().Trim() with
        | "y" | "Y" | "yes" ->
            inputPoints nPList
        | _ ->
            writeToFile nPList
    else inputPoints nPList

let inputPoinstDialog pList : list<Point> =
    if List.length pList > 2 then
        printfn "Хотите добавить новые точки (y/n)"
        match Console.ReadLine().Trim().ToLower() with
        | "y" | "yes" ->
            inputPoints pList
        | _ ->  pList
    else
        inputPoints pList
let calcDistance Point1 Point2 =
    sqrt((Point1.x - Point2.x)**2 + (Point1.y - Point2.y)**2)
        
let calcMap point k pointList =
    pointList
    |> List.map (fun p -> (p, calcDistance point p))
    |> List.sortBy snd
    |> List.truncate k
        
let countClass className wPointList =
    wPointList
    |> List.filter (fun (point, _) -> point.Class = className)
    |> List.length
    
let getMostClass wPointList =
    let maxVal = wPointList
                |> List.maxBy (fun (p, _) -> countClass p.Class wPointList)
                |> (fun (p, _) -> countClass p.Class wPointList)
                
    let answ = wPointList
               |> List.filter (fun (p, _) -> countClass p.Class wPointList = maxVal)
               |> List.sortBy (fun (p, dist) -> dist)
               |> List.head
               |> (fun (p, _) -> p.Class)
    
    answ


[<EntryPoint>]
let main argv=
    let savedPoints = readFromFile
    let PointList = inputPoinstDialog savedPoints
    let k = intInput "Введите кол-во точек для определения типа:"
    let uX = floatInput "X:"
    let uY = floatInput "Y:"
    let answ = calcMap {x = uX; y = uY; Class = ""} k PointList
                |> getMostClass
    printfn "Предсказанный класс точки: %s" answ
    0;