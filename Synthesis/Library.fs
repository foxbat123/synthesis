module Synthesis

let abelar x =
    (x > 12) && (x < 3097) && (x % 12 = 0)

let area b h =
    match (b < 0.0) || (h < 0.0) with
        | true -> failwith "Negative Number!!!"
        | false -> 0.5 * (b * h)
    
let zollo x =
    match (x < 0) with
        | true -> x * -1
        | false -> x * 2

let min x y =
    match (x < y) with
        | true -> x
        | false -> y

let max x y =
    match (x > y) with
        | true -> x
        | false -> y

let ofTime h m s =
    ((h * 60) * 60) + (m * 60) + s

let toTime n =
    match (n<0) with
    | true -> (0,0,0)
    | _ -> n / 3600, n % 3600 / 60, (n % 3600) % 60
    
let digits n =
    let rec countD n acc =
        match n=0 with 
        | true -> acc
        | _ -> countD (n/10) (1+acc)
    match n = 0 with
    | true -> 1
    | _ -> countD n 0
    

let minmax (a,b,c,d) =
    min (min a b) (min c d), max (max a b) (max c d)
    


let isLeap y =
    match (y<1582) with 
    |true -> failwith "date too early"
    |false -> match ((y%4=0) && (y%100<>0)) || ((y%4=0) && (y%100=0) && (y%400=0)) with
                |true -> true
                |false -> false
    



let month m =
    match m with 
    | 1 -> ("January", 31)
    | 2 -> ("February", 28)
    | 3 -> ("March", 31)
    | 4 -> ("April", 30)
    | 5 -> ("May", 31)
    | 6 -> ("June", 30)
    | 7 -> ("July", 31)
    | 8 -> ("August", 31)
    | 9 -> ("September", 30)
    | 10 -> ("October", 31)
    | 11 -> ("November", 30)
    | 12 -> ("December", 31)
    |_ -> failwith "Not valid month"



let toBinary posint =
    let rec conV n eString  =
        match n=0 with 
        | true -> eString
        | _ -> match n%2 = 0 with
                        | true -> conV (n/2) ("0" + eString)
                        | _ -> conV (n/2) ("1" + eString)
    match posint < 0 with 
    | true -> failwith "Negative!!!"
    | false -> match posint = 0 with
                | true -> "0"
                | _ -> conV posint ""

let bizFuzz num =
    let rec count n (three,five,both) =
        match n > num with 
        | true -> (three, five, both)
        | _ -> match n%3=0 && n%5=0 with
                | true -> count (n+1) (three + 1, five + 1, both+1)
                | _ -> match n%3=0 with  
                        |true -> count (n+1) (three + 1, five, both)
                        |_ -> match n%5=0 with 
                                |true -> count (n+1) (three, five + 1, both)
                                |false -> count (n+1) (three, five, both)
    count 1 (0,0,0) 

          

let monthDay d y =
    match (y<1582) with 
    |true -> failwith "date too early"
    |_ ->
    match ((d > 0) && (d < 32)) with 
    | true -> "January"
    | _ -> match ((d > 31) && (d < 60) && (isLeap y = false)) || ((d > 31) && (d < 61) && (isLeap y = true)) with 
            |true -> "February"
            |_ -> match ((d > 59) && (d < 91) && (isLeap y = false)) || ((d > 60) && (d < 92) && (isLeap y = true)) with 
                    |true -> "March"
                    |false -> match ((d > 90) && (d < 121) && (isLeap y = false)) || ((d > 91) && (d < 122) && (isLeap y = true)) with 
                                |true -> "April"
                                |false -> match ((d > 120) && (d < 152) && (isLeap y = false)) || ((d > 121) && (d < 153) && (isLeap y = true)) with 
                                            |true -> "May"
                                            |false -> match ((d > 151) && (d < 182) && (isLeap y = false)) || ((d > 152) && (d < 183) && (isLeap y = true)) with
                                                        | true -> "June"
                                                        | _ -> match ((d > 181) && (d < 213) && (isLeap y = false)) || ((d > 182) && (d < 214) && (isLeap y = true)) with 
                                                                |true -> "July"
                                                                |_ -> match ((d > 212) && (d < 244) && (isLeap y = false)) || ((d > 213) && (d < 245) && (isLeap y = true)) with 
                                                                        |true -> "August"
                                                                        |false -> match ((d > 243) && (d < 274) && (isLeap y = false)) || ((d > 244) && (d < 275) && (isLeap y = true)) with 
                                                                                    |true -> "September"
                                                                                    |false -> match ((d > 273) && (d < 305) && (isLeap y = false)) || ((d > 274) && (d < 306) && (isLeap y = true)) with 
                                                                                                |true -> "October"
                                                                                                |false -> match ((d > 304) && (d < 335) && (isLeap y = false)) || ((d > 305) && (d < 336) && (isLeap y = true)) with
                                                                                                                        |true -> "November"
                                                                                                                        |false -> match ((d > 334) && (d < 366) && (isLeap y = false)) || ((d > 335) && (d < 367) && (isLeap y = true)) with 
                                                                                                                                    |true -> "December"
                                                                                                                                    |_ -> failwith "oops"

    
  

    
  

let coord _ =
    failwith "Not implemented"