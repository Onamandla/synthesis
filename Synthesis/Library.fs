module Synthesis

let abelar a = a>12 && a<3097 && a%12=0

let area b h =
    match b<0.0 || h<0.0  with
    |true -> failwith "An area can not be negative"
    |false -> b*h*0.5

let zollo a =
    match a>0 with
    |true -> a*2
    |false -> a*(-1)

let min a b  =
    match a>b with
    |true -> b
    |false -> a

let max a b = 
    match a>b with
    |true -> a
    |false -> b

let ofTime h m s = h*3600+m*60+s

let toTime time = 
    match time>=0 with
    |false -> 0,0,0
    |true -> let hour=time/3600
             let minutes= (time - hour*3600)/60
             let sec = (time- (hour*3600) - minutes*60)
             hour,minutes,sec

let rec digits value =
    let rec num k account =
        match k=0 with
        |true -> account
        |false -> num(k/10) (1+account)
    match value <>0 with
    |false -> 1
    |true -> num value 0
    //failwith "Not implemented"

let minmax (a, b, c, d) = min a b |> min c, min d, max a b |> max c |> max d
 //   failwith "Not implemented"

let isLeap days =
    match days >= 1582 with
    |false -> failwith "The value is less"
    |true -> 
        match (days%4 = 0 || days%400 = 0) && days%100 = 1 with
        |true -> true
        |false -> false
   //failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"