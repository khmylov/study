// ------------------------------------------
// Some helpers - please ignore

type IO<'a> = IO of (unit -> 'a)

let execute (IO a : IO<'a>) : 'a =
    a ()

let putStr (s: string) : IO<unit> = 
    IO(fun () -> printf "%s" s)

[<AutoOpen>]
module IOMonad =

    let returnM (a : 'a) : IO<'a> =
        IO (fun () -> a)

    let private bind (ma : IO<'a>) (f : 'a -> IO<'b>) : IO<'b> =
        fun () ->
            execute ma |> f |> execute
        |> IO

    type IOBuilder() =
        member x.Bind(m, f)    = bind m f
        member x.Return(v)     = returnM v
        member x.ReturnFrom(v) = v
        member x.Delay(f)      = f ()

    let io = IOBuilder()    

// ------------------------------------------


type Action =
    | Atom of IO<Action>
    | Fork of Action * Action
    | Stop

type Concurrent<'a> = Concurrent of (('a -> Action) -> Action)

let continueWith c (Concurrent f) = f c

// ==============================================
// Ex. 0
// ==============================================

let action (x : Concurrent<'a>) : Action =
    match x with
    | Concurrent f -> f (fun _ -> Stop)

let action' (f: ('a -> Action) -> Action) : Action = 
    f(fun _ -> Stop)

// ==============================================
// Ex. 1
// ==============================================

let stop() : Concurrent<'a> =
    Concurrent(fun _ -> Stop)

// ==============================================
// Ex. 2
// ==============================================

let atom (x : IO<'a>) : Concurrent<'a> =
    Concurrent <| fun (c: ('a -> Action)) -> 
        Atom <| io {
            let! x' = x
            return c x'
        }

// ==============================================
// Ex. 3
// ==============================================

let fork (x : Concurrent<'a>) : Concurrent<unit> =
    Concurrent <| fun c -> Fork(action x, c())

let par (a : Concurrent<'a>) (b : Concurrent<'a>) : Concurrent<'a> =
    match a, b with
    | Concurrent a, Concurrent b -> 
        Concurrent <| fun c -> Fork(a c, b c)

// ==============================================
// Ex. 4 - implement bind
// ==============================================

[<AutoOpen>]
module ConcurrentMonad =

    let private returnM (a : 'a) : Concurrent<'a> =
        Concurrent (fun c -> c a)

    let private bind (ma : Concurrent<'a>) (f : 'a -> Concurrent<'b>) : Concurrent<'b> =
        match ma with
        | Concurrent ma -> 
            Concurrent (fun c -> ma(fun t -> 
                action <| f t
            ))

    type ConcurrentBuilder() =
        member x.Bind(m, f)    = bind m f
        member x.Return(v)     = returnM v
        member x.ReturnFrom(v) = v
        member x.Delay(f)      = f ()

    let concurrent = ConcurrentBuilder()    

// ==============================================
// Ex. 6
// ==============================================

let rec roundRobin (actions : Action list) : IO<unit> =
    failwith "You have to implement roundRobin"

// ==============================================
// Tests
// ==============================================

[<AutoOpen>]
module Tests =

    let run (x : Concurrent<'a>) =
        roundRobin [action x]
        |> execute

    let rec mapM_ (f : 'a -> Concurrent<'b>) (xs : 'a list) : Concurrent<unit> =
        match xs with
        | []      -> concurrent { return () }
        | (x::xs) -> 
            concurrent {
                let! _ = f x
                return! mapM_ f xs
            }

    let genRandom s =
        match s with
        | 1337 -> [1; 96; 36; 11; 42; 47; 9; 1; 62; 73]
        | 7331 -> [17; 73; 92; 36; 22; 72; 19; 35; 6; 74]
        | 2600 -> [83; 98; 35; 84; 44; 61; 54; 35; 83; 9]
        | 42   -> [71; 71; 17; 14; 16; 91; 18; 71; 58; 75]
        | _    -> failwith "uho - I am no Haskell StdGen sorry"

    let loop (xs : int list) : Concurrent<unit> =
        mapM_ (fun x -> atom (IO (fun () -> string x |> printf "%s"))) xs

    let ex0 =
        let first = loop (genRandom 1337)
        let second = 
            concurrent { 
                do! loop (genRandom 2600)
                do! atom (IO (fun () -> printfn "")) 
            }
        par first second
        
    let ex1 =
        concurrent {
            let! _ = atom (IO (fun () -> printf "Haskell"))
            do! fork (loop (genRandom 7331))
            do! loop (genRandom 42)
            do! atom (IO (fun () -> printf ""))
        }