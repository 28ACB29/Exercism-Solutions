module Clock

    open System

    type Clock = {totalMinutes:int}

    let private hour = 60

    let private day = 24 * hour

    let private rollOver (totalMinutes:int) =
        match totalMinutes > -1 with
        | true -> totalMinutes % day
        | false -> day - (-totalMinutes) % day
 
    let create (hours:int) (minutes:int):Clock = {totalMinutes = hour * hours + minutes |> rollOver}

    let add (minutes:int) (clock:Clock):Clock = {totalMinutes = clock.totalMinutes + minutes |> rollOver}

    let subtract (minutes:int) (clock:Clock):Clock = {totalMinutes = clock.totalMinutes - minutes |> rollOver}

    let display (clock:Clock):string = String.Format("{0}:{1}", clock.totalMinutes / hour, clock.totalMinutes % hour)