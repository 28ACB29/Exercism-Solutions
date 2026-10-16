module Meetup

    open System

    type Week =
        | First
        | Second
        | Third
        | Fourth
        | Last
        | Teenth

    let modulo (n: int) (m: int) =
        // canonical non-negative modulo
        ((n % m) + m) % m

    let findTeenth (year:int) (month:int) (dayOfWeek:DayOfWeek): DateTime =
        seq{13..19}
        |> Seq.map (fun (day:int) -> DateTime(year, month, day))
        |> Seq.filter (fun (date:DateTime) -> date.DayOfWeek = dayOfWeek)
        |> Seq.exactlyOne

    let meetup (year:int) (month:int) (week:Week) (dayOfWeek:DayOfWeek): DateTime =
        let firstDayOfMonth:DateTime = DateTime(year, month, 1)
        let firstDayinMonth:int = modulo ((int) dayOfWeek - (int) firstDayOfMonth.DayOfWeek) 7
        match week with
        | First ->
            firstDayinMonth
            |> double
            |> firstDayOfMonth.AddDays
        | Second ->
            firstDayinMonth
            |> (+) 7
            |> double
            |> firstDayOfMonth.AddDays
        | Third ->
            firstDayinMonth
            |> (+) 14
            |> double
            |> firstDayOfMonth.AddDays
        | Fourth ->
            firstDayinMonth
            |> (+) 21
            |> double
            |> firstDayOfMonth.AddDays
        | Last ->
            // start from last day of month and step back to the requested weekday
            let lastDayOfMonth:DateTime = DateTime(year, month, DateTime.DaysInMonth(year, month))
            let backOffset:int = modulo ((int) lastDayOfMonth.DayOfWeek - (int) dayOfWeek) 7
            lastDayOfMonth.AddDays(- (double) backOffset)
        | Teenth -> findTeenth year month dayOfWeek