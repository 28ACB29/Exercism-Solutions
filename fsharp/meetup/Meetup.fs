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
        match Math.Sign(n) with
        | -1 -> (m - ((-n) % m))
        | 0 -> 0
        | 1 -> n % m

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
        | Last -> firstDayOfMonth.AddDays((double) ((DateTime.DaysInMonth(year, month) - firstDayinMonth) - modulo (DateTime.DaysInMonth(year, month) - firstDayinMonth) 7))
        | Teenth -> findTeenth year month dayOfWeek