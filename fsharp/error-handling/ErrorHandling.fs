module ErrorHandling

    open System

    let handleErrorByThrowingException() = failwith "handle error by throwing exception"

    let handleErrorByReturningOption (input:string):int option =
        try
            input
            |> Convert.ToInt32
            |> Some
        with
            | (e:Exception) -> None

    let handleErrorByReturningResult (input:string):Result<int,string> =
        try
            input
            |> Convert.ToInt32
            |> Ok
        with
            | (e:Exception) ->
                "Could not convert input to integer"
                |> Error

    let bind (switchFunction:int -> Result<int,string>) (twoTrackInput:Result<int,string>):Result<int,string> =
        match twoTrackInput with
        | Ok(value:int) ->
            value
            |> switchFunction
        | Error(_) -> twoTrackInput

    let cleanupDisposablesWhenThrowingException (resource:IDisposable) =
        try
            failwith "no operation"
        finally
            resource.Dispose()