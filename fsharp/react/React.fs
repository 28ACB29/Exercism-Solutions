module React

// TODO: implement this module
type InputCell(Value:int) =
    let mutable (Value)
type ComputeCell(inputs:InputCell list, computation:InputCell list -> int) =
    let mutable (Changed:Handler<int>)
type Reactor() =
    member this.createInputCell (input:int) = InputCell(input)
    member this.createComputeCell (inputs:InputCell list) (computation:InputCell list -> int) = ComputeCell(inputs, computation)
end