
module ComplexNumbers

open System

type ComplexNumber = {real:double; imaginary:double}

let create (real:double) (imaginary:double):ComplexNumber = {real = real; imaginary = imaginary}

let mul ({real = a; imaginary = b}:ComplexNumber) ({real = c; imaginary = d}:ComplexNumber):ComplexNumber = {real = a * c - b * d; imaginary = b * c + a * d}

let add ({real = a; imaginary = b}:ComplexNumber) ({real = c; imaginary = d}:ComplexNumber):ComplexNumber = {real = a + c; imaginary = b + d}

let sub ({real = a; imaginary = b}:ComplexNumber) ({real = c; imaginary = d}:ComplexNumber):ComplexNumber = {real = a - c; imaginary = b - d}

let div ({real = a; imaginary = b}:ComplexNumber) ({real = c; imaginary = d}:ComplexNumber):ComplexNumber =
    let scale:double = pown c 2 + pown d 2
    {real = (a * c + b * d) / scale; imaginary = (b * c - a * d) / scale}

let abs ({real = a; imaginary = b}:ComplexNumber):double =
    pown a 2 + pown b 2
    |> Math.Sqrt

let conjugate ({real = a; imaginary = b}:ComplexNumber):ComplexNumber = {real = a; imaginary = -b}

let real (z:ComplexNumber):double = z.real

let imaginary (z:ComplexNumber):double = z.imaginary

let exp ({real = a; imaginary = b}:ComplexNumber):ComplexNumber = {real = Math.Exp a * Math.Cos b; imaginary = Math.Exp a * Math.Sin b}