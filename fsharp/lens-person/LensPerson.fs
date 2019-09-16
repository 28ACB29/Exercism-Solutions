module LensPerson

open System

// TODO: implement this module

type Name = { name:string; surName:string}

type Location = { street:string; houseNumber:int; place:string; country:string}

type Born = {at:Location; on:DateTime}

type Person = { name:Name; born:Born; address:Location}

let bornAtStreet (person:Person):string = person.born.at.street

let currentStreet (person:Person):string = person.address.street

let birthMonth (person:Person):int = person.born.on.Month

let bornOn (person:Person):DateTime = person.born.on