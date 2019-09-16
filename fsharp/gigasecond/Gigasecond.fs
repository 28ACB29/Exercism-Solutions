module Gigasecond

    open System

    let private giga:double = 1000000000.0

    let add (beginDate:DateTime):DateTime = beginDate.AddSeconds(giga)