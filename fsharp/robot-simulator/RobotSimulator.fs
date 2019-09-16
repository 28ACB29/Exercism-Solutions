module RobotSimulator

    type Direction =
    | North
    | East
    | South
    | West

    type Position = int * int

    type Robot = { direction: Direction; position: Position }

    let private leftTurn (direction: Direction): Direction =
        match direction with
        | North -> West
        | East -> North
        | South -> East
        | West -> South

    let private rightTurn (direction: Direction): Direction =
        match direction with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

    let private move {direction = orientation; position = (x: int, y: int)}: Position =
        match orientation with
        | North -> (x, y + 1)
        | East -> (x + 1, y)
        | South -> (x, y - 1)
        | West -> (x - 1, y)

    let create (direction: Direction) (position: Position): Robot = {direction = direction; position = position}

    let turnLeft (robot: Robot): Robot = {robot with direction = leftTurn robot.direction}

    let turnRight (robot: Robot): Robot = {robot with direction = rightTurn robot.direction}

    let advance (robot: Robot): Robot = {robot with position = move robot}

    let private evaluate (currentRobot: Robot) (instruction: char) =
        match instruction with
        | 'A' -> advance currentRobot
        | 'L' -> turnLeft currentRobot
        | 'R' -> turnRight currentRobot
        | _ -> currentRobot

    let instructions (instructions': string) (robot: Robot) =
        instructions'.ToCharArray()
        |> Array.fold evaluate robot