module RobotSimulator

type Direction = North | East | South | West
type Position = int * int

type Robot = { direction: Direction; position: Position }

let create (direction: Direction) (position: Position): Robot = {direction = direction; position = position}

let turnLeft (robot: Robot): Robot =
    match robot.direction with
    | North -> {robot with direction = Direction.West}
    | East -> {robot with direction = Direction.North}
    | South -> {robot with direction = Direction.East}
    | West -> {robot with direction = Direction.South}

let turnRight (robot: Robot): Robot =
    match robot.direction with
    | North -> {robot with direction = Direction.East}
    | East -> {robot with direction = Direction.South}
    | South -> {robot with direction = Direction.West}
    | West -> {robot with direction = Direction.North}

let advance (robot: Robot): Robot =
    let (x, y) = robot.position
    match robot.direction with
    | North -> {robot with position = (x, y + 1)}
    | East -> {robot with position = (x + 1, y)}
    | South -> {robot with position = (x, y - 1)}
    | West -> {robot with position = (x - 1, y)}

let instructions (instructions': string) (robot: Robot) =
    let evaluate (currentRobot: Robot) (instruction: char) =
        match instruction with
        | 'A' -> advance currentRobot
        | 'L' -> turnLeft currentRobot
        | 'R' -> turnRight currentRobot
        | _ -> currentRobot
    instructions'.ToCharArray()
    |> Array.fold (fun (currentRobot: Robot) (instruction: char) -> evaluate (currentRobot: Robot) (instruction: char)) robot