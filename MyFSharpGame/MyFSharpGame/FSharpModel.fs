module FSharpModel

let width, height = 15,20
let canvas = (width / 2 - 1, 0)
let rowScore = 50
let levelScore = 500

let commandDuration = 200.
let lineDuration = 1000.
let blockDuration = 1000.
let blockTime = 100.
let levelChangingDuration = 100.

let randomClass = new System.Random ()

type GameProject = {
        score: int
        level: int
        isGameEnd: bool

        recentEventTime: float
        recentBlockTime: float
        recentRowTime: float
        rowscancel: (Colour * int * int) list option
        cubes: (Colour * int * int) list
        pos: int * int
        cubeStyle: (Colour * Style list list) option
        nextCubeStyle: Colour * Style list list
        clickEvent: ObjectType list
}
and Colour = | Purple | Coral | Brown | SpringGreen | Olive | BlueViolet | Lavender
and Style =  | A | B
and ArrowEvent = | LeftArrow | RightArrow | UpArrow
and ObjectType = | Move | Rotate | Block | Drop | Line | NextLevel | EndGame

let cubeShapes = [
     Purple, [
     [A;A]
     [A;A]
     ]
     Coral, [
     [A;A;A;A]
     ]
     Brown, [
     [A;A;B]
     [B;A;A]
     ]
     SpringGreen, [
     [B;A;A]
     [A;B;B]
     ]
     Olive,[
     [A;A;A]
     [A;B;B]
     ]
     BlueViolet, [
     [A;A;A]
     [B;B;A]
     ]
     Lavender,[
     [A;A;A]
     [B;A;B]
     ]
]
let randomCubes () = cubeShapes.[randomClass.Next(cubeShapes.Length)]
let loadModel = {
     score = 0
     level = 0
     isGameEnd = false
     recentEventTime = 0.
     recentBlockTime = 0.
     recentRowTime = 0.
     rowscancel = None
     cubes = []
     pos = canvas
     cubeStyle = randomCubes () |> Some
     nextCubeStyle =  randomCubes ()
     clickEvent = []
}
let rec spin = function
    | (_::_)::_ as m ->
        (List.map List.head m |> List.rev)::(List.map List.tail m |> spin)
        | _ -> []
let outline (ox, oy) = 
    List.mapi (fun y ->
        List.mapi (fun x -> function
        | A -> (x + ox, y + oy) |> Some
        | B -> None) >> List.choose id) >> List.concat
let overLoadedBlocks blocks =
    blocks |> List.exists (fun (x, y) -> x < 0 || x >= width || y < 0 || y >= height)

let overLappingBlocks blocks gameProject = 
    let gameEnvBlocks = gameProject.cubes |> List.map (fun (_,x,y) -> x,y)
    List.except gameEnvBlocks blocks <> blocks

let execute elapsed command gameEnv = 
    match command with
    | None -> gameEnv 
    | Some c ->
        match gameEnv.cubeStyle with
        | None -> gameEnv
        | Some _ when elapsed - gameEnv.recentEventTime < commandDuration -> gameEnv
        | Some (colour, blocks) ->
            let (x, y) = gameEnv.pos
            let (gx, gy) = 
                match c with 
                | LeftArrow -> (x - 1, y)
                | RightArrow -> (x + 1, y)
                | UpArrow -> (x, y)
            let newStyle = blocks |> match c with | UpArrow ->  spin | _ -> id
            let newCubes = outline (gx, gy) newStyle
            if overLoadedBlocks newCubes || overLappingBlocks newCubes gameEnv then
                { gameEnv with  clickEvent = Block::gameEnv.clickEvent }
            else
                let click =
                    match c with 
                    | UpArrow -> Rotate
                    | LeftArrow | RightArrow -> Move
                { gameEnv with
                    cubeStyle = Some (colour, newStyle)
                    pos = (gx, gy)
                    clickEvent = click::gameEnv.clickEvent
                    recentEventTime = elapsed }

let down elapsed isDownKeyPressed gameEnv = 
    match gameEnv.cubeStyle with
    | None -> gameEnv 
    | Some _ when
        let blockDuration = 
            if isDownKeyPressed then blockTime
            else blockDuration - (float gameEnv.level * levelChangingDuration) |> max blockTime
        elapsed - gameEnv.recentBlockTime < blockDuration -> gameEnv
    | Some (colour, blocks) ->
        let (x,y) = gameEnv.pos
        let NewCubePos = (x, y+1)

        let newCubes = outline NewCubePos blocks
        if not (overLoadedBlocks newCubes) && not (overLappingBlocks newCubes gameEnv) then
            { gameEnv with pos = NewCubePos; recentBlockTime = elapsed; clickEvent = Drop::gameEnv.clickEvent}
        else 
            let comingBlocks = outline gameEnv.pos blocks |> List.map (fun (x,y) -> colour, x, y)
            { gameEnv with cubes = gameEnv.cubes @ comingBlocks; cubeStyle = None}

let nextCubeStyle gameEnv = 
    match gameEnv.cubeStyle with 
    | Some _ -> gameEnv
    | None -> 
        let nextCubes = snd gameEnv.nextCubeStyle |> outline canvas
        let isGameEnd = overLappingBlocks nextCubes gameEnv
        { gameEnv with
            clickEvent = if isGameEnd then [EndGame] else gameEnv.clickEvent
            isGameEnd = isGameEnd
            pos = canvas
            cubeStyle = Some gameEnv.nextCubeStyle
            nextCubeStyle = randomCubes ()}

let getRows gameEnv =
    gameEnv.cubes
          |> List.groupBy (fun (_,_,y) -> y)
          |> List.filter (fun f -> List.length (snd f) = width)
          |> List.collect snd
let collapseRows elapsed gameEnv = 
    match gameEnv.rowscancel with
    | None -> gameEnv
    | Some rows ->
        let newScore = List.length rows/width * rowScore |> (+) gameEnv.score
        let newLevel = float newScore / float levelScore |> floor |> int
        let cubeLine= rows |> List.map (fun (_,_,y) -> y) |> List.distinct
        let newCubes = 
            List.except rows gameEnv.cubes
            |> List.map (fun (c, x, y) -> 
                let change = y::cubeLine |> List.sortByDescending id |> List.findIndex ((=)y)
                c,x, (y + change))
        { gameEnv with 
             cubes = newCubes
             score = newScore
             level = newLevel
             clickEvent = if newLevel <> gameEnv.level then NextLevel::gameEnv.clickEvent else gameEnv.clickEvent 
             recentBlockTime = elapsed
             recentEventTime = elapsed
             rowscancel = None }

let graphicGame elapsed command isDownPressed gameEnv = 
    if elapsed - gameEnv.recentRowTime < lineDuration then
        { gameEnv with clickEvent = [] }
    else 
        let output = 
            { gameEnv with clickEvent = [] }
            |> collapseRows elapsed
            |> nextCubeStyle
            |> execute elapsed command
            |> down elapsed isDownPressed
        let rows = getRows output
        if List.isEmpty rows then output else
            { output with 
                clickEvent = Line::gameEnv.clickEvent
                recentRowTime = elapsed
                rowscancel = Some rows}