module FSharpView

open FSharpGame
open FSharpModel
open Microsoft.Xna.Framework

let w, h = 450, 600
let pixel = Window (w, h)
let loadCubes = [
    Dimention { key = "blank"; path = "Content/color"}
    Dimention { key = "cube"; path = "Content/cube"}
    TextFont { key = "default"; path = "Content/coders_crux"}
]

let blockwidth, blockheight = 23,23
let game_x, game_y, game_w, game_h = 10, 10, 350, 500
let next_x, next_y, next_w, next_h = 370, 10, 120, 70
let canvasOverSized = 20, 200, 360, 100
let textMargin = 0.5
let textHeight = 20
let score_x, score_y = next_x + (next_w / 2), next_y + next_h + 20
let level_x, level_y = score_x, score_y + 60
let int_x, int_y = w/2, game_y + game_h + 30
let gameOver_x, gameOver_y = 200, 230

let mapColor colour = 
    match colour with
    | Purple -> Color.Purple | Coral -> Color.Coral | Brown -> Color.Brown
    | SpringGreen -> Color.SpringGreen | Olive -> Color.Olive | BlueViolet -> Color.BlueViolet | Lavender -> Color.Lavender

let posNext (x,y)(px, py) = 
    x * blockwidth + px, y * blockwidth + py, blockwidth, blockheight

let designOfUI _ (fSharpModel: GameProject) = 
    let gameArea = [
        ImageColour (Color.Red, {cubeKey = "blank"; cubeRect = game_x-1, game_y-1, game_w+2, game_h+2; blockRect = None})
        ImageColour (Color.DarkGreen, {cubeKey = "blank"; cubeRect = game_x, game_y, game_w, game_h; blockRect = None})
    ]

    let nextBlockArea = [
        ImageColour (Color.Red, {cubeKey = "blank"; cubeRect = next_x-1, next_y-1, next_w+2, next_h+2; blockRect = None})
        ImageColour (Color.DarkGreen, {cubeKey = "blank"; cubeRect = next_x, next_y, next_w, next_h; blockRect = None})
    ]
    let rows =
        match fSharpModel.rowscancel with
        | Some rows -> rows |> List.map (fun (_, _,y) -> y)
        | _ -> []

    let cubes = 
        fSharpModel.cubes
        |> List.map (fun (c,x,y) -> 
            let color = if List.contains y rows then Color.White else mapColor c
            ImageColour (color, {cubeKey = "cube"; cubeRect = posNext (x,y) (game_x, game_y); blockRect = None}))
         
    let currentStyle = 
        match fSharpModel.cubeStyle with
        | Some (colour, blocks) -> 
             outline fSharpModel.pos blocks
             |> List.map (fun (x, y) -> 
                        ImageColour (mapColor colour, {cubeKey = "cube"; cubeRect= posNext (x,y) (game_x, game_y); blockRect = None}))
            | _ -> []

    let nextBlockColour = mapColor <| fst fSharpModel.nextCubeStyle
    let sw, sh = snd fSharpModel.nextCubeStyle |> List.head |> List.length, snd fSharpModel.nextCubeStyle |> List.length
    let sow, soh = (next_w - (sw * blockwidth)) / 2, (next_h - (sh * blockwidth)) / 2
    
    let nextCubeStyle =
        outline (0, 0) <| snd fSharpModel.nextCubeStyle
        |> List.map (fun (x,y) -> 
             ImageColour (nextBlockColour, {cubeKey = "cube"; cubeRect = posNext(x,y) (next_x + sow, next_y + soh); blockRect= None}))
    
    let plainText = {cubeKey = "default"; text = ""; location = (0,0); align = Centre; streek = textMargin}  
    let text = [
        Text { plainText with text = "Score"; location = (score_x, score_y) }
        Text { plainText with text = string fSharpModel.score; location = (score_x, score_y + textHeight) }

        Text { plainText with text = "Level"; location = (level_x, level_y) }
        Text { plainText with text = string fSharpModel.level; location = (level_x, level_y) }

        Text { plainText with text ="Game Directions"; location = (int_x, int_y) }
        Text { plainText with streek =0.4; text = "Left Arrow to move Left, Right Arrow to move Right"; location = (int_x, int_y + textHeight)}
        Text { plainText with streek =0.4; text = "Up Arrow to Spin the Cube, Down Arrow to Come fast to down"; location = (int_x, int_y + textHeight)}
    ]
    let gameEnd = 
        if fSharpModel.isGameEnd then [
            ImageColour (Color.DarkBlue, { cubeKey = "blank"; cubeRect = canvasOverSized; blockRect = None})
            TextColour (Color.White, { plainText with streek = 0.7; text = "Game Over"; location = (gameOver_x, gameOver_y) })
            TextColour (Color.White, { plainText with text = "Press G to Restart the Gamr"; location = (gameOver_x, gameOver_y + textHeight + textHeight) })
        ] else []

    gameArea @ nextBlockArea @ cubes @ currentStyle @ nextCubeStyle @ text @ gameEnd

        
   
