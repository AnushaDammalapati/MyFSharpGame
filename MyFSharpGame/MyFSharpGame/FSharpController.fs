module FSharpController

open FSharpModel
open FSharpGame
open Microsoft.Xna.Framework.Input

let keyMapping =
    function
    | Keys.Left -> Some ArrowEvent.LeftArrow
    | Keys.Right -> Some ArrowEvent.RightArrow
    | Keys.Up -> Some ArrowEvent.UpArrow
    | _ -> None

let initializeModel elapsed = 
    Some { loadModel with recentBlockTime = elapsed; recentEventTime = elapsed}

let graphicGame (runState: PlayState) getGameModel = 
    match getGameModel with
    | None -> 
        initializeModel runState.elapsed
    | Some _ when runState.WasJustPressed Keys.Escape -> 
        None
    | Some m when m.isGameEnd && runState.WasJustPressed Keys.G -> 
        initializeModel runState.elapsed
    | Some m when m.isGameEnd ->
        Some { m with clickEvent = [] }
    | Some m ->
        let command = List.map keyMapping runState.keyboard.pressed |> List.tryPick id
        let isDropPressed = List.contains Keys.Down runState.keyboard.pressed
        FSharpModel.graphicGame runState.elapsed command isDropPressed m |> Some
