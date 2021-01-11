module FSharpGame

open System
open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type CubeWay = {
     key: string
     path: string
}

type LoadCubes =
| Dimention of CubeWay
| TextFont of CubeWay

type Align = |TopLeft | Centre

type SetImage = {
     cubeKey: string
     cubeRect: int * int * int * int
     blockRect: (int * int * int * int) option
}
type SetText = {
     cubeKey: string
     text: string
     location: int * int
     align: Align
     streek: float
}

type ViewUIDesign = 
| Image of SetImage
| ImageColour of Color * SetImage
| Text of SetText
|TextColour of Color * SetText

type pixel = 
| Window of int * int
| FullScreen of int * int

type private Content = 
| CubesFrom of Texture2D
| FontFrom of SpriteFont

type PlayState = {
    elapsed: float
    keyboard: KeyboardInfo
    mouse: MouseInfo
} and KeyboardInfo = {
      pressed: Keys list;
      downKey: Keys list;
      upKey: Keys list
} and MouseInfo = {
    location: int * int
    pressed: bool * bool
}

type PlayState with 
    member __.WasJustPressed key = List.contains key __.keyboard.downKey

type PlayGame<'TModel> (pixel, loadCubes, updateViewModel, designOfUI)
    as this = 
    inherit Game()

    let mutable animation = new GraphicsDeviceManager(this)
    let mutable things = Map.empty<string, Content>
    let mutable keyboardInfo = { pressed = []; downKey = []; upKey = []}
    let mutable presentModel: 'TModel option = None
    let mutable presentView: ViewUIDesign list = []
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    do
        match pixel with
        | FullScreen (w, h) -> 
            animation.PreferredBackBufferWidth <- w
            animation.PreferredBackBufferHeight <- h
            animation.IsFullScreen <- true
        | Window (w, h) -> 
            animation.PreferredBackBufferWidth <- w
            animation.PreferredBackBufferHeight <- h

    let changeKeyboardInfo (keyboard: KeyboardState) (existing: KeyboardInfo) = 
        let pressed = keyboard.GetPressedKeys ()|> Set.ofArray
        {
            pressed = pressed |> Set.toList
            downKey = Set.difference pressed (existing.pressed |> Set.ofList) |> Set.toList
            upKey = Set.difference (existing.pressed |> Set.ofList) pressed |> Set.toList
        }

    let getMouseInfo (mouse: MouseState) = 
        {
            location = mouse.X, mouse.Y
            pressed = mouse.LeftButton = ButtonState.Pressed, mouse.RightButton = ButtonState.Pressed
        }
        
    let vector2 (x,y) = new Vector2(float32 x, float32 y)
    let rect (x,y,width,height) = 
        new Rectangle (x,y,width,height)

    let imageStyle (spriteBlock: SpriteBatch) img colour = 
        let blockRect = 
            match img.blockRect with
            | None -> Unchecked.defaultof<Nullable<Rectangle>>
            | Some r -> rect r |> Nullable
        let texture = 
            match Map.tryFind img.cubeKey things with
            | Some (CubesFrom t) -> t
            | None -> sprintf " %s is missing" img.cubeKey |> failwith
            | _-> sprintf "Things not from Texture2D: %s" img.cubeKey |> failwith
        spriteBlock.Draw(
            texture, rect img.cubeRect,
            blockRect, colour, 0.0f, Vector2.Zero,
            SpriteEffects.None, 0.0f)
    let setText (spriteBlock: SpriteBatch) text colour = 
        let font = 
            match Map.tryFind text.cubeKey things with
            | Some (FontFrom f) -> f
            | None -> sprintf "Missing Things: %s" text.cubeKey |> failwith
            | _ -> sprintf "Things not from SpriteFont: %s" text.cubeKey |> failwith
        let location = 
            match text.align with 
            | TopLeft -> vector2 text.location
            | Centre ->
                let size = Vector2.Divide (font.MeasureString(text.text), 2.f / float32 text.streek)
                Vector2.Subtract (vector2 text.location, size)
        spriteBlock.DrawString(
                 font, text.text, location, colour, 
                  0.0f, Vector2.Zero, float32 text.streek, SpriteEffects.None, 0.5f)

    override __.LoadContent() = 
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        things <- 
            loadCubes
            |> List.map (
                function
                | Dimention info -> info.key, this.Content.Load<Texture2D>(info.path) |> CubesFrom
                | TextFont info -> info.key, this.Content.Load<SpriteFont>(info.path) |> FontFrom)
            |> Map.ofList

    override __.Update(gameTime) = 
        keyboardInfo <- changeKeyboardInfo (Keyboard.GetState()) keyboardInfo
        let mouseInfo = getMouseInfo (Mouse.GetState())
        let runlevel = {
            elapsed = gameTime.TotalGameTime.TotalMilliseconds 
            keyboard = keyboardInfo
            mouse = mouseInfo
        }

        presentModel <- updateViewModel runlevel presentModel
        match presentModel with 
        | None -> __.Exit()
        | Some model -> 
            presentView <- designOfUI runlevel model
     
    override __.Draw(_) = 
        this.GraphicsDevice.Clear Color.White

        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp)
        presentView
           |> List.iter (
                function 
                | Image i -> imageStyle spriteBatch i Color.White
                | ImageColour (c,i) -> imageStyle spriteBatch i c
                | Text t -> setText spriteBatch t Color.Black
                | TextColour (c,t) -> setText spriteBatch t c)

        spriteBatch.End()  

