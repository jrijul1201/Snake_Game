port module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Json.Decode.Pipeline
import Json.Encode
import Random
import Random.Extra
import Svg.Attributes exposing (direction)
import Task
import Time



-- PORTS


port signIn : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg


port saveHighScore : Json.Encode.Value -> Cmd msg


port receiveLeaderboards : (Json.Encode.Value -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = viewLayout
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { snake : Snake
    , gameState : GameState
    , size : Size
    , food : Point
    , score : Int
    , highScore : HighScore
    , pressedKey : String
    , emoji : Emoji
    , userData : Maybe UserData
    , error : ErrorData
    , leaderboards : Leaderboards
    , theme : Theme
    , windowWidth : Int
    , windowHeight : Int
    , level : Level
    }


type alias HighScore =
    { easy : Int
    , medium : Int
    , hard : Int
    }


type alias Theme =
    { bgcolour : Color
    , fontcolour : Color
    , helpcolour : Color
    , helpfcolour : Color
    }


type Level
    = Easy
    | Medium
    | Hard


dark : Theme
dark =
    { bgcolour = rgb255 40 44 52
    , fontcolour = rgb255 171 178 191
    , helpcolour = rgb255 216 154 158
    , helpfcolour = rgb255 40 44 52
    }


light : Theme
light =
    { bgcolour = rgb255 255 209 102
    , fontcolour = rgb255 83 19 30
    , helpcolour = rgb255 233 166 166
    , helpfcolour = rgb255 83 19 30
    }


type alias Leaderboards =
    { easy : Leaderboard
    , medium : Leaderboard
    , hard : Leaderboard
    }


type alias Leaderboard =
    { first : Position
    , second : Position
    , third : Position
    }


type alias Position =
    { name : String
    , highScore : Int
    }


type GameState
    = Playing
    | Paused
    | Lost
    | PlayingAgain
    | LoadedPage
    | HelpPage


type alias Snake =
    { body : List Point
    , head : Point
    , currentDirection : Direction
    , nextDirection : Direction
    }


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Point =
    { x : Int
    , y : Int
    }


type alias Size =
    { min : Int
    , max : Int
    }


type alias ErrorData =
    { code : Maybe String
    , message : Maybe String
    , credential : Maybe String
    }


type alias UserData =
    { name : String
    , uid : String
    , highScore : HighScore
    }


type alias Emoji =
    { head : String
    , tail : String
    , food : String
    }


cat : Emoji
cat =
    Emoji "😼" "🌕" "🍍"


pumpkin : Emoji
pumpkin =
    Emoji "🎃" "🟠" "🍊"


monster : Emoji
monster =
    Emoji "👾" "🟣" "🍇"


skull : Emoji
skull =
    Emoji "💀" "⚪️" "🍙"


dog : Emoji
dog =
    Emoji "🐶" "🟤" "🦴"


devil : Emoji
devil =
    Emoji "😈" "⭕" "🍓"


alien : Emoji
alien =
    Emoji "👽" "🟢" "🛸"


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform GotViewport Browser.Dom.getViewport
    )


initialModel : Model
initialModel =
    { snake = initialSnake
    , gameState = LoadedPage
    , size = Size 1 15
    , food = Point 3 3
    , score = 0
    , highScore = HighScore 0 0 0
    , pressedKey = "Just loaded"
    , emoji = cat
    , userData = Maybe.Nothing
    , error = emptyError
    , leaderboards = Leaderboards emptyLeaderboard emptyLeaderboard emptyLeaderboard
    , theme = light
    , windowWidth = 1300
    , windowHeight = 600
    , level = Easy
    }


initialSnake : Snake
initialSnake =
    Snake
        [ Point 1 1, Point 2 1, Point 3 1 ]
        (Point 3 1)
        Right
        Right



-- UPDATE


type Msg
    = Tick Time.Posix
    | SetGameState GameState
    | ChangeDirection Direction
    | NewFood Point
    | KeyPressed String
    | UnknownKeyPressed
    | PlayAgain
    | ChangeEmoji Emoji
    | LogIn
    | LogOut
    | LoggedInData (Result Decode.Error UserData)
    | LoggedInError (Result Decode.Error ErrorData)
    | ReceiveLeaderboards (Result Decode.Error Leaderboards)
    | SetTheme Theme
    | GotNewWidth Int Int
    | GotViewport Viewport
    | SetLevel Level


emptyError : ErrorData
emptyError =
    { code = Maybe.Nothing
    , credential = Maybe.Nothing
    , message = Maybe.Nothing
    }


emptyLeaderboard : Leaderboard
emptyLeaderboard =
    { first = Position "" 0
    , second = Position "" 0
    , third = Position "" 0
    }


validateSnake : Size -> Snake -> Bool
validateSnake size snake =
    isSnakeInBounds size snake && not (didEatHimself snake)


didEatHimself : Snake -> Bool
didEatHimself snake =
    let
        reversed =
            List.reverse snake.body
    in
    case reversed of
        -- means x = head and xs = rest of the body
        x :: xs ->
            -- means x is a member of xs
            List.member x xs

        -- will be executed if reversed is empty
        _ ->
            False


isSnakeInBounds : Size -> Snake -> Bool
isSnakeInBounds size snake =
    -- just to get it formatted correctly
    True
        && (snake.head.x <= size.max)
        && (snake.head.x >= size.min)
        && (snake.head.y <= size.max)
        && (snake.head.y >= size.min)


moveSnake : Snake -> Bool -> Snake
moveSnake { body, head, nextDirection } ateFood =
    let
        tail =
            if ateFood then
                body
                -- Saying x |> f is exactly the same as f x.
                -- String.toInt (String.trim input) is equivalent to input
                -- |> String.trim
                -- |> String.toInt

            else
                body
                    |> List.tail
                    |> Maybe.withDefault initialSnake.body

        -- Maybe.withDefault 100 (Just 42)   -- 42
        -- Maybe.withDefault 100 Nothing     -- 100
        updateBody newHead =
            Snake (tail ++ [ newHead ]) newHead nextDirection nextDirection
    in
    case nextDirection of
        Right ->
            Point (head.x + 1) head.y |> updateBody

        Left ->
            Point (head.x - 1) head.y |> updateBody

        Up ->
            Point head.x (head.y - 1) |> updateBody

        Down ->
            Point head.x (head.y + 1) |> updateBody


generatePoint : Model -> Random.Generator Point
generatePoint model =
    let
        generateInt : Size -> Random.Generator Int
        generateInt { min, max } =
            Random.int min max

        generateGenericPoint : Size -> Random.Generator Point
        generateGenericPoint size =
            Random.map2 Point (generateInt size) (generateInt size)
    in
    Random.Extra.filter (\p -> not <| List.member p model.snake.body) (generateGenericPoint model.size)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newSnake =
                    moveSnake model.snake False

                ateFood =
                    List.member model.food newSnake.body

                isValid =
                    validateSnake model.size newSnake
            in
            if model.gameState == Playing || model.gameState == PlayingAgain then
                let
                    highScore =
                        if model.level == Easy then
                            model.highScore.easy

                        else if model.level == Medium then
                            model.highScore.medium

                        else
                            model.highScore.hard
                in
                if isValid then
                    if ateFood then
                        ( { model | snake = moveSnake model.snake True, score = model.score + 1 }
                        , Random.generate NewFood (generatePoint model)
                        )

                    else
                        ( { model | snake = newSnake }
                        , Cmd.none
                        )

                else if highScore < model.score then
                    let
                        highscore =
                            model.highScore

                        newhighscore =
                            if model.level == Easy then
                                { highscore | easy = model.score }

                            else if model.level == Medium then
                                { highscore | medium = model.score }

                            else
                                { highscore | hard = model.score }

                        newmodel =
                            { model | gameState = Lost, highScore = newhighscore }
                    in
                    ( newmodel
                    , saveHighScore <| highscoreEncoder newmodel
                    )

                else
                    ( { model | gameState = Lost }
                    , Cmd.none
                    )

            else
                ( model, Cmd.none )

        SetGameState gameState ->
            ( { model | gameState = gameState }
            , Cmd.none
            )

        ChangeDirection direction ->
            if model.gameState == Playing || model.gameState == PlayingAgain then
                let
                    newDirection =
                        calcNextDirection model.snake.currentDirection direction

                    newSnake snake =
                        { snake | nextDirection = newDirection }
                in
                ( { model | snake = newSnake model.snake }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        PlayAgain ->
            ( { model | gameState = PlayingAgain, food = Point 3 3, snake = initialSnake, score = 0, pressedKey = "keys here" }
            , Cmd.none
            )

        NewFood point ->
            ( { model | food = point }
            , Cmd.none
            )

        KeyPressed key ->
            ( { model | pressedKey = key }
            , Cmd.none
            )

        UnknownKeyPressed ->
            ( model, Cmd.none )

        ChangeEmoji emoji ->
            ( { model | emoji = emoji }, Cmd.none )

        LogIn ->
            ( model, signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing, error = emptyError, highScore = HighScore 0 0 0 }, signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value, highScore = value.highScore }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Decode.errorToString error }, Cmd.none )

        ReceiveLeaderboards result ->
            case result of
                Ok value ->
                    ( { model | leaderboards = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Decode.errorToString error }, Cmd.none )

        SetTheme theme ->
            ( { model | theme = theme }
            , Cmd.none
            )

        GotNewWidth w h ->
            ( { model | windowWidth = w, windowHeight = h }
            , Cmd.none
            )

        GotViewport viewport ->
            let
                viewport1 =
                    Just viewport

                w =
                    round viewport.scene.width

                h =
                    round viewport.scene.height
            in
            ( { model | windowWidth = w, windowHeight = h }, Cmd.none )

        SetLevel level ->
            if model.gameState == Lost || model.gameState == LoadedPage then
                ( { model | level = level }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )


highscoreEncoder : Model -> Json.Encode.Value
highscoreEncoder model =
    Json.Encode.object
        [ ( "easy", Json.Encode.int model.highScore.easy )
        , ( "medium", Json.Encode.int model.highScore.medium )
        , ( "hard", Json.Encode.int model.highScore.hard )
        , ( "uid"
          , case model.userData of
                Just userData ->
                    Json.Encode.string userData.uid

                Maybe.Nothing ->
                    Json.Encode.null
          )
        , ( "displayname"
          , case model.userData of
                Just userData ->
                    Json.Encode.string userData.name

                Maybe.Nothing ->
                    Json.Encode.null
          )
        ]


messageToError : String -> ErrorData
messageToError message =
    { code = Maybe.Nothing, credential = Maybe.Nothing, message = Just message }


userDataDecoder : Decode.Decoder UserData
userDataDecoder =
    Decode.succeed UserData
        |> Json.Decode.Pipeline.required "name" Decode.string
        |> Json.Decode.Pipeline.required "uid" Decode.string
        |> Json.Decode.Pipeline.required "highScore" highScoreDecoder


highScoreDecoder : Decode.Decoder HighScore
highScoreDecoder =
    Decode.succeed HighScore
        |> Json.Decode.Pipeline.required "easy" Decode.int
        |> Json.Decode.Pipeline.required "medium" Decode.int
        |> Json.Decode.Pipeline.required "hard" Decode.int


logInErrorDecoder : Decode.Decoder ErrorData
logInErrorDecoder =
    Decode.succeed ErrorData
        |> Json.Decode.Pipeline.required "code" (Decode.nullable Decode.string)
        |> Json.Decode.Pipeline.required "message" (Decode.nullable Decode.string)
        |> Json.Decode.Pipeline.required "credential" (Decode.nullable Decode.string)


leaderboardsDecoder : Decode.Decoder Leaderboards
leaderboardsDecoder =
    Decode.succeed Leaderboards
        |> Json.Decode.Pipeline.required "easy" leaderboardDecoder
        |> Json.Decode.Pipeline.required "medium" leaderboardDecoder
        |> Json.Decode.Pipeline.required "hard" leaderboardDecoder


leaderboardDecoder : Decode.Decoder Leaderboard
leaderboardDecoder =
    Decode.succeed Leaderboard
        |> Json.Decode.Pipeline.required "first" positionDecoder
        |> Json.Decode.Pipeline.required "second" positionDecoder
        |> Json.Decode.Pipeline.required "third" positionDecoder


positionDecoder : Decode.Decoder Position
positionDecoder =
    Decode.succeed Position
        |> Json.Decode.Pipeline.required "name" Decode.string
        |> Json.Decode.Pipeline.required "highScore" Decode.int


type Side
    = Horizontal
    | Vertical


mapDirectionToSide : Direction -> Side
mapDirectionToSide dir =
    case dir of
        Left ->
            Horizontal

        Right ->
            Horizontal

        Up ->
            Vertical

        Down ->
            Vertical


calcNextDirection : Direction -> Direction -> Direction
calcNextDirection prev next =
    let
        prevSide =
            mapDirectionToSide prev

        nextSide =
            mapDirectionToSide next
    in
    case ( prevSide, nextSide ) of
        ( Horizontal, Vertical ) ->
            next

        ( Vertical, Horizontal ) ->
            next

        _ ->
            prev



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (speedUp model.score model.level) Tick
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onKeyDown keyDecoderToString
        , signInInfo (Decode.decodeValue userDataDecoder >> LoggedInData)
        , signInError (Decode.decodeValue logInErrorDecoder >> LoggedInError)
        , receiveLeaderboards (Decode.decodeValue leaderboardsDecoder >> ReceiveLeaderboards)
        , Browser.Events.onResize (\w h -> GotNewWidth w h)
        ]


speedUp : Int -> Level -> Float
speedUp score level =
    let
        x =
            toFloat score

        rate =
            case level of
                Easy ->
                    1.05

                Medium ->
                    1.1

                Hard ->
                    1.15

        initialSpeed =
            300

        finalSpeed =
            150
    in
    (rate ^ -x) * (initialSpeed - finalSpeed) + finalSpeed


keyDecoderToString : Decode.Decoder Msg
keyDecoderToString =
    Decode.map KeyPressed (Decode.field "key" Decode.string)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMessage (Decode.field "key" Decode.string)


toMessage : String -> Msg
toMessage string =
    string |> foobar



-- VIEW


noOutline =
    Element.htmlAttribute <| Html.Attributes.style "box-shadow" "none"


view : Model -> Element Msg
view model =
    let
        w =
            px model.windowWidth
    in
    -- row [ centerX, centerY, padding 5, width w, inFront <| viewHelp model ]
        row [ centerX, centerY, padding 5, width w]
        [ viewCorner model.theme <| 50
        , viewKeyLine model.gameState model.level <| 200
        , viewGrid model <| 520
        , viewLeaderboards model.theme model.level model.leaderboards <| 300
        ]


viewHelp : Model -> Element Msg
viewHelp model =
    let
        w =
            model.windowWidth

        h =
            model.windowHeight

        bg =
            el [ width <| px w, height <| px h, Background.color <| rgb255 19 28 33, alpha 0.5 ] none
        
        -- im = image [] {description = "ss", src = "help_keyboard.png"}
    in
    if model.pressedKey == "Just loaded" then
        row [ width <| px w, height <| px h, centerX, centerY, font, Font.color <| rgb255 255 255 255, Font.center, Font.size 25, behindContent bg ] 
        -- [column [] [ im] ]
        [el [](text "shh")] 

    else
        none


font : Attribute Msg
font =
    Font.family
        [ Font.external
            { name = "Carter One"
            , url = "https://fonts.googleapis.com/css2?family=Carter+One"
            }
        ]


viewGrid : Model -> Int -> Element Msg
viewGrid model w =
    column [ centerX, centerY, width (fillPortion 40) ]
        [ viewMainHeading w
        , viewLoginLine model.userData w
        , row [ padding 10, width <| px w, centerX ]
            [ playButton model.gameState w
            , viewScore model.emoji model.score w
            , viewHighScore model.level model.highScore w
            ]
        , board model 520
        , viewLastLine w
        ]


viewMainHeading : Int -> Element Msg
viewMainHeading w =
    let
        pixels =
            px w
    in
    el [ Font.size <| 45, width pixels, Font.center, font, centerX ] (text "SNAKE MANIA")


viewLoginLine : Maybe UserData -> Int -> Element Msg
viewLoginLine userdata w =
    row [ padding 10, width <| px w, centerX ]
        [ viewName userdata w
        , viewLoginButton userdata w
        ]


viewScore : Emoji -> Int -> Int -> Element Msg
viewScore emoji score w =
    let
        pixels =
            px <| 150
    in
    el [ Font.size <| 35, width pixels, Font.center, font ] (text <| emoji.food ++ " - " ++ String.fromInt score)


viewHighScore : Level -> HighScore -> Int -> Element Msg
viewHighScore level model_highScore w =
    let
        pixels =
            px <| 150

        highScore =
            if level == Easy then
                model_highScore.easy

            else if level == Medium then
                model_highScore.medium

            else
                model_highScore.hard
    in
    el [ Font.size <| 35, width pixels, Font.alignRight, font ] (text <| "🏆 - " ++ String.fromInt highScore)


viewLastLine : Int -> Element Msg
viewLastLine w =
    let
        pixels =
            px <| 72

        templateButton str state =
            button [ Font.size <| 35, width pixels, Font.center, font, noOutline, width (fillPortion 1) ]
                { label = text str
                , onPress = Just state
                }
    in
    row [ padding 10, width <| px w, centerX ]
        [ templateButton cat.head (ChangeEmoji cat)
        , templateButton pumpkin.head (ChangeEmoji pumpkin)
        , templateButton monster.head (ChangeEmoji monster)
        , templateButton skull.head (ChangeEmoji skull)
        , templateButton devil.head (ChangeEmoji devil)
        , templateButton alien.head (ChangeEmoji alien)
        , templateButton dog.head (ChangeEmoji dog)
        ]


viewLeaderboards : Theme -> Level -> Leaderboards -> Int -> Element Msg
viewLeaderboards theme level leaderboards w =
    let
        spacing =
            padding 8

        leaderboard leaderboardLevel color =
            [ viewLeaderboardLine 1 (shortName leaderboardLevel.first.name) leaderboardLevel.first.highScore w color
            , viewLeaderboardLine 2 (shortName leaderboardLevel.second.name) leaderboardLevel.second.highScore w color
            , viewLeaderboardLine 3 (shortName leaderboardLevel.third.name) leaderboardLevel.third.highScore w color
            ]

        easy_color =
            if level == Easy then
                orange

            else
                theme.fontcolour

        medium_color =
            if level == Medium then
                orange

            else
                theme.fontcolour

        hard_color =
            if level == Hard then
                orange

            else
                theme.fontcolour
    in
    column [ centerX, centerY, spacing, width (fillPortion 30) ]
        ([ viewLeaderboardHeading w ]
            ++ [ el [ Font.size 25 ] (text "  ") ]
            ++ [ viewLeaderboardSubHeading w "Easy" easy_color ]
            ++ leaderboard leaderboards.easy easy_color
            ++ [ viewLeaderboardSubHeading w "Medium" medium_color ]
            ++ leaderboard leaderboards.medium medium_color
            ++ [ viewLeaderboardSubHeading w "Hard" hard_color ]
            ++ leaderboard leaderboards.hard hard_color
        )


shortName : String -> String
shortName name =
    let
        words =
            String.words name
    in
    String.join "" <| List.take 1 words


viewLeaderboardLine : Int -> String -> Int -> Int -> Color -> Element Msg
viewLeaderboardLine position name highScore w color =
    row [ padding 8, centerX, centerY ]
        [ el [ Font.size <| 25, width <| px 250, Font.alignLeft, font, Font.color color ] (text <| String.fromInt position ++ "\t" ++ name)
        , el [ Font.size <| 25, width <| px 50, Font.alignRight, font, Font.color color ] (text <| String.fromInt highScore)
        ]


viewLeaderboardHeading : Int -> Element Msg
viewLeaderboardHeading w =
    let
        spacing =
            paddingEach
                { top = 0
                , right = 0
                , bottom = 8
                , left = 0
                }
    in
    el [ Font.size <| 35, Font.center, font, spacing, centerX ] (text "🏆 Leaderboard 🏆")


viewLeaderboardSubHeading : Int -> String -> Color -> Element Msg
viewLeaderboardSubHeading w str color =
    let
        spacing =
            paddingEach
                { top = 0
                , right = 0
                , bottom = 8
                , left = 0
                }
    in
    el [ Font.size <| 30, Font.center, font, spacing, centerX, Font.color color ] (text str)


viewCorner : Theme -> Int -> Element Msg
viewCorner theme w =
    let
        templateButton str state =
            button [ Font.size <| 30, alignLeft, font, noOutline ]
                { label = text str
                , onPress = Just state
                }
    in
    column [ alignRight, alignTop, padding 5, width (fillPortion 4) ]
        [ if theme == dark then
            templateButton "🌖" (SetTheme light)

          else
            templateButton "🌘" (SetTheme dark)
        ]


viewKeyLine : GameState -> Level -> Int -> Element Msg
viewKeyLine gamestate level w =
    column [ centerX, centerY, width (fillPortion 20) ] <|
        viewLevels level w
            ++ [ row [ height <| px 30 ] [] ]
            ++ viewArrowKeys w
            ++ [ row [ height <| px 30 ] [] ]
            ++ viewHelpButton gamestate


viewArrowKeys : Int -> List (Element Msg)
viewArrowKeys w =
    let
        pixels =
            px <| 35

        buttongap =
            px <| 90

        templateButton str state =
            button [ Font.size <| 35, width pixels, Font.center, font, noOutline ]
                { label = text str
                , onPress = Just state
                }
    in
    [ row [ centerX, padding 16 ]
        [ templateButton "⬆️" (foobar "ArrowUp")
        ]
    , row [ centerX, padding 8 ]
        [ templateButton "⬅️" (foobar "ArrowLeft")
        , el [ width buttongap ] (text "              ")
        , templateButton "➡️" (foobar "ArrowRight")
        ]
    , row [ centerX, padding 16 ]
        [ templateButton "⬇️" (foobar "ArrowDown")
        ]
    ]


viewLevels : Level -> Int -> List (Element Msg)
viewLevels level w =
    let
        pixels =
            px <| 140

        templateButton str state highlight =
            if highlight then
                button [ Font.size <| 35, width pixels, Font.center, font, Font.color orange, noOutline ]
                    { label = text str
                    , onPress = Just state
                    }

            else
                button [ Font.size <| 35, width pixels, Font.center, font, noOutline ]
                    { label = text str
                    , onPress = Just state
                    }

        highlightEasy =
            level == Easy

        highlightMedium =
            level == Medium

        highlightHard =
            level == Hard
    in
    [ row [ centerX, padding 8 ]
        [ templateButton "Easy" (SetLevel Easy) highlightEasy
        ]
    , row [ centerX, padding 8 ]
        [ templateButton "Medium" (SetLevel Medium) highlightMedium
        ]
    , row [ centerX, padding 8 ]
        [ templateButton "Hard" (SetLevel Hard) highlightHard
        ]
    ]


type DirectionMap
    = Match Direction
    | NoMatch String


coolMap : String -> Direction -> DirectionMap -> DirectionMap
coolMap binding direction dirmap =
    case dirmap of
        Match d ->
            Match d

        NoMatch key ->
            if key == binding then
                Match direction

            else
                NoMatch key


unWrap : DirectionMap -> Msg
unWrap dirmap =
    case dirmap of
        Match direction ->
            ChangeDirection direction

        NoMatch _ ->
            UnknownKeyPressed


foobar : String -> Msg
foobar key =
    NoMatch key
        |> coolMap "ArrowLeft" Left
        |> coolMap "ArrowRight" Right
        |> coolMap "ArrowUp" Up
        |> coolMap "ArrowDown" Down
        |> unWrap


viewLayout : Model -> Html Msg
viewLayout model =
    layout [ Background.color model.theme.bgcolour, Font.color model.theme.fontcolour, width <| px model.windowWidth, centerX, centerY ] (view model)


playButton : GameState -> Int -> Element Msg
playButton game w =
    let
        pixels =
            px <| 206

        templateButton str state =
            button [ Font.size <| 35, width pixels, Font.alignLeft, font, noOutline ]
                { label = text str
                , onPress = Just state
                }
    in
    case game of
        Playing ->
            templateButton "Pause" (SetGameState Paused)

        Paused ->
            templateButton "Resume" (SetGameState Playing)

        Lost ->
            templateButton "Play" PlayAgain

        PlayingAgain ->
            templateButton "Pause" (SetGameState Paused)

        LoadedPage ->
            templateButton "Play" (SetGameState Playing)

        HelpPage ->
            templateButton "" (SetGameState LoadedPage)


viewHelpButton : GameState -> List (Element Msg)
viewHelpButton gamestate =
    let
        pixels =
            px <| 140

        templateButton str state =
            button [ Font.size <| 35, width pixels, Font.center, font, noOutline ]
                { label = text str
                , onPress = Just state
                }
    in
    if gamestate == HelpPage then
        [ row [ centerX ] [ templateButton "" (SetGameState LoadedPage) ] ]

    else
        [ row [ centerX ] [ templateButton "Help" (SetGameState HelpPage) ] ]


viewLoginButton : Maybe UserData -> Int -> Element Msg
viewLoginButton userdata w =
    let
        pixels =
            px <| 150

        templateButton str state =
            button [ Font.size <| 30, width pixels, Font.alignRight, font, noOutline, width (fillPortion 1) ]
                { label = text str
                , onPress = Just state
                }
    in
    case userdata of
        Just data ->
            templateButton "Logout" LogOut

        Maybe.Nothing ->
            templateButton "Login" LogIn


viewName : Maybe UserData -> Int -> Element Msg
viewName userdata w =
    let
        pixels =
            px <| 356

        template str =
            el [ Font.size <| 30, width pixels, Font.alignLeft, font, width (fillPortion 35) ]
                (text str)
    in
    case userdata of
        Just data ->
            template <| "Hi, " ++ shortName data.name

        Maybe.Nothing ->
            template ""


orange : Color
orange =
    rgb255 247 99 12


board : Model -> Int -> Element Msg
board model w =
    let
        sizeRange =
            List.range model.size.min model.size.max

        tilepadding =
            spacing 4

        tileType x y =
            if Point x y == model.snake.head then
                SnakeHead

            else if List.member (Point x y) model.snake.body then
                SnakeTail

            else if Point x y == model.food then
                Food

            else
                Board

        templateButton str state =
            button [ Font.size <| 25, Font.alignRight, font, noOutline, width (fillPortion 1) ]
                { label = text str
                , onPress = Just state
                }

        gameover_helpmenu =
            if model.gameState == Lost then
                el [ Font.size <| 45, centerX, font, centerY, Background.color model.theme.bgcolour ] (text "Game Over!")

            else if model.gameState == HelpPage then
                textColumn [ Font.size <| 20, centerX, centerY, font, height <| px 470, width <| px 370, Font.justify, Background.color model.theme.helpcolour, Font.color model.theme.helpfcolour, padding 10 ]
                    [ row [ centerX ]
                        [ el [ Font.center, Font.size 25, Font.center, width (fillPortion 36) ] (text "How to Play")
                        , templateButton "X" (SetGameState LoadedPage)
                        ]
                    , el [ Font.center, Font.size 25 ] (text " ")
                    , paragraph [] [ text "Play/Pause:" ]
                    , paragraph [] [ text "Start or pause the game." ]
                    , el [ Font.center, Font.size 10 ] (text " ")
                    , paragraph [] [ text "Login/Logout:" ]
                    , paragraph [] [ text "Helps you save save highscore." ]
                    , el [ Font.center, Font.size 10 ] (text " ")
                    , paragraph [] [ text "Easy/Medium/Hard:" ]
                    , paragraph [] [ text "Change difficulty of the game." ]
                    , el [ Font.center, Font.size 10 ] (text " ")
                    , paragraph [] [ text "Light/Dark mode:" ]
                    , paragraph [] [ text "Click on 🌖, at top left corner." ]
                    , el [ Font.center, Font.size 10 ] (text " ")
                    , paragraph [] [ text "High Score:" ]
                    , paragraph [] [ text "Check trophy 🏆, on top of grid." ]
                    , el [ Font.center, Font.size 10 ] (text " ")
                    , paragraph [] [ text "Score:" ]
                    , paragraph [] [ text <| "Check fruit " ++ model.emoji.food ++ ", on top of grid." ]
                    , el [ Font.center, Font.size 10 ] (text " ")
                    , paragraph [] [ text "Leaderboard:" ]
                    , paragraph [] [ text "Shows all the top highscores." ]
                    ]

            else
                none
    in
    column [ padding 15, tilepadding, width <| px w, centerX, centerY, inFront gameover_helpmenu ]
        (List.map
            (\y ->
                row [ tilepadding, width <| px w, centerX, centerY ]
                    (List.map (\x -> viewTile model.emoji (tileType x y) w) sizeRange)
            )
            sizeRange
        )


type TileType
    = SnakeHead
    | SnakeTail
    | Board
    | Food


viewTile : Emoji -> TileType -> Int -> Element Msg
viewTile emoji tile w =
    let
        pixels =
            px <| 30

        template fsize str spacing =
            el [ width pixels, height pixels, Font.size <| fsize, font, padding spacing, centerX, centerY ] (text str)
    in
    case tile of
        SnakeTail ->
            template 30 emoji.tail 0

        SnakeHead ->
            template 30 emoji.head 0

        Food ->
            template 27 emoji.food 0

        Board ->
            template 15 "🌳" 10
