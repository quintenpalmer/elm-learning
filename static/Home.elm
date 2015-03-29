import Color (black, white , rgb, Color(..))
import Graphics.Element (Element, container, flow)
import Graphics.Element as Element
import Text
import List
import Http
import Result
import Signal
import Window
import Mouse
import Json.Decode ((:=))
import Json.Decode as Decode

main : Signal Element
main = Signal.map3 (\x y z -> view (parseSignals x y z)) Window.dimensions Mouse.position getAlbums

getAlbums : Signal (Http.Response String)
getAlbums = Http.send <| Signal.constant <| Http.get <| requestPath "artists"

requestPath : String -> String
requestPath path =
    "http://localhost:8080/rest/v1/" ++ path

type alias Dimensions =
    { width: Int
    , height: Int
    }

type alias Position =
    { x: Int
    , y: Int
    }

type alias Signals =
    { dimensions: Dimensions
    , position: Position
    , response: Http.Response String
    }

parseSignals : (Int, Int) -> (Int, Int) -> Http.Response String -> Signals
parseSignals (w, h) (x, y) resp = Signals (Dimensions w h) (Position x y) resp

background : Color
background = rgb 255 78 0

mainBackground : Color
mainBackground = rgb 255 255 255

mainWidth : Int
mainWidth = 800

type alias Artist =
    { name: String
    }

artist : Decode.Decoder Artist
artist = Decode.object1 Artist ("name" := Decode.string)

artists : Decode.Decoder (List Artist)
artists = Decode.list artist

view : Signals -> Element
view signals =
    flow Element.outward [
        Element.color background <|
        container (.width (.dimensions signals)) (.height (.dimensions signals)) Element.midTop <|
            mainContainer signals,
        devInfo signals
    ]

mainContainer : Signals -> Element
mainContainer signals =
    let dims = .dimensions signals
        width = .width dims
        height = .height dims
    in
        Element.color mainBackground <|
        container mainWidth height Element.midTop <|
        flow Element.down [
            header signals,
            container mainWidth 800 Element.topLeft <| case (.response signals) of
                Http.Success resp -> buildArtistPanes resp
                Http.Waiting ->  Text.plainText "Loading"
                Http.Failure code status -> Text.plainText <| toString code ++ " : " ++ status
        ]

buildArtistPanes : String -> Element
buildArtistPanes resp = flow Element.down <| List.map buildArtistPane <| case (Decode.decodeString artists resp) of
    Result.Ok values -> values
    Result.Err _ -> []

buildArtistPane : Artist -> Element
buildArtistPane artist = container 100 100 Element.topLeft <| Text.plainText <| .name artist

header : Signals -> Element
header signals =
    flow Element.right [
        container 200 400 Element.topLeft <| Text.plainText "Welcome",
        container 200 400 Element.topLeft <| Text.plainText "Account"
    ]

devInfo : Signals -> Element
devInfo signals =
    let dims = .dimensions signals
        pos = .position signals
        w = .width dims
        h = .height dims
        x = .x pos
        y = .y pos
    in
        container w h Element.bottomRight <|
        Element.color black <|
        Text.leftAligned <| Text.color white <|
            Text.fromString (toString ((x, y), (w, h)))
