module Gifphy exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (at, field, int, string, list, decodeString, Decoder, succeed)
import Json.Decode.Pipeline exposing (decode, required, requiredAt,  optional, hardcoded)
import Html.Attributes exposing (src, value,  placeholder)



-- MODEL --

type alias Gif =
    { url : String}

type alias Model =
    {
       q : String
       , url: String
       , results : GifResponse
    }

type alias GifResponse = { data: List Gif }

init : (Model, Cmd Msg)
init =
     (model, getGifs model.q)

model : Model
model =
    {
      q = "cats"
      , url = giphyUrlBuilder "cats"
      , results = {data = [{url = ""}]}
    }



giphyBaseUrl : String
giphyBaseUrl =
    "https://api.giphy.com/v1/gifs/search?api_key=<YOUR_KEY>"

giphyParams : String
giphyParams =
  "&limit=25&offset=0&rating=G&lang=en"


giphyUrlBuilder : String -> String
giphyUrlBuilder search =
    giphyBaseUrl
    ++ "&q="
    ++ search
    ++ giphyParams


getGifs : String -> Cmd Msg
getGifs query =
    (gifResponseDecoder)
      |> Http.get (giphyUrlBuilder query)
      |> Http.send NewGifs


--gifsDecoder : Decoder Gifs
--gifsDecoder =
--    decode Gifs
--      |> required "data" (list gifDecoder)


gifDecoder : Decoder Gif
gifDecoder =
    decode Gif
      |> requiredAt ["images", "fixed_height", "url"] string

gifListDecoder : Decoder (List Gif)
gifListDecoder =
    Decode.list gifDecoder


gifResponseDecoder : Decoder GifResponse
gifResponseDecoder =
    decode GifResponse
      |> required "data" gifListDecoder

-- MSG --


type Msg =
  Change String
  | NewGifs (Result Http.Error GifResponse)

-- UPDATE --
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Change str ->
          ({model | q = str }, getGifs model.q)

        NewGifs (Ok gifs) ->
                   ({model | results = gifs}, Cmd.none)

        NewGifs (Err error) ->
            let
                 _ =  Debug.log "ooops" error
            in
                (model, Cmd.none)
-- VIEW --

view : Model -> Html Msg
view m =
    div []
      [ input [placeholder "hold this", onInput Change ] []
      , div [] [ text (toString m) ]
      ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

{--

imagesDecoder : Decoder Images
imagesDecoder =
    decode
      Images
      |> required "data" (list ImageWrapper)



imageDecoder : Decoder Image
imageDecoder  =
    decode
      Image
      |> required "images"

gifDecoder : Decoder Gif
gifDecoder =
  decode
      Gif
      |> required "url"
--}
