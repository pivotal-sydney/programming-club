-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Action =
  { id: Int
  , value: String
  , isEditing: Bool
  }

type alias Model =
  { items : List Action
  , value : String
  , nextIndex: Int
  }


init : (Model, Cmd Msg)
init =
  ( Model [] "" 0
  , Cmd.none
  )



-- UPDATE


type Msg
  = Update String
  | Add
  | Delete Int
  | Edit Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update value ->
      ({model | value = value}, Cmd.none)
    Add ->
      ({model | items = (Action model.nextIndex model.value False) :: model.items
      , value = ""
      , nextIndex = model.nextIndex + 1}, Cmd.none)
    Delete value ->
      ({model | items = List.filter (itemNotEquals value) model.items}, Cmd.none)
    Edit id ->
      ({model | items = List.map (setEditMode id) model.items }, Cmd.none)

-- VIEW

setEditMode id item =
    {item | isEditing = (id == item.id)}


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Actions"]
    , ul [] (listOfActions model)
    , button [ onClick Add ] [ text "Add" ]
    , input [ type_ "text", placeholder "Action Item", value model.value, onInput Update ] []
    ]

listOfActions model =
    List.map actionToItem model.items

actionToItem item =
    li []
        [ text item.value
        , button [ onClick (Edit item.id) ] [ text "Edit" ]
        , button [ onClick (Delete item.id) ] [ text "Delete" ]
        ]

itemNotEquals id item =
    id /= item.id


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

