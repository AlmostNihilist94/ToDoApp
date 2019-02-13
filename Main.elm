module Main exposing (..)

import Dict exposing (Dict)

import Html exposing(..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Browser
import List




--MODEL--


type alias Entry =
    { post : String
    , completed : Bool
    , postId : Int
    , toEdit : Bool
    , changeTo : String
    }

-- Model should be Dict Counter Entry
type alias Model=
      { entries : List Entry
      , field : String
      , editField : String
      }


init: () -> (Model, Cmd msg)
init _ =
      (Model [] "" "", Cmd.none)




--UPDATE--


type Msg = Add String
        |  Delete Int
        | Change String
        | UpdateEntry String Int
        | Edit Int
        | UpdateChangeTo String
        | ToggleCompleted Int


update: Msg -> Model -> (Model, Cmd msg)
update msg model =
      case msg of
        Add str ->
            if String.words str == [""] then (model, Cmd.none)
            else  let
                    id = List.length model.entries
                    newEntry = Entry str False (id + 1) False ""
                    newEntries = (::) newEntry model.entries
                in
                    ({ model | entries = newEntries, field = ""}
                    , Cmd.none)

        Delete idToBeDeleted ->
              ({model | entries = List.filter (\e-> e.postId /= idToBeDeleted) model.entries}
              , Cmd.none)

        Change str ->
                ({model | field = str}
                , Cmd.none)

        UpdateEntry str idToBeUpdated ->
              let
                  updateToEdit e =
                      if e.postId == idToBeUpdated then
                        {e | post = model.editField, toEdit=False, completed=False}
                      else
                        e
              in
                ({model | entries = List.map updateToEdit model.entries, field = ""},
                Cmd.none)

        Edit idToBeUpdated ->
              let
                  updateToEdit e =
                      if e.postId == idToBeUpdated then
                        {e | toEdit = True}
                      else
                        e
              in
                ({model | entries = List.map updateToEdit model.entries, field = ""},
                Cmd.none)

        UpdateChangeTo str  ->
              ({model | editField = str}
              , Cmd.none)

        ToggleCompleted idToBeCompleted ->
              let
                  updateToEdit e =
                      if e.postId == idToBeCompleted then
                        let
                            newBool = not e.completed
                        in
                            {e | completed = newBool}
                      else
                        e
              in
                ({model | entries = List.map updateToEdit model.entries, field = ""},
                Cmd.none)







--VIEW--



view: Model -> Html Msg
view model =
    div []
        [h1 []
            [text "TO-DO!"]
            , div []
                  [ addNewBox model
                  , div [] <| List.map renderTodo model.entries
                  ]
        ]



addNewBox: Model -> Html Msg
addNewBox model =
      div []
          [ input
                [ placeholder "What would you like to do?"
                , value model.field
                , onInput Change
                ]
                []
          ,     button [onClick <| Add model.field] [text "Add"]
          ]

renderTodo entry =
          li []
              [ (if entry.toEdit == False then renderTextBox entry else renderEditBox entry)
              ]

renderEditBox entry =
      div []
          [ input
                [ placeholder "Edit?"
                , onInput UpdateChangeTo
                ]
                []
          ,     button [onClick <| UpdateEntry entry.changeTo <| entry.postId] [text "Update"]
          ]

renderTextBox entry =
      div []
            [ (if entry.completed == False then text entry.post else strong [] [text <| "done: " ++ entry.post])
            , button [onClick <| Delete entry.postId] [text "Delete"]
            , button [onClick <| Edit entry.postId ] [text "Edit"]
            , button [onClick <| ToggleCompleted entry.postId] [(if entry.completed == False then text "Done!" else text "Not Done!")]
            ]

--MAIN--
main =
    Browser.element
      { init = init
      , view = view
      , update = update
      , subscriptions = \_ -> Sub.none
      }
