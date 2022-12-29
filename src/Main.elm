module Main exposing (Model, Msg(..), Repo, decoder, getRepos, init, main, repoDecoder, sendGet, subscriptions, update, url, view)

import Browser
import Debug
import Html as Html exposing (..)
import Html.Attributes as Attr exposing (value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, field, int, string)
import List.Extra as ListExtra
import Svg as Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Url.Builder as Url


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Repo =
    { id : Int
    , full_name : String
    }


type alias StageDescription =
    { stage : Stage
    , description : String
    , choices : List Choice
    , index : Int
    }


stageDescriptions : List StageDescription
stageDescriptions =
    [ StageDescription ChooseSurface "Choose Surface" [ Block, Resin, Concrete, Tarmac ] 0
    , StageDescription SelectArea "Select Area" [ Small, Medium, Large, ExtraLarge ] 1
    ]


type Choice
    = Block
    | Resin
    | Concrete
    | Tarmac
    | Small
    | Medium
    | Large
    | ExtraLarge


type Stage
    = ChooseSurface
    | SelectArea


type alias Model =
    { query : String
    , repos : List Repo
    , error : Maybe Error
    , stage : Stage
    , material : Choice
    , area : Choice
    }


init : ( Model, Cmd Msg )
init =
    ( Model "rails" [] Nothing ChooseSurface Block Small, Cmd.none )


decoder : Decoder (List Repo)
decoder =
    Json.Decode.list repoDecoder


repoDecoder : Decoder Repo
repoDecoder =
    Json.Decode.map2 Repo
        (field "id" int)
        (field "full_name" string)


url : String -> String
url query =
    Url.crossOrigin "https://api.github.com" [ "users", query, "repos" ] []


getRepos : String -> Cmd Msg
getRepos query =
    sendGet LoadRepos (url query) decoder


type Msg
    = UpdateQuery String
    | Search
    | LoadRepos (Result Http.Error (List Repo))
    | NextStage
    | PreviousStage
    | ChangeTo Stage Choice


nextStage : Stage -> Stage
nextStage currentStage =
    case currentStage of
        ChooseSurface ->
            SelectArea

        SelectArea ->
            SelectArea


previousStage : Stage -> Stage
previousStage currentStage =
    case currentStage of
        ChooseSurface ->
            ChooseSurface

        SelectArea ->
            ChooseSurface


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTo stage choice ->
            case stage of
                ChooseSurface ->
                    ( { model | material = choice }, Cmd.none )

                SelectArea ->
                    ( { model | area = choice }, Cmd.none )

        NextStage ->
            ( { model | stage = nextStage model.stage }, Cmd.none )

        PreviousStage ->
            ( { model | stage = previousStage model.stage }, Cmd.none )

        UpdateQuery value ->
            ( { model | query = value }, Cmd.none )

        Search ->
            ( model, getRepos model.query )

        LoadRepos maybeRepos ->
            case maybeRepos of
                Ok repos ->
                    ( { model | repos = repos }, Cmd.none )

                Err err ->
                    ( { model | repos = [], error = Just err }, Cmd.none )


sendGet : (Result Error a -> msg) -> String -> Decoder a -> Cmd msg
sendGet msg theUrl theDecoder =
    Http.get theUrl theDecoder
        |> Http.send msg


stageCompleteOrActive : Stage -> Stage -> Bool
stageCompleteOrActive currentStage stage =
    case ( currentStage, stage ) of
        ( ChooseSurface, ChooseSurface ) ->
            True

        ( ChooseSurface, SelectArea ) ->
            False

        ( SelectArea, ChooseSurface ) ->
            True

        ( SelectArea, SelectArea ) ->
            True


renderStep : Model -> StageDescription -> Html Msg
renderStep model stageDescription =
    let
        backgroundClass =
            if stageCompleteOrActive model.stage stageDescription.stage then
                " bg-green-500 "

            else
                " bg-white "

        textClass =
            if stageCompleteOrActive model.stage stageDescription.stage then
                " text-white "

            else
                " text-gray-500 "

        borderClass =
            if stageCompleteOrActive model.stage stageDescription.stage then
                "  "

            else
                " border-2  border-gray-500 "
        progressBar =
             if stageDescription.stage == ChooseSurface then
               []
             else if stageCompleteOrActive model.stage stageDescription.stage then
             [div
                [ Attr.class "absolute flex align-center items-center align-middle content-center"
                , Attr.style "width" "calc(100% - 2.5rem - 1rem)"
                , Attr.style "top" "50%"
                , Attr.style "transform" "translate(-50%, -50%)"
                ]
                [ div [ Attr.class "w-full bg-gray-200 rounded items-center align-middle align-center flex-1" ]
                    [ div [ Attr.class "w-0 bg-green-300 py-1 rounded", Attr.style "width" "100%" ] []
                    ]
                ]
                ]

             else
             [div
                [ Attr.class "absolute flex align-center items-center align-middle content-center"
                , Attr.style "width" "calc(100% - 2.5rem - 1rem)"
                , Attr.style "top" "50%"
                , Attr.style "transform" "translate(-50%, -50%)"
                ]
                [ div [ Attr.class "w-full bg-gray-200 rounded items-center align-middle align-center flex-1" ]
                    [ div [ Attr.class "w-0 bg-green-300 py-1 rounded", Attr.style "width" "0%" ] []
                    ]
                ]
                ]
    in
    div
        [ Attr.class "flex-auto"
        ]
        [ div
            [ Attr.class "relative mb-2"
            ]
            (progressBar ++ [div
                [ Attr.class ("w-10 h-10 mx-auto rounded-full text-lg flex items-center " ++ backgroundClass ++ textClass ++ borderClass)
                ]
                [ span
                    [ Attr.class ("text-center w-full" ++ textClass)
                    ]
                    [ svg
                        [ SvgAttr.class "w-full fill-current"
                        , SvgAttr.viewBox "0 0 24 24"
                        , SvgAttr.width "24"
                        , SvgAttr.height "24"
                        ]
                        [ path
                            [ SvgAttr.class "heroicon-ui"
                            , SvgAttr.d "M5 3h14a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5c0-1.1.9-2 2-2zm14 8V5H5v6h14zm0 2H5v6h14v-6zM8 9a1 1 0 1 1 0-2 1 1 0 0 1 0 2zm0 8a1 1 0 1 1 0-2 1 1 0 0 1 0 2z"
                            ]
                            []
                        ]
                    ]
                ]
            ])
        , div
            [ Attr.class "text-xs text-center md:text-base"
            ]
            [ text stageDescription.description ]
        ]


navigation : Model -> Html Msg
navigation model =
    let
        renderSteps =
            stageDescriptions |> List.map (\sd -> renderStep model sd)
    in
    div
        [ Attr.class "w-full py-6"
        ]
        [ div [ Attr.class "flex" ] renderSteps ]


renderChooseSurface : Model -> Html Msg
renderChooseSurface model =
    let
        visibleClass =
            if model.stage == ChooseSurface then
                ""

            else
                "hidden"
    in
    div
        [ Attr.class ("flex flex-wrap " ++ visibleClass)
        ]
        [ item model ChooseSurface Block "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        , item model ChooseSurface Resin "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        , item model ChooseSurface Concrete "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        , item model ChooseSurface Tarmac "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        ]


renderSelectArea : Model -> Html Msg
renderSelectArea model =
    let
        visibleClass =
            if model.stage == SelectArea then
                ""

            else
                "hidden"
    in
    div
        [ Attr.class ("flex flex-wrap " ++ visibleClass)
        ]
        [ item model SelectArea Small "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        , item model SelectArea Medium "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        , item model SelectArea Large "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        , item model SelectArea ExtraLarge "" "https://7udfuvi8.twic.pics/tree_trimming__lansing__michigan/images/tree_trimming_contractor_for_hire.jpg?twic=v1/cover=200x200"
        ]


item : Model -> Stage -> Choice -> String -> String -> Html Msg
item model itemStage choice subheading urlString =
    let
        checked =
            case itemStage of
                ChooseSurface ->
                    if model.material == choice then
                        [ Attr.attribute "checked" "checked" ]

                    else
                        []

                SelectArea ->
                    if model.area == choice then
                        [ Attr.attribute "checked" "checked" ]

                    else
                        []

        heading =
            case choice of
                Block ->
                    "Block"

                Resin ->
                    "Resin"

                Concrete ->
                    "Concrete"

                Tarmac ->
                    "Tarmac"

                Small ->
                    "Small"

                Medium ->
                    "Medium"

                Large ->
                    "Large"

                ExtraLarge ->
                    "Extra Large"

        name =
            case itemStage of
                ChooseSurface ->
                    "material"

                SelectArea ->
                    "area"
    in
    div
        [ Attr.class "w-1/2 md:w-1/4 p-2"
        ]
        [ label
            [ Attr.class ""
            ]
            [ input
                ([ Attr.type_ "radio"
                 , Attr.name name
                 , Attr.class "invisible absolute peer"
                 , Attr.value heading
                 , onClick (ChangeTo itemStage choice)
                 ]
                    ++ checked
                )
                []
            , div
                [ Attr.class "border border-coolGray-100 rounded-md flex flex-col justify-center items-center px-4 py-4 bg-white peer-checked:bg-indigo-500 peer-checked:text-white"
                ]
                [ img
                    [ Attr.class "mb-4 rounded-full"
                    , Attr.src urlString
                    , Attr.alt ""
                    , Attr.attribute "data-config-id" "auto-img-2-1"
                    ]
                    []
                , h2
                    [ Attr.class "text-sm font-medium"
                    , Attr.attribute "data-config-id" "auto-txt-10-1"
                    ]
                    [ text heading ]
                , h3
                    [ Attr.class "mb-3 text-xs font-medium"
                    , Attr.attribute "data-config-id" "auto-txt-11-1"
                    ]
                    [ text subheading ]
                ]
            ]
        ]


previousButton : Stage -> List (Html Msg)
previousButton _ =
    [ a
        [ Attr.class "flex-auto w-10 overflow-visible my-4 py-2 px-2 ml-0 mr-2 text-xl font-bold leading-normal text-center text-white normal-case align-middle whitespace-nowrap rounded border border-solid cursor-pointer border-gray-500 bg-gray-500 hover:border-gray-600 hover:bg-gray-600 hover:text-white"
        , onClick PreviousStage
        ]
        [ text "Previous" ]
    ]


actions : Model -> Html Msg
actions model =
    let
        prev =
            previousButton model.stage
    in
    div
        [ Attr.class "flex flex-wrap"
        ]
        (prev
            ++ [ a
                    [ Attr.class "flex-auto w-10 flex-overflow-visible my-4 py-2 px-2 ml-2 mr-0 text-xl font-bold leading-normal text-center text-white normal-case align-middle whitespace-nowrap rounded border border-solid cursor-pointer border-sky-500 bg-sky-500 hover:border-sky-600 hover:bg-sky-600 hover:text-white"
                    , onClick NextStage
                    ]
                    [ text "Next" ]
               ]
        )


view : Model -> Html Msg
view model =
    div []
        [ navigation model
        , renderChooseSurface model
        , renderSelectArea model
        , actions model
        , div []
            [ input
                [ onInput UpdateQuery, value model.query ]
                []
            , button [ onClick Search ] [ text "Search" ]
            , div
                []
                (List.map (\{ full_name } -> p [] [ text full_name ]) model.repos)
            , text
                (case model.error of
                    Nothing ->
                        ""

                    Just error ->
                        "Error: " ++ Debug.toString error
                )
            ]
        ]
