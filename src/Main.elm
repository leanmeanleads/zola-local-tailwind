module Main exposing (Model, Msg(..), Repo, decoder, getRepos, init, main, repoDecoder, sendGet, subscriptions, update, url, view)

import Browser
import Html as Html exposing (..)
import Html.Attributes as Attr exposing (value)
import Html.Events exposing (on, onClick)
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
    }


stageDescriptions : List StageDescription
stageDescriptions =
    [ StageDescription ChooseMaterial "Material"
    , StageDescription SelectArea "Area"
    , StageDescription GetQuote "Quote"
    ]


type MaterialChoice
    = Block
    | Resin
    | Concrete
    | Tarmac


type AreaChoice
    = Small
    | Medium
    | Large


type Stage
    = ChooseMaterial
    | SelectArea
    | GetQuote


type alias Model =
    { query : String
    , repos : List Repo
    , error : Maybe Error
    , stage : Stage
    , material : MaterialChoice
    , area : AreaChoice
    , quote : Range
    }


firstStage : Stage
firstStage =
    ChooseMaterial


init : ( Model, Cmd Msg )
init =
    let
        defaultMaterial =
            Block

        defaultArea =
            Small

        quote =
            calculateQuote defaultMaterial defaultArea
    in
    ( Model "rails" [] Nothing firstStage defaultMaterial defaultArea quote, Cmd.none )


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
    | ChangeToArea AreaChoice
    | ChangeToMaterial MaterialChoice
    | CalculateQuote


indexOfStage : Stage -> Int
indexOfStage currentStage =
    Maybe.withDefault 0 (orderedStages |> ListExtra.elemIndex currentStage)


nextStage : Stage -> Stage
nextStage currentStage =
    Maybe.withDefault firstStage (orderedStages |> ListExtra.getAt (indexOfStage currentStage + 1))


previousStage : Stage -> Stage
previousStage currentStage =
    Maybe.withDefault firstStage (orderedStages |> ListExtra.getAt (indexOfStage currentStage - 1))


type alias Range =
    { start : Int, end : Int }


calculateQuote : MaterialChoice -> AreaChoice -> Range
calculateQuote materialChoice areaChoice =
    case ( materialChoice, areaChoice ) of
        ( Block, Small ) ->
            Range 2100 3000

        ( Block, Medium ) ->
            Range 4200 6000

        ( Block, Large ) ->
            Range 6300 9000

        ( Resin, Small ) ->
            Range 1500 2400

        ( Resin, Medium ) ->
            Range 3000 4800

        ( Resin, Large ) ->
            Range 4500 7200

        ( Concrete, Small ) ->
            Range 2700 4800

        ( Concrete, Medium ) ->
            Range 5000 9500

        ( Concrete, Large ) ->
            Range 8000 14000

        ( Tarmac, Small ) ->
            Range 1200 1800

        ( Tarmac, Medium ) ->
            Range 2400 3600

        ( Tarmac, Large ) ->
            Range 2700 5400


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeToArea choice ->
            ( { model | area = choice, quote = calculateQuote model.material choice }, Cmd.none )

        ChangeToMaterial choice ->
            ( { model | material = choice, quote = calculateQuote choice model.area }, Cmd.none )

        CalculateQuote ->
            ( { model | quote = calculateQuote model.material model.area }, Cmd.none )

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


orderedStages : List Stage
orderedStages =
    [ ChooseMaterial, SelectArea, GetQuote ]


stageCompleteOrActive : Stage -> Stage -> Bool
stageCompleteOrActive currentStage stage =
    let
        currentStageIndex =
            Maybe.withDefault 0 (orderedStages |> ListExtra.elemIndex currentStage)

        stageIndex =
            Maybe.withDefault 0 (orderedStages |> ListExtra.elemIndex stage)
    in
    stageIndex <= currentStageIndex


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
            if stageDescription.stage == ChooseMaterial then
                []

            else if stageCompleteOrActive model.stage stageDescription.stage then
                [ div
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
                [ div
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
            (progressBar
                ++ [ div
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
                   ]
            )
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
        [ Attr.class "w-full py-2 md:py-6"
        ]
        [ div [ Attr.class "flex" ] renderSteps ]


renderChooseMaterial : Model -> Html Msg
renderChooseMaterial model =
    let
        visibleClass =
            if model.stage == ChooseMaterial then
                ""

            else
                "hidden"
    in
    div
        [ Attr.class ("flex flex-wrap " ++ visibleClass)
        ]
        ([ materialSelection model Block "/assets/img/block-driveway.png"
         , materialSelection model Resin "/assets/img/resin-driveway.png"
         , materialSelection model Concrete "/assets/img/concrete-driveway.png"
         , materialSelection model Tarmac "/assets/img/tarmac-driveway.png"
         ]
            ++ actions model
        )


renderGetQuote : Model -> Html Msg
renderGetQuote model =
    let
        visibleClass =
            if model.stage == GetQuote then
                ""

            else
                "hidden"

        quoteAsString =
            "£" ++ String.fromInt model.quote.start ++ " - £" ++ String.fromInt model.quote.end
    in
    div
        [ Attr.class ("flex flex-wrap " ++ visibleClass)
        ]
        [ form
            [ Attr.name "quote"
            , Attr.attribute "data-netlify" "true"
            , Attr.action "/thank-you"
            , Attr.class "w-full flex flex-wrap"
            ]
            ([ div
                [ Attr.class "flex flex-wrap mb-3 w-full"
                ]
                [ div
                    [ Attr.class "flex-auto w-2/5 sm:w-1/5 text-left pr-4 text-xl pl-3" ]
                    [ text "Quote" ]
                , div
                    [ Attr.class "flex-auto w-3/5 sm:w-4/5 text-left text-xl font-bold pl-3 sm:pl-6" ]
                    [ text quoteAsString ]
                ]
             , div
                [ Attr.class "flex flex-wrap mb-6 w-full"
                ]
                [ div
                    [ Attr.class "flex-auto hidden sm:block sm:w-1/5 text-left pr-4 text-xl font-semibold pl-3" ]
                    [ text "" ]
                , div
                    [ Attr.class "flex-auto w-full sm:w-4/5 text-left pl-3 sm:pl-6" ]
                    [ text "This is an approximate quote. Enter contact details below for a more detailed price." ]
                ]
             , div
                [ Attr.class "flex flex-wrap mb-4 w-full"
                ]
                [ label
                    [ Attr.for "name"
                    , Attr.class "inline-block relative flex-grow-0 flex-shrink-0 py-2 px-3 mb-0 w-full leading-normal cursor-default md:flex-shrink-0 md:flex-grow-0 w-1/5 text-slate-800"
                    ]
                    [ text "Name" ]
                , div
                    [ Attr.class "relative flex-grow-0 flex-shrink-0 px-3 w-full md:flex-shrink-0 md:flex-grow-0 w-4/5"
                    ]
                    [ input
                        [ Attr.type_ "text"
                        , Attr.id "name"
                        , Attr.class "block overflow-visible py-2 px-4 m-0 w-full h-10 text-base leading-normal bg-clip-padding rounded border border-gray-300 border-solid cursor-text text-slate-800 focus:border-sky-300 focus:bg-white focus:text-slate-800"
                        , Attr.placeholder "Your Name"
                        , Attr.name "name"
                        , Attr.required True
                        , Attr.attribute "autocomplete" "name"
                        , Attr.attribute "data-form-type" "name"
                        ]
                        []
                    ]
                ]
             , div
                [ Attr.class "flex flex-wrap mb-4 w-full"
                ]
                [ label
                    [ Attr.for "email"
                    , Attr.class "inline-block relative flex-grow-0 flex-shrink-0 py-2 px-3 mb-0 w-full leading-normal cursor-default md:flex-shrink-0 md:flex-grow-0 w-1/5 text-slate-800"
                    ]
                    [ text "Email" ]
                , div
                    [ Attr.class "relative flex-grow-0 flex-shrink-0 px-3 w-full md:flex-shrink-0 md:flex-grow-0 w-4/5"
                    ]
                    [ input
                        [ Attr.type_ "email"
                        , Attr.id "email"
                        , Attr.class "block overflow-visible py-1 px-4 m-0 w-full h-10 text-base leading-normal bg-clip-padding rounded border border-gray-300 border-solid cursor-text text-slate-800 focus:border-sky-300 focus:bg-white focus:text-slate-800"
                        , Attr.placeholder "name@example.com"
                        , Attr.name "email"
                        , Attr.required True
                        , Attr.attribute "autocomplete" "email"
                        ]
                        []
                    ]
                ]
             , div
                [ Attr.class "flex flex-wrap mb-4 w-full"
                ]
                [ label
                    [ Attr.for "phone"
                    , Attr.class "inline-block relative flex-grow-0 flex-shrink-0 py-2 px-3 mb-0 w-full leading-normal cursor-default md:flex-shrink-0 md:flex-grow-0 w-1/5 text-slate-800"
                    ]
                    [ text "Phone" ]
                , div
                    [ Attr.class "relative flex-grow-0 flex-shrink-0 px-3 w-full md:flex-shrink-0 md:flex-grow-0 w-4/5"
                    ]
                    [ input
                        [ Attr.type_ "tel"
                        , Attr.id "phone"
                        , Attr.class "block overflow-visible py-1 px-4 m-0 w-full h-10 text-base leading-normal bg-clip-padding rounded border border-gray-300 border-solid cursor-text text-slate-800 focus:border-sky-300 focus:bg-white focus:text-slate-800"
                        , Attr.placeholder "07812 345678"
                        , Attr.name "phone"
                        , Attr.required True
                        , Attr.attribute "autocomplete" "tel"
                        ]
                        []
                    ]
                ]
             ]
                ++ actions model
            )
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
        ([ areaSelection model Small
         , areaSelection model Medium
         , areaSelection model Large
         ]
            ++ actions model
        )


areaSelection : Model -> AreaChoice -> Html Msg
areaSelection model choice =
    let
        checked =
            if model.area == choice then
                [ Attr.attribute "checked" "checked" ]

            else
                []

        heading =
            case choice of
                Small ->
                    "Small"

                Medium ->
                    "Medium"

                Large ->
                    "Large"

        name =
            "area"

        containerClasses : String
        containerClasses =
            "w-full sm:w-1/3 p-2"

        subheadingElement : AreaChoice -> List (Html Msg)
        subheadingElement elementChoice =
            let
                squared =
                    [ node "sup" [] [ text "2" ] ]
            in
            case elementChoice of
                Small ->
                    [ h3
                        [ Attr.class "text-sm"
                        , Attr.attribute "data-config-id" "auto-txt-11-1"
                        ]
                        (text "10 - 40m" :: squared)
                    ]

                Medium ->
                    [ h3
                        [ Attr.class "text-sm"
                        , Attr.attribute "data-config-id" "auto-txt-11-1"
                        ]
                        (text "40 - 70m" :: squared)
                    ]

                Large ->
                    [ h3
                        [ Attr.class "text-sm"
                        , Attr.attribute "data-config-id" "auto-txt-11-1"
                        ]
                        (text "70 - 120m" :: squared)
                    ]
    in
    div
        [ Attr.class containerClasses
        ]
        [ label
            [ Attr.class ""
            ]
            [ input
                ([ Attr.type_ "radio"
                 , Attr.name name
                 , Attr.class "invisible absolute peer"
                 , Attr.value heading
                 , on "change" (Json.Decode.succeed (ChangeToArea choice))
                 ]
                    ++ checked
                )
                []
            , div
                [ Attr.class "border border-bg-sky-500 rounded-md flex flex-col justify-center items-center px-4 py-4 bg-white drop-shadow-md peer-checked:drop-shadow-none peer-checked:bg-sky-400 peer-checked:text-white"
                ]
                (h2
                    [ Attr.class "text-lg font-medium"
                    , Attr.attribute "data-config-id" "auto-txt-10-1"
                    ]
                    [ text heading ]
                    :: subheadingElement choice
                )
            ]
        ]


materialSelection : Model -> MaterialChoice -> String -> Html Msg
materialSelection model choice urlString =
    let
        checked =
            if model.material == choice then
                [ Attr.attribute "checked" "checked" ]

            else
                []

        heading =
            case choice of
                Block ->
                    "Block Paving"

                Resin ->
                    "Resin"

                Concrete ->
                    "Concrete"

                Tarmac ->
                    "Tarmac"

        name =
            "material"

        containerClasses : String
        containerClasses =
            "w-1/2 sm:w-1/4 p-2"

        imageClasses : String
        imageClasses =
            "mb-4 rounded-full max-h-20"
    in
    div
        [ Attr.class containerClasses
        ]
        [ label
            [ Attr.class ""
            ]
            [ input
                ([ Attr.type_ "radio"
                 , Attr.name name
                 , Attr.class "invisible absolute peer"
                 , Attr.value heading
                 , on "change" (Json.Decode.succeed (ChangeToMaterial choice))
                 ]
                    ++ checked
                )
                []
            , div
                [ Attr.class "border border-bg-sky-500 rounded-md flex flex-col justify-center items-center px-4 py-4 bg-white drop-shadow-md peer-checked:drop-shadow-none peer-checked:bg-sky-400 peer-checked:text-white"
                ]
                [ img
                    [ Attr.class imageClasses
                    , Attr.src urlString
                    , Attr.alt ""
                    , Attr.attribute "data-config-id" "auto-img-2-1"
                    ]
                    []
                , h2
                    [ Attr.class "text-md font-medium text-center"
                    , Attr.attribute "data-config-id" "auto-txt-10-1"
                    ]
                    [ text heading ]
                ]
            ]
        ]


previousButton : Stage -> List (Html Msg)
previousButton _ =
    [ a
        [ Attr.class "flex-1 w-10 overflow-visible my-4 py-2 px-2 ml-0 mr-2 text-xl font-bold leading-normal text-center text-white normal-case align-middle whitespace-nowrap rounded border border-solid cursor-pointer border-gray-500 bg-gray-500 hover:border-gray-600 hover:bg-gray-600 hover:text-white"
        , onClick PreviousStage
        ]
        [ text "Previous" ]
    ]


nextButton : Stage -> List (Html Msg)
nextButton stage =
    case stage of
        GetQuote ->
            [ button
                [ Attr.class "flex-auto w-10 flex-overflow-visible my-4 py-2 px-2 ml-2 mr-0 text-xl font-bold leading-normal text-center text-white normal-case align-middle whitespace-nowrap rounded border border-solid cursor-pointer border-sky-500 bg-sky-500 hover:border-sky-600 hover:bg-sky-600 hover:text-white"
                , Attr.type_ "submit"
                ]
                [ text "Request Quote" ]
            ]

        _ ->
            [ a
                [ Attr.class "flex-auto w-10 flex-overflow-visible my-4 py-2 px-2 ml-2 mr-0 text-xl font-bold leading-normal text-center text-white normal-case align-middle whitespace-nowrap rounded border border-solid cursor-pointer border-sky-500 bg-sky-500 hover:border-sky-600 hover:bg-sky-600 hover:text-white"
                , onClick NextStage
                ]
                [ text "Next" ]
            ]


actions : Model -> List (Html Msg)
actions model =
    previousButton model.stage ++ nextButton model.stage


view : Model -> Html Msg
view model =
    div []
        [ navigation model
        , renderChooseMaterial model
        , renderSelectArea model
        , renderGetQuote model
        ]
