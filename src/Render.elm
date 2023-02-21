module Render exposing (Coordinates, Dimensions, Icon(..), Name, color, computeHorizontalText, computeTextHeight, computeTextWidth, computeVerticalText, darkClr, iconGeneric, iconGraph, iconRect, lightClr, pillHeight, roundRect, separatorGraph, toSvgCoordsTuple, viewAnonymousSchema, viewBodyParams, viewBool, viewElms, viewElms_, viewFile, viewFloat, viewInteger, viewMaybeTitle, viewMulti, viewNameGraph, viewPathParams, viewProperty, viewResponse, viewResponses, viewSchema, viewString, ySpace)

import Color exposing (gray)
import Color.Convert
import Dict
import Html exposing (text)
import Json.Schema as Schema
import JsonSchema
import List.Extra
import Svg exposing (Svg)
import Svg.Attributes as SvgA exposing (refY)



-- import Swagger2 as Swagger exposing (Location(..), Parameter(..), Verb(..), decoder)


type alias Coordinates =
    ( Float, Float )


type alias Dimensions =
    ( Float, Float )


ySpace =
    10


pillHeight =
    28


viewElms :
    (Schema.Definitions -> Coordinates -> a -> ( Svg msg, Dimensions ))
    -> Schema.Definitions
    -> Coordinates
    -> List a
    -> ( List (Svg msg), Coordinates )
viewElms fn defs coords elms =
    let
        ( g, ( _, h ), w ) =
            viewElms_ fn defs coords elms
    in
    ( g, ( w, h ) )


viewElms_ :
    (Schema.Definitions -> Coordinates -> a -> ( Svg msg, Dimensions ))
    -> Schema.Definitions
    -> Coordinates
    -> List a
    -> ( List (Svg msg), Coordinates, Float )
viewElms_ fn defs (( x, y ) as coords) elms =
    case elms of
        [] ->
            ( [], coords, x )

        element :: elements ->
            let
                ( g, ( w1, h1 ) ) =
                    fn defs coords element

                ( gs, ( w2, h2 ), w3 ) =
                    viewElms_ fn defs ( x, h1 + 10 ) elements

                maxW =
                    List.foldl Basics.max w1 [ w1, w2, w3 ]
            in
            ( g :: gs, ( x, h2 ), maxW )


toSvgCoordsTuple ( a, b ) =
    ( Svg.g [] a, b )


type alias Name =
    String


viewAnonymousSchema : Schema.Definitions -> Coordinates -> Schema.Schema -> ( Svg msg, Dimensions )
viewAnonymousSchema defs coords schema =
    viewSchema defs coords Nothing schema


viewSchema : Schema.Definitions -> Coordinates -> Maybe Name -> Schema.Schema -> ( Svg msg, Dimensions )
viewSchema defs (( x, y ) as coords) name schema =
    case schema of
        Schema.Object { title, properties } ->
            let
                t =
                    Maybe.map (\a -> a ++ " | {..}") name
                        |> Maybe.withDefault "{..}"

                ( objectGraph, ( w, h ) ) =
                    iconRect IObject name coords

                ( propertiesGraphs, newCoords ) =
                    viewElms viewProperty defs ( w + 10, y ) properties

                graphs =
                    objectGraph :: propertiesGraphs
            in
            ( graphs, newCoords )
                |> toSvgCoordsTuple

        Schema.Array { title, items } ->
            let
                t =
                    Maybe.map (\a -> a ++ " | [..]") name
                        |> Maybe.withDefault "[..]"

                ( arrayGraph, ( w, h ) ) =
                    iconRect IList name coords

                ( itemsGraphs, newCoords ) =
                    viewElms viewArrayItem defs ( w + 10, y ) items

                graphs =
                    arrayGraph :: itemsGraphs

                -- viewMaybeSchema =
                --     -- List.map (viewSchema defs ( w + 10, y ) Nothing) items
                --     List.Extra.scanl (\g ( _, y_ ) -> viewSchema defs ( w_, y_ + 10 ) Nothing) items
            in
            ( graphs, newCoords )
                |> toSvgCoordsTuple

        -- case viewMaybeSchema of
        --     Nothing ->
        --         ( arrayGraph, ( w, h ) )
        --     Just ( g, c ) ->
        --         ( Svg.g [] [ arrayGraph, g ], c )
        Schema.String { title } ->
            viewString coords name

        Schema.Integer { title } ->
            viewInteger coords name

        Schema.Number { title } ->
            viewFloat coords name

        Schema.Boolean { title } ->
            viewBool coords name

        Schema.Null { title } ->
            viewMaybeTitle coords "Null" name

        Schema.Ref { title, ref } ->
            let
                refName =
                    String.dropLeft 14 ref

                rname =
                    "< " ++ refName ++ " >"

                t =
                    Maybe.map (\a -> a ++ " | < " ++ refName ++ " >") name
                        |> Maybe.withDefault rname

                ( iconGraph_, ( w, h ) ) =
                    iconRect (IRef refName) name ( x, y )

                refGraph =
                    Dict.get ref defs
                        |> Maybe.map (viewSchema defs ( w + 10, y ) Nothing)
            in
            case refGraph of
                Nothing ->
                    ( iconGraph_, ( w, h ) )

                Just ( g, c ) ->
                    ( Svg.g [] [ iconGraph_, g ], c )

        Schema.OneOf { title, subSchemas } ->
            viewMulti defs ( x, y ) "|1|" name subSchemas

        Schema.AnyOf { title, subSchemas } ->
            viewMulti defs ( x, y ) "|o|" name subSchemas

        Schema.AllOf { title, subSchemas } ->
            viewMulti defs ( x, y ) "(&)" name subSchemas

        Schema.Fallback _ ->
            ( Svg.g [] [], coords )


viewMulti defs ( x, y ) icon name schemas =
    let
        ( choiceGraph, ( w, h ) ) =
            roundRect icon ( x, y )

        ( subSchemaGraphs, newCoords ) =
            viewElms viewAnonymousSchema defs ( w + 10, y ) schemas

        allOfGraph =
            choiceGraph :: subSchemaGraphs
    in
    ( allOfGraph, newCoords )
        |> toSvgCoordsTuple


viewMaybeTitle : Coordinates -> String -> Maybe String -> ( Svg msg, Dimensions )
viewMaybeTitle coords s mTitle =
    let
        mkTitle t =
            t ++ " : " ++ s

        title =
            Maybe.map mkTitle mTitle
                |> Maybe.withDefault s
    in
    roundRect title coords


viewBodyParams defs coords { name, schema } =
    viewPathParams defs coords "body" schema name


viewPathParams defs ( x, y ) location schema name =
    let
        ( locGraph, ( w, h ) ) =
            roundRect (location ++ " ::") ( x, y )

        ( schemaGraphs, newCoords ) =
            viewSchema defs ( w + 10, y ) (Just name) schema

        fullGraph =
            [ locGraph, schemaGraphs ]
    in
    ( fullGraph, newCoords )
        |> toSvgCoordsTuple


viewFile coords name =
    iconRect IFile name coords


viewBool coords name =
    iconRect IBool name coords


viewFloat coords name =
    iconRect IFloat name coords


viewInteger coords name =
    iconRect IInt name coords


viewString coords name =
    iconRect IStr name coords


viewResponses defs coords responses =
    viewElms viewResponse defs coords responses


viewResponse defs ( x, y ) ( code, { description, schema } ) =
    let
        ( codeGraph, ( w, h ) ) =
            roundRect code ( x, y )

        schemaView =
            Maybe.map (viewAnonymousSchema defs ( w + 10, y )) schema
    in
    case schemaView of
        Nothing ->
            ( codeGraph, ( w, h ) )

        Just ( schemaGraph, newCoords ) ->
            let
                graph =
                    Svg.g [] [ codeGraph, schemaGraph ]
            in
            ( graph, newCoords )


viewProperty defs coords objectProperty =
    let
        ( name, property ) =
            case objectProperty of
                Schema.Required name_ property_ ->
                    ( name_, property_ )

                Schema.Optional name_ property_ ->
                    ( name_, property_ )

        ( schemaGraph, newCoords ) =
            viewSchema defs coords (Just name) property
    in
    ( Svg.g [] [ schemaGraph ], newCoords )


viewArrayItem defs coords schema =
    let
        ( schemaGraph, newCoords ) =
            viewSchema defs coords Nothing schema
    in
    ( Svg.g [] [ schemaGraph ], newCoords )


roundRect : String -> Coordinates -> ( Svg msg, Dimensions )
roundRect txt ( x, y ) =
    let
        l =
            String.length txt
                |> Basics.toFloat

        charWidth =
            7.2

        textWidth =
            computeTextWidth txt

        mt =
            computeHorizontalText x txt
                |> String.fromFloat

        rectWidth =
            textWidth + 30

        wRect =
            String.fromFloat rectWidth

        wText =
            String.fromFloat textWidth

        tt =
            computeVerticalText y
                |> String.fromFloat

        bg =
            darkClr
                |> SvgA.fill

        fg =
            lightClr
                |> SvgA.fill

        border =
            lightClr
                |> SvgA.stroke

        caption c =
            let
                attrs =
                    [ SvgA.x mt
                    , SvgA.y tt
                    , fg
                    , SvgA.fontFamily "Monospace"
                    , SvgA.fontSize "12"
                    , SvgA.dominantBaseline "middle"
                    , SvgA.cursor "pointer"
                    ]
            in
            Svg.text_
                attrs
                [ Svg.text c ]

        rct =
            Svg.rect
                [ SvgA.x (String.fromFloat x)
                , SvgA.y (String.fromFloat y)
                , SvgA.width wRect
                , SvgA.height "28"
                , bg
                , border
                , SvgA.strokeWidth "0.2"
                , SvgA.rx "2"
                , SvgA.ry "2"
                ]
                []

        el =
            Svg.g [ SvgA.textAnchor "middle" ]
                [ rct, caption txt ]
    in
    ( el, ( rectWidth + x, 28 + y ) )


type Icon
    = IList
    | IObject
    | IInt
    | IStr
    | IFloat
    | IFile
    | IBool
    | INull
    | IRef String


iconRect : Icon -> Maybe String -> Coordinates -> ( Svg msg, Dimensions )
iconRect icon txt ( x, y ) =
    let
        space =
            10

        ( iconG, ( iconW, _ ) ) =
            iconGraph icon ( x + space, y )

        ( separatorG, ( separatorW, _ ) ) =
            separatorGraph ( iconW + space, y )

        mNameG =
            Maybe.map (viewNameGraph ( separatorW + space, y )) txt

        ( graphs, rectWidth ) =
            case mNameG of
                Nothing ->
                    ( [ iconG ], iconW - x + space )

                Just ( nameG, ( nameW, _ ) ) ->
                    ( [ iconG, separatorG, nameG ], nameW - x + space )

        wRect =
            String.fromFloat rectWidth

        bg =
            darkClr
                |> SvgA.fill

        border =
            lightClr
                |> SvgA.stroke

        rct =
            Svg.rect
                [ SvgA.x (String.fromFloat x)
                , SvgA.y (String.fromFloat y)
                , SvgA.width wRect
                , SvgA.height "28"
                , bg
                , border
                , SvgA.strokeWidth "0.2"
                , SvgA.rx "2"
                , SvgA.ry "2"
                ]
                []

        el =
            Svg.g [ SvgA.textAnchor "middle" ]
                (rct :: graphs)
    in
    ( el, ( rectWidth + x, 28 + y ) )


viewNameGraph : Coordinates -> String -> ( Svg msg, Dimensions )
viewNameGraph ( x, y ) name =
    let
        tt =
            computeVerticalText y

        mt =
            computeHorizontalText x name

        fullWidth =
            computeTextWidth name

        fg =
            lightClr
                |> SvgA.fill

        caption c =
            let
                attrs =
                    [ SvgA.x (String.fromFloat mt)
                    , SvgA.y (String.fromFloat tt)
                    , fg
                    , SvgA.fontFamily "Monospace"
                    , SvgA.fontSize "12"
                    , SvgA.fontWeight "700"
                    , SvgA.dominantBaseline "middle"
                    , SvgA.cursor "pointer"
                    ]
            in
            Svg.text_
                attrs
                [ Svg.text c ]

        graph =
            caption name

        dims =
            ( x + fullWidth, y + 28 )
    in
    ( graph, dims )


separatorGraph : Coordinates -> ( Svg msg, Dimensions )
separatorGraph ( x, y ) =
    let
        x1 =
            String.fromFloat x

        x2 =
            x1

        y1 =
            (y + 5)
                |> String.fromFloat

        y2 =
            (y + 28 - 5)
                |> String.fromFloat

        strokeColor =
            lightClr
                |> SvgA.stroke

        strokeWidth =
            1.2

        attrs =
            [ SvgA.strokeWidth (String.fromFloat strokeWidth)
            , strokeColor
            , SvgA.strokeLinecap "Round"
            , SvgA.x1 x1
            , SvgA.y1 y1
            , SvgA.x2 x2
            , SvgA.y2 y2
            ]

        separator =
            Svg.line attrs []

        dims =
            ( x + strokeWidth, y + 28 )
    in
    ( separator, dims )


iconGraph : Icon -> Coordinates -> ( Svg msg, Dimensions )
iconGraph icon coords =
    case icon of
        IList ->
            iconGeneric coords "[..]"

        IObject ->
            iconGeneric coords "{..}"

        IInt ->
            viewNameGraph coords "I"

        IStr ->
            iconGeneric coords "S"

        IFile ->
            iconGeneric coords "File"

        INull ->
            iconGeneric coords "Null"

        IBool ->
            iconGeneric coords "B"

        IFloat ->
            iconGeneric coords "F"

        IRef s ->
            iconGeneric coords ("& " ++ s)


iconGeneric : Coordinates -> String -> ( Svg msg, Dimensions )
iconGeneric ( x, y ) iconStr =
    let
        strWidth =
            computeTextWidth iconStr

        strHeight =
            computeTextHeight iconStr

        mt =
            computeHorizontalText x iconStr

        tt =
            computeVerticalText y

        fg =
            lightClr
                |> SvgA.fill

        attrs =
            [ SvgA.x (String.fromFloat mt)
            , SvgA.y (String.fromFloat tt)
            , fg
            , SvgA.fontFamily "Monospace"
            , SvgA.fontSize "12"
            , SvgA.dominantBaseline "middle"
            , SvgA.cursor "pointer"
            ]

        graph =
            Svg.text_
                attrs
                [ Svg.text iconStr ]

        dims =
            ( x + strWidth, y + strHeight )
    in
    ( graph, dims )


computeTextWidth txt =
    String.length txt
        |> Basics.toFloat
        |> (*) 7.2


computeTextHeight txt =
    28


computeHorizontalText x txt =
    let
        textWidth =
            computeTextWidth txt
    in
    x + (textWidth / 2)


computeVerticalText y =
    y + 15


color r g b =
    Color.rgb r g b
        |> Color.Convert.colorToHex


lightClr =
    color 230 230 230


darkClr =
    color 57 114 206



--
