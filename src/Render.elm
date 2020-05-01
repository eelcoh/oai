module Render exposing (Coordinates, Dimensions, Icon(..), Name, color, computeHorizontalText, computeTextHeight, computeTextWidth, computeVerticalText, darkClr, iconGeneric, iconGraph, iconRect, lightClr, pillHeight, roundRect, separatorGraph, toSvgCoordsTuple, viewAnonymousSchema, viewBodyParams, viewBool, viewElms, viewElms_, viewFile, viewFloat, viewInteger, viewMaybeTitle, viewMulti, viewNameGraph, viewPath, viewPathItems, viewPathParams, viewProperties, viewProperty, viewRequest, viewRequestResponse, viewResponse, viewResponses, viewSchema, viewString, ySpace)

import Color exposing (gray)
import Color.Convert
import Dict
import Html exposing (text)
import JsonSchema
import Svg exposing (Svg)
import Svg.Attributes as SvgA exposing (refY)
import Swagger2 as Swagger exposing (Location(..), Parameter(..), Verb(..), decoder)


type alias Coordinates =
    ( Float, Float )


type alias Dimensions =
    ( Float, Float )


ySpace =
    10


pillHeight =
    28


viewPath : JsonSchema.Definitions -> ( Swagger.Path, Swagger.PathItem ) -> Svg msg
viewPath defs ( path, pathItem ) =
    let
        ( pathGraph, ( width, height ) ) =
            roundRect path ( 10, 10 )

        ( pathItemGraph, ( width2, height2 ) ) =
            viewPathItems defs ( width + 10, 10 ) pathItem

        w =
            Basics.max width width2
                |> (+) 10
                |> Basics.toString

        h =
            (height2 + 10)
                |> Basics.toString

        box =
            "0 0 " ++ w ++ " " ++ h

        graph =
            Svg.g [] [ pathGraph, pathItemGraph ]
    in
    Svg.svg
        [ SvgA.width w, SvgA.height h, SvgA.viewBox box ]
        [ graph ]


viewPathItems : JsonSchema.Definitions -> Coordinates -> Swagger.PathItem -> ( Svg msg, Dimensions )
viewPathItems defs coords { get, put, post, delete, patch, options } =
    let
        f ( a, mB ) =
            case mB of
                Just b ->
                    Just ( a, b )

                Nothing ->
                    Nothing

        ops =
            List.filterMap f [ ( "get", get ), ( "put", put ), ( "post", post ), ( "delete", delete ), ( "patch", patch ), ( "options", options ) ]

        ( graphs, ( w, h ) ) =
            viewElms viewRequestResponse defs coords ops

        graph =
            Svg.g [] graphs
    in
    ( graph, ( w, h ) )


viewRequestResponse : JsonSchema.Definitions -> Coordinates -> ( String, Swagger.Operation ) -> ( Svg msg, Dimensions )
viewRequestResponse defs ( x, y ) ( path, { parameters, responses } ) =
    let
        ( pathGraph, ( w1, h1 ) ) =
            roundRect path ( x, y )

        ( paramsGraph, ( widthP, heightP ) ) =
            viewElms viewRequest defs ( w1 + 10, y ) parameters

        responseY =
            Basics.max (heightP + ySpace) (h1 + ySpace + pillHeight)

        ( responseGraph, ( widthR, heightR ) ) =
            viewElms viewResponse defs ( w1 + 10, responseY ) responses

        width =
            Basics.max widthP widthR

        height =
            heightR
    in
    ( Svg.g [] (pathGraph :: (paramsGraph ++ responseGraph)), ( width, heightR ) )


viewElms :
    (JsonSchema.Definitions -> Coordinates -> a -> ( Svg msg, Dimensions ))
    -> JsonSchema.Definitions
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
    (JsonSchema.Definitions -> Coordinates -> a -> ( Svg msg, Dimensions ))
    -> JsonSchema.Definitions
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


viewRequest : JsonSchema.Definitions -> Coordinates -> Swagger.Parameter -> ( Svg msg, Dimensions )
viewRequest defs coords parameter =
    case parameter of
        InBody prms ->
            viewBodyParams defs coords prms

        InOther location prms name ->
            let
                loc =
                    Swagger.locationString location
            in
            viewPathParams defs coords loc prms name


toSvgCoordsTuple ( a, b ) =
    ( Svg.g [] a, b )


type alias Name =
    String


viewAnonymousSchema : JsonSchema.Definitions -> Coordinates -> JsonSchema.PreSchema -> ( Svg msg, Dimensions )
viewAnonymousSchema defs coords schema =
    viewSchema defs coords Nothing schema


viewSchema : JsonSchema.Definitions -> Coordinates -> Maybe Name -> JsonSchema.PreSchema -> ( Svg msg, Dimensions )
viewSchema defs (( x, y ) as coords) name schema =
    case schema of
        JsonSchema.Object { title, properties } ->
            let
                t =
                    Maybe.map (\a -> a ++ " | {..}") name
                        |> Maybe.withDefault "{..}"

                ( objectGraph, ( w, h ) ) =
                    iconRect IObject name coords

                ( propertiesGraphs, newCoords ) =
                    Dict.toList properties
                        |> viewElms viewProperty defs ( w + 10, y )

                graphs =
                    objectGraph :: propertiesGraphs
            in
            ( graphs, newCoords )
                |> toSvgCoordsTuple

        JsonSchema.Array { title, items } ->
            let
                t =
                    Maybe.map (\a -> a ++ " | [..]") name
                        |> Maybe.withDefault "[..]"

                ( arrayGraph, ( w, h ) ) =
                    iconRect IList name coords

                viewMaybeSchema =
                    Maybe.map (viewSchema defs ( w + 10, y ) Nothing) items
            in
            case viewMaybeSchema of
                Nothing ->
                    ( arrayGraph, ( w, h ) )

                Just ( g, c ) ->
                    ( Svg.g [] [ arrayGraph, g ], c )

        JsonSchema.String { title } ->
            viewString coords name

        JsonSchema.Integer { title } ->
            viewInteger coords name

        JsonSchema.Number { title } ->
            viewFloat coords name

        JsonSchema.Boolean { title } ->
            viewBool coords name

        JsonSchema.Null { title } ->
            viewMaybeTitle coords "Null" name

        JsonSchema.Ref { title, ref } ->
            let
                refName =
                    String.dropLeft 14 ref

                rname =
                    "< " ++ refName ++ " >"

                t =
                    Maybe.map (\a -> a ++ " | < " ++ refName ++ " >") name
                        |> Maybe.withDefault rname

                ( iconGraph, ( w, h ) ) =
                    iconRect (IRef refName) name ( x, y )

                refGraph =
                    Dict.get ref defs
                        |> Maybe.map (viewSchema defs ( w + 10, y ) Nothing)
            in
            case refGraph of
                Nothing ->
                    ( iconGraph, ( w, h ) )

                Just ( g, c ) ->
                    ( Svg.g [] [ iconGraph, g ], c )

        JsonSchema.OneOf { title, subSchemas } ->
            viewMulti defs ( x, y ) "|1|" name subSchemas

        JsonSchema.AnyOf { title, subSchemas } ->
            viewMulti defs ( x, y ) "|o|" name subSchemas

        JsonSchema.AllOf { title, subSchemas } ->
            viewMulti defs ( x, y ) "(&)" name subSchemas

        JsonSchema.Fallback _ ->
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


viewProperties defs coords d =
    Dict.toList d
        |> viewElms viewProperty defs coords


viewProperty defs ( x, y ) ( name, schema ) =
    let
        ( schemaGraph, newCoords ) =
            viewSchema defs ( x, y ) (Just name) schema
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
                |> Basics.toString

        rectWidth =
            textWidth + 30

        wRect =
            Basics.toString rectWidth

        wText =
            Basics.toString textWidth

        tt =
            computeVerticalText y
                |> Basics.toString

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
                [ SvgA.x (Basics.toString x)
                , SvgA.y (Basics.toString y)
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
            Basics.toString rectWidth

        bg =
            darkClr
                |> SvgA.fill

        border =
            lightClr
                |> SvgA.stroke

        rct =
            Svg.rect
                [ SvgA.x (Basics.toString x)
                , SvgA.y (Basics.toString y)
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
                    [ SvgA.x (Basics.toString mt)
                    , SvgA.y (Basics.toString tt)
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
            Basics.toString x

        x2 =
            x1

        y1 =
            (y + 5)
                |> Basics.toString

        y2 =
            (y + 28 - 5)
                |> Basics.toString

        strokeColor =
            lightClr
                |> SvgA.stroke

        strokeWidth =
            1.2

        attrs =
            [ SvgA.strokeWidth (Basics.toString strokeWidth)
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
            [ SvgA.x (Basics.toString mt)
            , SvgA.y (Basics.toString tt)
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
