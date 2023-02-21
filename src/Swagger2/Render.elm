module Swagger2.Render exposing (..)


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
                |> String.fromFloat

        h =
            (height2 + 10)
                |> String.fromFloat

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
