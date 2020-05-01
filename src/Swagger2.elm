module Swagger2 exposing (BodyParameters, Endpoint, Endpoints, Location(..), Name, Operation, Parameter(..), Path, PathItem, Paths, Response, Responses, Spec, Verb(..), constant, decoder, locationString, maybeOptional, operationDecoder, parameterDecoder, pathItemDecoder, pathItemDecoder_, pathsDecoder, responseDecoder)

--import Decode exposing (Schema, constant)

import Dict
import Json.Decode exposing (Decoder, andThen, bool, dict, fail, field, float, int, keyValuePairs, list, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (decode, optional, required)
import JsonSchema exposing (Definitions, PreSchema(..), decoder, definitionsDecoder)


type alias Spec =
    { paths : Paths
    , definitions : Definitions
    }


type alias Path =
    String


type alias Paths =
    List ( Path, PathItem )


type alias Endpoints =
    List Endpoint


type Verb
    = GET
    | PUT
    | POST
    | DELETE
    | PATCH
    | OPTIONS


type alias PathItem =
    { get : Maybe Operation
    , put : Maybe Operation
    , post : Maybe Operation
    , delete : Maybe Operation
    , patch : Maybe Operation
    , options : Maybe Operation
    , ref : Maybe String
    , parameters : Maybe Parameter
    }


type alias Endpoint =
    ( Verb, Operation )


type alias Operation =
    { parameters : List Parameter
    , responses : Responses
    }


type alias Responses =
    List ( String, Response )


type alias Response =
    { description : String
    , schema : Maybe PreSchema
    }


type alias Name =
    String


type Location
    = Body
    | Header
    | Path
    | Query
    | FormData


type Parameter
    = InBody BodyParameters
    | InOther Location PreSchema Name


type alias BodyParameters =
    { name : String
    , description : Maybe String
    , required : Maybe Bool
    , schema : PreSchema
    }


decoder : Decoder Spec
decoder =
    Json.Decode.map2 Spec
        (field "paths" pathsDecoder)
        (field "definitions"
            (keyValuePairs JsonSchema.preSchemaDecoder
                |> Json.Decode.map (List.map (Tuple.mapFirst ((++) "#/definitions/")) >> Dict.fromList)
            )
            |> Json.Decode.maybe
            |> Json.Decode.map (Maybe.withDefault Dict.empty)
        )



{-

   type alias Path =
       String


   type alias Paths =
       List ( Path, List Endpoint )


   type alias Endpoints =
       List Endpoint

   type alias Endpoint =
       ( Verb, Operation )


-}


pathsDecoder : Decoder Paths
pathsDecoder =
    keyValuePairs pathItemDecoder


pathItemDecoder_ : Decoder PathItem
pathItemDecoder_ =
    Json.Decode.map8 PathItem
        (Json.Decode.maybe (Json.Decode.field "get" operationDecoder))
        (Json.Decode.maybe (Json.Decode.field "put" operationDecoder))
        (Json.Decode.maybe (Json.Decode.field "post" operationDecoder))
        (Json.Decode.maybe (Json.Decode.field "delete" operationDecoder))
        (Json.Decode.maybe (Json.Decode.field "patch" operationDecoder))
        (Json.Decode.maybe (Json.Decode.field "options" operationDecoder))
        (Json.Decode.maybe (Json.Decode.field "$ref" string))
        (Json.Decode.maybe (Json.Decode.field "parameters" parameterDecoder))


pathItemDecoder : Decoder PathItem
pathItemDecoder =
    decode PathItem
        |> maybeOptional "get" operationDecoder
        |> maybeOptional "put" operationDecoder
        |> maybeOptional "post" operationDecoder
        |> maybeOptional "delete" operationDecoder
        |> maybeOptional "patch" operationDecoder
        |> maybeOptional "options" operationDecoder
        |> maybeOptional "$ref" string
        |> maybeOptional "parameters" parameterDecoder


operationDecoder : Decoder Operation
operationDecoder =
    decode Operation
        |> required "parameters" (list parameterDecoder)
        |> required "responses" (keyValuePairs responseDecoder)


responseDecoder : Decoder Response
responseDecoder =
    decode Response
        |> required "description" string
        |> maybeOptional "schema" JsonSchema.preSchemaDecoder


parameterDecoder : Decoder Parameter
parameterDecoder =
    let
        withIn : String -> Decoder a -> Decoder a
        withIn typeString decoder =
            field "in" (constant typeString string)
                |> andThen (always decoder)

        withType : String -> Decoder a -> Decoder a
        withType typeString decoder =
            field "type" (constant typeString string)
                |> andThen (always decoder)
    in
    Json.Decode.lazy
        (\_ ->
            Json.Decode.oneOf
                [ decode BodyParameters
                    |> required "name" string
                    |> maybeOptional "description" string
                    |> maybeOptional "required" bool
                    |> required "schema" JsonSchema.preSchemaDecoder
                    |> withIn "body"
                    |> Json.Decode.map InBody
                , Json.Decode.map (InOther Path) JsonSchema.preSchemaDecoder
                    |> required "name" string
                    |> withIn "path"
                , Json.Decode.map (InOther Header) JsonSchema.preSchemaDecoder
                    |> required "name" string
                    |> withIn "header"
                , Json.Decode.map (InOther Query) JsonSchema.preSchemaDecoder
                    |> required "name" string
                    |> withIn "query"
                , Json.Decode.map (InOther FormData) JsonSchema.preSchemaDecoder
                    |> required "name" string
                    |> withIn "formData"
                ]
        )


maybeOptional : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybeOptional key decoder =
    optional key (nullable decoder) Nothing


constant : a -> Decoder a -> Decoder a
constant expectedValue decoder =
    decoder
        |> andThen
            (\actualValue ->
                if actualValue == expectedValue then
                    succeed actualValue

                else
                    fail <| "Expected value: " ++ toString expectedValue ++ " but got value: " ++ toString actualValue
            )


locationString : Location -> String
locationString location =
    case location of
        Body ->
            "Body"

        Query ->
            "Query"

        Header ->
            "Header"

        FormData ->
            "Form"

        Path ->
            "Path"
