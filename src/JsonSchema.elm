module JsonSchema exposing (decoder)

{-| Decoding a JSON Schema to an `JsonSchema.Schema`

@docs decoder

-}

import Debug
import Dict
import Json.Decode exposing (Decoder, andThen, bool, dict, fail, field, float, int, keyValuePairs, lazy, list, map, map2, maybe, nullable, oneOf, string, succeed, value)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Schema as Schema exposing (Schema)


{-| Decoder for a JSON Schema
-}
decoder : Decoder Schema.Model
decoder =
    map2 Schema.Model definitionsDecoder schemaDecoder


definitionsDecoder : Decoder Schema.Definitions
definitionsDecoder =
    field "definitions"
        (keyValuePairs schemaDecoder
            |> map (List.map (Tuple.mapFirst ((++) "#/definitions/")) >> Dict.fromList)
        )
        |> maybe
        |> map (Maybe.withDefault Dict.empty)


schemaDecoder : Decoder Schema
schemaDecoder =
    lazy
        (\_ ->
            oneOf
                [ succeed Schema.object
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "properties" (dict schemaDecoder) Dict.empty
                    |> optional "required" (list string) []
                    |> maybeOptional "minProperties" int
                    |> maybeOptional "maxProperties" int
                    |> optional "examples" (list value) []
                    |> withType "object"
                , succeed Schema.array
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "items" (list schemaDecoder) []
                    |> maybeOptional "minItems" int
                    |> maybeOptional "maxItems" int
                    |> optional "examples" (list value) []
                    |> withType "array"
                , succeed Schema.string
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minLength" int
                    |> maybeOptional "maxLength" int
                    |> maybeOptional "pattern" string
                    |> maybeOptional "format" (string |> map stringFormat)
                    |> maybeOptional "enum" (list string)
                    |> optional "examples" (list value) []
                    |> withType "string"
                    |> map Schema.String
                , succeed Schema.integer
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minimum" int
                    |> maybeOptional "maximum" int
                    |> maybeOptional "enum" (list int)
                    |> optional "examples" (list value) []
                    |> withType "integer"
                , succeed Schema.float
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minimum" float
                    |> maybeOptional "maximum" float
                    |> maybeOptional "enum" (list float)
                    |> optional "examples" (list value) []
                    |> withType "number"
                , succeed Schema.boolean
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "enum" (list bool)
                    |> optional "examples" (list value) []
                    |> withType "boolean"
                , succeed Schema.null
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> withType "null"
                , succeed Schema.reference
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "$ref" string
                    |> optional "examples" (list value) []
                , succeed Schema.baseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "oneOf" (list schemaDecoder)
                    |> optional "examples" (list value) []
                    |> map Schema.OneOf
                , succeed Schema.baseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "anyOf" (list schemaDecoder)
                    |> optional "examples" (list value) []
                    |> map Schema.AnyOf
                , succeed Schema.baseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "allOf" (list schemaDecoder)
                    |> optional "examples" (list value) []
                    |> map Schema.AllOf
                , map Schema.Fallback value
                ]
        )


{-| Ensure a decoder has a specific "type" value.
-}
withType : String -> Decoder a -> Decoder a
withType typeString decoder_ =
    field "type" (constant typeString string)
        |> andThen (always decoder_)


{-| Decode into a specific expected value or fail.
-}
constant : a -> Decoder a -> Decoder a
constant expectedValue decoder_ =
    decoder_
        |> andThen
            (\actualValue ->
                if actualValue == expectedValue then
                    succeed actualValue

                else
                    fail <| "Expected value: " ++ Debug.toString expectedValue ++ " but got value: " ++ Debug.toString actualValue
            )


maybeOptional : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybeOptional key decoder_ =
    optional key (nullable decoder_) Nothing


stringFormat : String -> Schema.StringFormat
stringFormat format =
    case format of
        "date-time" ->
            Schema.DateTime

        "email" ->
            Schema.Email

        "hostname" ->
            Schema.Hostname

        "ipv4" ->
            Schema.Ipv4

        "ipv6" ->
            Schema.Ipv6

        "uri" ->
            Schema.Uri

        _ ->
            Schema.Custom format
