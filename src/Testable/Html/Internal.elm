module Testable.Html.Internal exposing (..)

import Html as PlatformHtml
import Html.Events as PlatformEvents
import Html.Attributes as PlatformAttributes
import Json.Decode as Json
import Json.Encode
import Testable.Html.Selector exposing (Selector(..))


type Node msg
    = Node String (List (Attribute msg)) (List (Node msg))
    | Text String


type Attribute msg
    = Attribute String String
    | Property String Json.Encode.Value
    | Style (List ( String, String ))
    | On String (Json.Decoder msg)
    | OnWithOptions String Options (Json.Decoder msg)


type alias Options =
    { stopPropagation : Bool
    , preventDefault : Bool
    }


toPlatformHtml : Node msg -> PlatformHtml.Html msg
toPlatformHtml node =
    case node of
        Node type_ attributes children ->
            PlatformHtml.node type_ (List.map toPlatformAttribute attributes) (List.map toPlatformHtml children)

        Text value ->
            PlatformHtml.text value


toPlatformAttribute : Attribute msg -> PlatformHtml.Attribute msg
toPlatformAttribute attribute =
    case attribute of
        Attribute name value ->
            PlatformAttributes.attribute name value

        Property name value ->
            PlatformAttributes.property name value

        Style rules ->
            PlatformAttributes.style rules

        On event decoder ->
            PlatformEvents.on event decoder

        OnWithOptions event options decoder ->
            PlatformEvents.onWithOptions event options decoder


nodeText : Node msg -> String
nodeText node =
    case node of
        Node _ _ children ->
            children
                |> List.map (nodeText)
                |> String.join ""

        Text nodeText ->
            nodeText


nodeMatchesSelector : Node msg -> Selector -> Bool
nodeMatchesSelector node selector =
    case node of
        Node type_ attributes children ->
            case selector of
                Tag expectedType ->
                    type_ == expectedType

        Text _ ->
            False


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


findNode : List Selector -> Node msg -> Maybe (Node msg)
findNode query node =
    case node of
        Node type_ attributes children ->
            let
                nodeMatches =
                    List.map (nodeMatchesSelector node) query
                        |> (::) True
                        |> List.all identity
            in
                if nodeMatches then
                    Just node
                else
                    List.map (findNode query) children
                        |> List.filter isJust
                        |> List.head
                        |> Maybe.withDefault Nothing

        Text _ ->
            if List.isEmpty query then
                Just node
            else
                Nothing


isEventWithName : String -> Attribute msg -> Bool
isEventWithName expectedName attribute =
    case attribute of
        On eventName _ ->
            eventName == expectedName

        OnWithOptions eventName _ _ ->
            eventName == expectedName

        _ ->
            False


getMsg : String -> Attribute msg -> Result String msg
getMsg event attribute =
    case attribute of
        On _ decoder ->
            Json.decodeString decoder event

        OnWithOptions _ _ decoder ->
            Json.decodeString decoder event

        _ ->
            Err "This is not an event attribute"


triggerEvent : Node msg -> String -> String -> Result String msg
triggerEvent node name event =
    case node of
        Node _ attributes _ ->
            List.filter (isEventWithName name) attributes
                |> List.head
                |> Maybe.map (getMsg event)
                |> Maybe.withDefault (Err ("Could not find event " ++ name ++ " to be triggered on node " ++ toString node))

        Text _ ->
            Err "Cannot trigger events on text nodes"
