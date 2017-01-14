module Testable.Html.Internal exposing (..)

import Html as PlatformHtml
import Html.Events as PlatformEvents
import Html.Attributes as PlatformAttributes
import Html.Keyed as PlatformKeyed
import Json.Decode as Json
import Json.Encode


type Node msg
    = Node String (List (Attribute msg)) (List (Node msg))
    | KeyedNode String (List (Attribute msg)) (List ( String, Node msg ))
    | Text String


type Attribute msg
    = Property String Json.Encode.Value
    | Style (List ( String, String ))
    | On String (Json.Decoder msg)
    | OnWithOptions String Options (Json.Decoder msg)


type Selector
    = Tag String
    | Attribute String String


type alias Options =
    { stopPropagation : Bool
    , preventDefault : Bool
    }


toPlatformHtml : Node msg -> PlatformHtml.Html msg
toPlatformHtml node =
    case node of
        Node type_ attributes children ->
            PlatformHtml.node type_ (List.map toPlatformAttribute attributes) (List.map toPlatformHtml children)

        KeyedNode type_ attributes children ->
            PlatformKeyed.node type_ (List.map toPlatformAttribute attributes) (List.map (\( key, child ) -> ( key, toPlatformHtml child )) children)

        Text value ->
            PlatformHtml.text value


toPlatformAttribute : Attribute msg -> PlatformHtml.Attribute msg
toPlatformAttribute attribute =
    case attribute of
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

        KeyedNode _ _ children ->
            children
                |> List.map (Tuple.second >> nodeText)
                |> String.join ""

        Text nodeText ->
            nodeText


attributeMatches : String -> String -> Attribute msg -> Bool
attributeMatches expectedName expectedValue attribute =
    case attribute of
        Property name value ->
            (expectedName == name) && (Json.Encode.string expectedValue == value)

        _ ->
            False


nodeMatchesSelector : Node msg -> Selector -> Bool
nodeMatchesSelector node selector =
    let
        checkSelector type_ attributes =
            case selector of
                Tag expectedType ->
                    type_ == expectedType

                Attribute name value ->
                    List.any (attributeMatches name value) attributes
    in
        case node of
            Node type_ attributes children ->
                checkSelector type_ attributes

            KeyedNode type_ attributes children ->
                checkSelector type_ attributes

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
    let
        nodeMatches =
            List.map (nodeMatchesSelector node) query
                |> (::) True
                |> List.all identity
    in
        if nodeMatches then
            Just node
        else
            case node of
                Node type_ attributes children ->
                    List.map (findNode query) children
                        |> List.filter isJust
                        |> List.head
                        |> Maybe.withDefault Nothing

                KeyedNode type_ attributes children ->
                    List.map (Tuple.second >> findNode query) children
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
    let
        findAttributeAndTrigger attributes =
            List.filter (isEventWithName name) attributes
                |> List.head
                |> Maybe.map (getMsg event)
                |> Maybe.withDefault (Err ("Could not find event " ++ name ++ " to be triggered on node " ++ toString node))
    in
        case node of
            Node _ attributes _ ->
                findAttributeAndTrigger attributes

            KeyedNode _ attributes _ ->
                findAttributeAndTrigger attributes

            Text _ ->
                Err "Cannot trigger events on text nodes"
