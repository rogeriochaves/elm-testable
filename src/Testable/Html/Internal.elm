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
    | Class String


type Query
    = Single (List Selector)
    | Multiple (List Selector)
    | Children Query Query


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


attributeValueByName : String -> Node msg -> Result String String
attributeValueByName attributeName node =
    let
        getValue attribute =
            case attribute of
                Property _ value ->
                    Json.decodeValue Json.string value

                _ ->
                    Err ("Error decoding attribute " ++ attributeName ++ " on node " ++ (toString node))

        findByName attribute =
            case attribute of
                Property name _ ->
                    (attributeName == name)

                _ ->
                    False

        findAttribute attributes =
            List.filter findByName attributes
                |> List.head
                |> Maybe.map getValue
                |> Maybe.withDefault (Err ("Could not find attribute named " ++ attributeName ++ " on node " ++ (toString node)))
    in
        case node of
            Node _ attributes _ ->
                findAttribute attributes

            KeyedNode _ attributes _ ->
                findAttribute attributes

            Text _ ->
                Err ("Tried to get value of " ++ attributeName ++ " from a text node, which has no attributes")


classMatches : String -> Attribute msg -> Bool
classMatches expectedValue attribute =
    case attribute of
        Property name value ->
            if (name == "className") then
                Json.decodeValue Json.string value
                    |> Result.map (String.split " " >> List.any ((==) expectedValue))
                    |> Result.withDefault False
            else
                False

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

                Class value ->
                    List.any (classMatches value) attributes
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


findNodes : Query -> Node msg -> List (Node msg)
findNodes query node =
    case query of
        Single selectors ->
            findNodesForSelectors selectors node
                |> List.take 1

        Multiple selectors ->
            findNodesForSelectors selectors node

        Children parentQuery childrenQuery ->
            findNodes parentQuery node
                |> List.concatMap
                    (\node ->
                        getChildren node
                            |> List.concatMap (findNodes childrenQuery)
                    )


getChildren : Node msg -> List (Node msg)
getChildren node =
    case node of
        Node _ _ children ->
            children

        KeyedNode _ _ children ->
            List.map Tuple.second children

        Text _ ->
            []


findNodesForSelectors : List Selector -> Node msg -> List (Node msg)
findNodesForSelectors selectors node =
    let
        nodeMatches =
            List.map (nodeMatchesSelector node) selectors
                |> (::) True
                |> List.all identity

        currentNodeMatches =
            if nodeMatches then
                [ node ]
            else
                []
    in
        case node of
            Node type_ attributes children ->
                List.concatMap (findNodesForSelectors selectors) children
                    |> List.append currentNodeMatches

            KeyedNode type_ attributes children ->
                List.concatMap (Tuple.second >> findNodesForSelectors selectors) children
                    |> List.append currentNodeMatches

            Text _ ->
                if List.isEmpty selectors then
                    [ node ]
                else
                    []


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
