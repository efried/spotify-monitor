port module Main exposing (main)

import Base64.Encode as Base64
import Browser
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Maybe exposing (withDefault)
import OAuth
import OAuth.AuthorizationCode.PKCE as OAuth
import Url exposing (Protocol(..), Url)
import Util exposing (done)



-- MAIN


main : Program (Maybe (List Int)) Model Msg
main =
    Browser.application
        { init = Maybe.andThen convertBytes >> init
        , update = update
        , subscriptions = \_ -> randomBytes GotRandomBytes
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        , view = view
        }


configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = "accounts.spotify.com", path = "/authorize" }
    , tokenEndpoint =
        { defaultHttpsUrl | host = "accounts.spotify.com", path = "/api/token" }
    , currentSongEndpoint =
        { defaultHttpsUrl
            | host = "api.spotify.com"
            , path = "/v1/me/player/currently-playing"
            , query = Just "market=US"
        }
    , currentSongDecoder = spotifySongDecoder
    , clientId =
        "afa91477b2f64ebeabee042dca970f53"
    , scope = [ "user-read-playback-state" ]
    }



-- MODEL


type alias Model =
    { redirectUri : Url
    , auth : Maybe OAuth.Token
    , flow : Flow
    }


type Flow
    = Idle
    | Authorized OAuth.AuthorizationCode OAuth.CodeVerifier
    | Authenticated OAuth.Token
    | Done (Maybe SpotifySong)
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrFailedToConvertBytes
    | ErrAuthorization OAuth.AuthorizationError
    | ErrAuthentication OAuth.AuthenticationError
    | ErrHTTPGetAccessToken
    | ErrHTTPGetCurrentSong


type alias Configuration =
    { authorizationEndpoint : Url
    , tokenEndpoint : Url
    , currentSongEndpoint : Url
    , currentSongDecoder : Json.Decoder SpotifySong
    , clientId : String
    , scope : List String
    }


type alias SpotifySong =
    { name : String
    , albumArts : List AlbumArt
    , artists : List String
    }


type alias AlbumArt =
    { width : Int
    , url : String
    }


init : Maybe { state : String, codeVerifier : OAuth.CodeVerifier } -> Url -> Key -> ( Model, Cmd Msg )
init mflags origin navigationKey =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case OAuth.parseCode origin of
        OAuth.Empty ->
            done { flow = Idle, redirectUri = redirectUri, auth = Nothing }

        OAuth.Success { code, state } ->
            case mflags of
                Nothing ->
                    ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri, auth = Nothing }
                    , clearUrl
                    )

                Just flags ->
                    if state /= Just flags.state then
                        ( { flow = Errored ErrStateMismatch, redirectUri = redirectUri, auth = Nothing }
                        , clearUrl
                        )

                    else
                        ( { flow = Authorized code flags.codeVerifier, redirectUri = redirectUri, auth = Nothing }
                        , Cmd.batch
                            [ getAccessToken configuration redirectUri code flags.codeVerifier
                            , clearUrl
                            ]
                        )

        OAuth.Error error ->
            ( { flow = Errored <| ErrAuthorization error, redirectUri = redirectUri, auth = Nothing }
            , clearUrl
            )



-- UPDATE


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)
    | GotCurrentSong (Result Http.Error SpotifySong)
    | RefreshRequested
    | SignOutRequested


getAccessToken : Configuration -> Url -> OAuth.AuthorizationCode -> OAuth.CodeVerifier -> Cmd Msg
getAccessToken { clientId, tokenEndpoint } redirectUri code codeVerifier =
    OAuth.makeTokenRequest GotAccessToken
        { credentials =
            { clientId = clientId
            , secret = Nothing
            }
        , code = code
        , codeVerifier = codeVerifier
        , url = tokenEndpoint
        , redirectUri = redirectUri
        }
        |> Http.request


getCurrentSong : Configuration -> OAuth.Token -> Cmd Msg
getCurrentSong { currentSongDecoder, currentSongEndpoint } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString currentSongEndpoint
        , expect = Http.expectJson GotCurrentSong currentSongDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


port genRandomBytes : Int -> Cmd msg


port randomBytes : (List Int -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized _ _, GotAccessToken authenticationResponse ) ->
            gotAccessToken model authenticationResponse

        ( _, GotCurrentSong song ) ->
            gotCurrentSong model song

        ( Done _, RefreshRequested ) ->
            case model.auth of
                Nothing ->
                    signOutRequested model

                Just token ->
                    ( model, getCurrentSong configuration token )

        ( Done _, SignOutRequested ) ->
            signOutRequested model

        _ ->
            done model


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
      -- We generate random bytes for both the state and the code verifier. First bytes are
      -- for the 'state', and remaining ones are used for the code verifier.
    , genRandomBytes (cSTATE_SIZE + cCODE_VERIFIER_SIZE)
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    case convertBytes bytes of
        Nothing ->
            done { model | flow = Errored ErrFailedToConvertBytes }

        Just { state, codeVerifier } ->
            let
                authorization =
                    { clientId = configuration.clientId
                    , redirectUri = model.redirectUri
                    , scope = configuration.scope
                    , state = Just state
                    , codeChallenge = OAuth.mkCodeChallenge codeVerifier
                    , url = configuration.authorizationEndpoint
                    }
            in
            ( { model | flow = Idle }
            , authorization
                |> OAuth.makeAuthorizationUrl
                |> Url.toString
                |> Navigation.load
            )


gotAccessToken : Model -> Result Http.Error OAuth.AuthenticationSuccess -> ( Model, Cmd Msg )
gotAccessToken model authenticationResponse =
    case authenticationResponse of
        Err (Http.BadBody body) ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    done { model | flow = Errored <| ErrAuthentication error }

                _ ->
                    done { model | flow = Errored ErrHTTPGetAccessToken }

        Err _ ->
            done { model | flow = Errored ErrHTTPGetAccessToken }

        Ok { token } ->
            ( { model | flow = Authenticated token, auth = Just token }
            , getCurrentSong configuration token
            )


gotCurrentSong : Model -> Result Http.Error SpotifySong -> ( Model, Cmd Msg )
gotCurrentSong model songResponse =
    case songResponse of
        Err (Http.BadBody _) ->
            done { model | flow = Done Nothing }

        Err _ ->
            done { model | flow = Errored ErrHTTPGetCurrentSong }

        Ok song ->
            done { model | flow = Done (Just song) }


signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    ( { model | flow = Idle }
    , Navigation.load (Url.toString model.redirectUri)
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Spotify Monitor"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.gradient { angle = -4, steps = [ spotifyGreen, white ] }
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        ]
    <|
        case model.flow of
            Idle ->
                viewIdle

            Done song ->
                case song of
                    Just currentSong ->
                        viewCurrentSong currentSong

                    Nothing ->
                        viewNoSongPlaying

            Errored err ->
                viewErrored err

            _ ->
                Element.none


viewIdle : Element.Element Msg
viewIdle =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacingXY 0 20
        ]
        [ styledButton SignInRequested "Sign In"
        , Element.image
            [ Element.width (Element.fill |> Element.maximum 200)
            , Element.centerX
            ]
            { src = "/assets/images/Spotify_Logo_RGB_Black.png"
            , description = "Spotify Logo"
            }
        ]


viewCurrentSong : SpotifySong -> Element.Element Msg
viewCurrentSong { name, albumArts, artists } =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacingXY 0 20
        ]
        [ Element.el [ Font.color white ] (Element.text ("Song: " ++ name))
        , Element.el [ Font.color white ]
            (String.join ", " artists
                |> (++) "Artists: "
                |> Element.text
            )
        , pickAlbumArt albumArts
            |> Maybe.map
                (\cover ->
                    Element.image
                        [ Element.centerX
                        , Element.width (Element.fill |> Element.maximum 300)
                        ]
                        { src = cover.url, description = "Album Art" }
                )
            |> withDefault Element.none
        , styledButton RefreshRequested "Reload"
        , styledButton SignOutRequested "Sign Out"
        ]


styledButton : Msg -> String -> Element.Element Msg
styledButton msg label =
    Input.button
        [ Background.color white
        , Border.color spotifyBlack
        , Border.width 2
        , Border.rounded 6
        , Element.centerX
        , Element.paddingXY 20 5
        , Element.width (Element.fill |> Element.maximum 150)
        , Font.center
        ]
        { onPress = Just msg
        , label = Element.text label
        }


viewNoSongPlaying : Element.Element Msg
viewNoSongPlaying =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacingXY 0 20
        ]
        [ Element.el [ Font.color white ] (Element.text "Nothing is playing")
        , Element.image
            [ Element.centerX
            ]
            { src = "/assets/images/static.png", description = "TV Static" }
        , styledButton RefreshRequested "Reload"
        , styledButton SignOutRequested "Sign Out"
        ]


viewErrored : Error -> Element.Element Msg
viewErrored error =
    Element.el
        [ Element.centerX
        , Element.centerY
        , Font.color (Element.rgb 192 0 0)
        ]
        (viewError error)


viewError : Error -> Element.Element Msg
viewError e =
    Element.text <|
        case e of
            ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            ErrFailedToConvertBytes ->
                "Unable to convert bytes to 'state' and 'codeVerifier', this is likely not your fault..."

            ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrAuthentication error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrHTTPGetAccessToken ->
                "Unable to retrieve token: HTTP request failed. CORS is likely disabled on the authorization server."

            ErrHTTPGetCurrentSong ->
                "Unable to retrieve the current song: HTTP request failed."



-- HELPERS


spotifyGreen : Element.Color
spotifyGreen =
    Element.rgb255 30 215 96


spotifyBlack : Element.Color
spotifyBlack =
    Element.rgb255 25 20 20


white : Element.Color
white =
    Element.rgb255 255 255 255


cSTATE_SIZE : Int
cSTATE_SIZE =
    8


cCODE_VERIFIER_SIZE : Int
cCODE_VERIFIER_SIZE =
    32


convertBytes : List Int -> Maybe { state : String, codeVerifier : OAuth.CodeVerifier }
convertBytes bytes =
    if List.length bytes < (cSTATE_SIZE + cCODE_VERIFIER_SIZE) then
        Nothing

    else
        let
            state =
                bytes
                    |> List.take cSTATE_SIZE
                    |> toBytes
                    |> base64

            mCodeVerifier =
                bytes
                    |> List.drop cSTATE_SIZE
                    |> toBytes
                    |> OAuth.codeVerifierFromBytes
        in
        Maybe.map (\codeVerifier -> { state = state, codeVerifier = codeVerifier }) mCodeVerifier


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


albumArtDecoder : Json.Decoder AlbumArt
albumArtDecoder =
    Json.map2 AlbumArt
        (Json.field "width" Json.int)
        (Json.field "url" Json.string)


spotifySongDecoder : Json.Decoder SpotifySong
spotifySongDecoder =
    Json.map3 SpotifySong
        (Json.at [ "item", "name" ] Json.string)
        (Json.field "item" <|
            Json.field "album" <|
                Json.field "images" (Json.list albumArtDecoder)
        )
        (Json.field "item" <|
            Json.field "album" <|
                Json.field "artists" (Json.list (Json.field "name" Json.string))
        )


pickAlbumArt : List AlbumArt -> Maybe AlbumArt
pickAlbumArt albumArts =
    List.filter (\{ width } -> width >= 300) albumArts
        |> List.sortBy .width
        |> List.reverse
        |> List.head
