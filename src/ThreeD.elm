port module ThreeD exposing (..)

import Random
import Angle
import Camera3d
import Length exposing (Meters, meters)
import Color
import Direction3d exposing (Direction3d)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Length
import Pixels
import Point3d exposing (Point3d)
import Sphere3d
import Cylinder3d
import Triangle3d
import Axis3d
import Vector3d exposing (Vector3d)
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d
import Browser
import Browser.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Random
import Time

main = Browser.element {init = init
                        ,update = update
                        ,view = view
                        ,subscriptions = subscriptions}


-- send
port join : String -> Cmd msg
port moved : Player  ->  Cmd msg

-- receive
port skywayId : ({id:String, num:Int} -> msg) -> Sub msg
port othersMove : (Player -> msg) -> Sub msg
       
type WorldCoordinates = WorldCoordinates

type alias Player = {id: Maybe String
                    ,name: String
                    ,x: Float
                    ,y: Float
                    ,theta: Float
                    ,oni: Bool
                    }
    
type alias Model = {me: Player
                   ,others: List Player
                   ,room: String
                   ,name: String
                   ,host: Bool
                   }
    
type Msg = KeyPressed Direction
         | OthersMoved Player
         | IdDefined {id:String, num:Int}
         | RoomChanged String
         | NameChanged String
         | Join
         | RandomPlayerGenerated Player
           
type Direction = Left
               | Right
               | Other
               | Forward

init: () -> (Model, Cmd Msg)
init _ =
    ({me = {id=Nothing,name="",x=5,y=5,theta=0,oni=False}
     ,room = ""
     ,name = ""
     ,host = False
     ,others = []
     }
    ,Random.generate RandomPlayerGenerated randomPlayer)

randomPlayer: Random.Generator Player
randomPlayer =
    Random.map3
        (\x y theta ->
             {x=x
             ,y=y
             ,theta=theta
             ,id=Nothing
             ,oni=False
             ,name=""
             }
        )
        (Random.float -10 10) 
        (Random.float -10 10) 
        (Random.float 0 (2*pi)) 

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RandomPlayerGenerated player ->
            ({model | me = player}
            ,Cmd.none
            )
        RoomChanged room ->
            ( { model | room = room }
            , Cmd.none
            )
        NameChanged name ->
            ( { model | name = name }
            , Cmd.none
            )
        Join ->
            ( model
            , (join model.room)
            )
        IdDefined info ->
            let
                setId: Player  -> String -> Player
                setId player id = {player|id = Just id}
            in
                ({model | me = setId model.me info.id
                 ,host = (info.num == 1)
                 }
                ,Cmd.none
                )
        KeyPressed dir ->
            let
                dummy = Debug.log "" dir
            in
                case dir of
                    Left -> ({model| me = turnLeft model.me}
                            , Cmd.none)
                    Right -> ({model| me = turnRight model.me}
                             , Cmd.none)
                    Forward -> ({model| me = moveForward model.me}
                             , Cmd.none)
                    _ -> (model, Cmd.none)
        OthersMoved other ->
            let
                players = other::(List.filter (\player -> player.id /= other.id) model.others)
            in
                ({model|others=players}, Cmd.none)

turnLeft: Player -> Player
turnLeft p = {p|theta=p.theta+(3*pi/180)}          

turnRight: Player -> Player
turnRight p = {p|theta=p.theta-(3*pi/180)}          

moveForward: Player -> Player
moveForward p =
    let
        newX = p.x + 0.5*(cos p.theta)
        newY = p.y + 0.5*(sin p.theta)
    in
        {p| x = newX, y = newY}

view: Model -> Html Msg
view model =
    div [align "center"]
    (case model.me.id of
         Nothing -> [input
                         [ type_ "text"
                         , placeholder "Room"
                         , onInput RoomChanged
                         , on "keydown" (ifIsEnter Join)
                         , value model.room
                         ]
                         []
                    ,input
                         [ type_ "text"
                         , placeholder "Name"
                         , onInput NameChanged
                         , value model.me.name
                         ]
                         []
                    ,button [onClick Join] [text "Join"]
                    ]
         Just id ->
             let
                 material =
                     Material.nonmetal
                         { baseColor = Color.blue
                         , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                         }
                 materialWhite =
                     Material.nonmetal
                         { baseColor = Color.white
                         , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                         }

                 materialBlack =
                     Material.nonmetal
                         { baseColor = Color.black
                         , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                         }
                
                 materialGreen =
                     Material.nonmetal
                         { baseColor = Color.green
                         , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                         }
                 materialDart =
                     Material.nonmetal
                         { baseColor = Color.rgba 0.4 0.6 0.3 0.8
                         , roughness = 0.1 -- varies from 0 (mirror-like) to 1 (matte)
                         }

                 plane = Scene3d.quad materialDart
                         (Point3d.meters 1000 1000 0)
                         (Point3d.meters -1000 1000 0)
                         (Point3d.meters -1000 -1000 0)
                         (Point3d.meters 1000 -1000 0)

                 cyl = Scene3d.cylinder material
                       <| Cylinder3d.along Axis3d.z
                           { start = Length.meters 0
                           , end = Length.meters 1.5
                           , radius = Length.meters 1
                           }
                  
                 left = Scene3d.sphere materialWhite
                        <| Sphere3d.atPoint
                            (Point3d.meters
                                 (0.8*(cos (model.me.theta+(-pi/10))))
                                 (0.8*(sin (model.me.theta+(-pi/10))))
                                 1
                            )
                            (Length.meters 0.3)
                 right = Scene3d.sphere materialWhite
                         <| Sphere3d.atPoint
                             (Point3d.meters
                                  (0.8*(cos (model.me.theta+(pi/10))))
                                  (0.8*(sin (model.me.theta+(pi/10))))
                                  1
                             )
                             (Length.meters 0.3)
                 lb = Scene3d.sphere materialBlack
                      <| Sphere3d.atPoint
                          (Point3d.meters
                               (0.9*(cos (model.me.theta-(pi/10))))
                               (0.9*(sin (model.me.theta-(pi/10))))
                               1
                          )
                          (Length.meters 0.22)
                 rb = Scene3d.sphere materialBlack
                      <| Sphere3d.atPoint
                          (Point3d.meters
                               (0.9*(cos (model.me.theta+(pi/10))))
                               (0.9*(sin (model.me.theta+(pi/10))))
                               1
                          )
                          (Length.meters 0.22)

                 robot = Scene3d.group [cyl,left,right,lb,rb]
                    
                 -- Define a camera as usual
                 camera =
                     let
                         ex = model.me.x
                         ey = model.me.y
                         fx = ex + 5*(cos model.me.theta)
                         fy = ey + 5*(sin model.me.theta)
                     in
                         Camera3d.perspective
                             { viewpoint =
                                   Viewpoint3d.lookAt
                                   { focalPoint = (Point3d.meters fx fy 1.0)
                                   , eyePoint = (Point3d.meters ex ey 1.0 )
                                   , upDirection = Direction3d.positiveZ
                                   }
                             , verticalFieldOfView = Angle.degrees 40
                             }
                 relativePos event =
                     {x=Tuple.first event.pointer.offsetPos
                     ,y=Tuple.second event.pointer.offsetPos}
             in
                 [Scene3d.sunny
                      { camera = camera
                      , clipDepth = Length.centimeters 0.5
                      , dimensions = ( Pixels.int 1000, Pixels.int 1000 )
                      , background = Scene3d.transparentBackground
                      , entities = [plane,robot]  
                      , shadows = True
                      , upDirection = Direction3d.z
                      , sunlightDirection = Direction3d.yz (Angle.degrees -120)
                      }
                 ]
         )
        
keyDecoder : D.Decoder Msg
keyDecoder =
  D.map toDirection (D.field "key" D.string)

toDirection : String -> Msg
toDirection string =
    let
        dummy = Debug.log "" "pressed"
    in
        case string of
            "l" ->
                KeyPressed Left
            "r" ->
                KeyPressed Right
            "f" ->
                KeyPressed Forward
            _ ->
                KeyPressed Other
          
subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
        [othersMove OthersMoved
        ,skywayId IdDefined
        ,Browser.Events.onKeyPress keyDecoder 
        ]

ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")
        
