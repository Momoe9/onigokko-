port module Onigokko exposing (..)

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
import Block3d
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
import Dict exposing (Dict)
import Duration exposing (Duration)
import Task
import HandsSigns exposing (..)
import Types exposing (..)

main = Browser.element {init = init
                        ,update = update
                        ,view = view
                        ,subscriptions = subscriptions}


-- send
port join : String -> Cmd msg
port loggedIn : Player  ->  Cmd msg
port startGame : Bool  ->  Cmd msg
port wallsCompleted : {host:Player, walls:List {x:Int, y:Int, dir:Int}}   ->  Cmd msg
port moved : Player  ->  Cmd msg
port caught : List Player  ->  Cmd msg

-- receive
port skywayId : ({id:String, num:Int} -> msg) -> Sub msg
port othersLogin : (Player -> msg) -> Sub msg
port gameStarted : (String -> msg) -> Sub msg
port othersMove : (Player -> msg) -> Sub msg
port wallInfo: (List {x:Int, y:Int, dir:Int} -> msg) -> Sub msg
port handsReceiver : (List {x:Float, y:Float, z:Float} -> msg) -> Sub msg
port toGoJail : (List Player -> msg) -> Sub msg
       
    

mazeSize = 5
init: () -> (Model, Cmd Msg)
init _ =
    ({me = {id=Nothing, num=0, name="",x=5,y=5,theta=0,oni=False}
     ,room = ""
     ,name = ""
     ,host = False
     ,others = []
     ,mazeData = {maze = Dict.empty
                 ,outOfTree = vertexList mazeSize
                 ,currentPos = (0,0)
                 ,lerwStart = (0,0)
                 ,dual = []
                 }
     --,vertices = initialVertices
     --,prev = initialVertices
     ,state = Waiting
     ,angle = 0
     ,start = Nothing
     ,hands = []
     ,prevHands = []
     ,onHomePosition = False
     ,elapsed = 0
     ,started = False
     ,timeLeft = 0
     }
    ,Random.generate RandomPlayerGenerated randomPlayer)

randomPlayer: Random.Generator Player
randomPlayer =
    Random.map3
        (\x y theta ->
             {x=(toFloat (round x))+1.5
             ,y=(toFloat (round y))+1.5
             ,theta=theta
             ,id=Nothing
             ,oni=False
             ,name=""
             ,num=0
             }
        )
        (Random.float -10 10) 
        (Random.float -10 10) 
        (Random.float 0 (2*pi)) 

initPosition: Player -> Player
initPosition player = {player|
                       x = (toFloat (-mazeSize*3))+4.5-(toFloat (3*(modBy 2 player.num)))
                      --x = (toFloat (mazeSize*3))+4.5-(toFloat (3*(modBy 2 player.num)))+10
                      , y = if player.oni then
                                (toFloat ((-mazeSize*3) + (3*((player.num-1) // 2))))+1.5
                            else
                                -((toFloat ((-mazeSize*3) + (3*((player.num-1) // 2))))+1.5)
                      --, y = 10
                      , theta = 0
                      }

            
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
            let
                setName: Player -> String -> Player
                setName player n =
                    {player | name = n}
            in
            ( { model | me = setName model.me name }
            , Cmd.none
            )
        Join ->
            ( model
            , (join model.room)
            )
        IdDefined info ->
            let
                setId: Player -> String -> Int -> Player
                setId player id num = {player|id = Just id
                                      , num = num
                                      , oni = ((modBy 2 num)== 1)
                                      --, x = (toFloat (-mazeSize*3))+4.5-(toFloat (3*(modBy 2 num)))
                                      --, y = (toFloat ((-mazeSize*3) + (3*((num-1) // 2))))+1.5
                                      --,theta = 0
                                      }
            in
                ({model | me = initPosition <| setId model.me info.id info.num
                 ,host = Debug.log "host?" <| (info.num == 1)
                 }
                , if info.num == 1 then
                      Random.generate NextGen (nextDir (0,0) mazeSize)
                  else
                      loggedIn model.me
                )
        StartGame ->
            ({model|started = True, timeLeft = 30, me = initPosition model.me}
            , startGame True)
        GameStarted str ->
            let
                dummy = Debug.log "" "Game Started"
            in
                ({model|started=True, timeLeft = 30, me = initPosition model.me}, Cmd.none)
        KeyDown keycode ->
            let
                dummy = Debug.log "key" keycode
            in
                case keycode of
                    39 -> 
                        let
                            newMe = turnRight model.me
                        in
                            ({model| me = newMe}
                             , moved newMe)
                    37 ->
                        let
                            newMe = turnLeft model.me
                        in
                            ({model| me = newMe}
                             , moved newMe)
                    38 ->
                        let
                            newMe = (moveForward model).newMe
                            newlyCaught = (moveForward model).caught
                        in
                            ({model| me = newMe}
                            , Cmd.batch [ moved newMe
                                        , caught newlyCaught
                                        ]
                            )
                    40 ->
                        let
                            newMe = moveBackward model.me
                        in
                            ({model| me = newMe}
                             , moved newMe)
                    _ -> (model, Cmd.none)
        OthersMoved other ->
            let
                players = other::(List.filter (\player -> player.id /= other.id) model.others)
            in
                ({model|others=players}, Cmd.none)
        GoToJail players ->
            if List.member model.me players then
                let
                    p = model.me
                    newMe = {p|x = toFloat (3*mazeSize+10)
                            ,y= 10
                            }
                in
                    ({model|me=newMe}
                    , moved newMe
                    )
            else
                (model, Cmd.none)
        OthersLoggedIn other ->
            let
                players = Debug.log "others logined" <| other::(List.filter (\player -> player.id /= other.id) model.others)
            in
                ({model|others=players}
                ,wallsCompleted {host=model.me, walls=model.mazeData.dual}
                )
        SendWall walls t ->
            (model
            ,wallsCompleted {host=model.me, walls=model.mazeData.dual}
            ) 
        WallBuilt walls ->
            if model.host then
                (model, Cmd.none)
            else
                let
                    mazemodel = model.mazeData
                    newMaze = {mazemodel|dual = walls}
                in
                    ({model|mazeData=newMaze}, Cmd.none)
        NextGen dir ->
            let
                newMaze = addToMaze dir model.mazeData
                completed = (List.length newMaze.outOfTree) == 0
                newMazeWithDual = if completed then
                                      {newMaze | dual = dual newMaze.maze}
                                  else
                                      newMaze
            in
                ({model | mazeData = newMazeWithDual}
                ,if not completed then
                     Random.generate
                         NextGen (nextDir newMaze.currentPos mazeSize)
                 else
                     wallsCompleted {host=model.me, walls=model.mazeData.dual}
                )
        Elapsed t ->
            let
                dummy = Debug.log "ellapsed" t
            in
                if model.started then
                    ({model|timeLeft=(model.timeLeft-1)},
                         Cmd.none)
                else
                    (model, Cmd.none)
        Hands handsData ->
            ({model | hands = handsData}, Cmd.none)
        LocateHands t ->
            if model.elapsed < 1 then
                ({model | elapsed = model.elapsed+1}, Cmd.none)
            else
                ({model | prevHands = model.hands
                 ,elapsed = 0
                 }
                ,case (handsDirection model.hands) of
                     Just Left ->
                         Task.perform KeyDown <| Task.succeed 37
                     Just Right ->
                         Task.perform KeyDown <| Task.succeed 39
                     _ -> case (handsForward model.hands) of
                              Just Forward ->
                                  Task.perform KeyDown <| Task.succeed 38
                              _ -> Cmd.none
                )
                    
--dual: Maze -> Dict (Int, Int) (List MazeDirection)
dual: Maze -> List {x:Int, y:Int, dir:Int}
dual primal =
    let
        dualV = List.concat <|
                List.map (\x ->
                              List.map (\y -> (x,y)) (List.range (-mazeSize) (mazeSize+1))
                         )
                    (List.range (-mazeSize) (mazeSize+1))
        edges: (Int, Int) -> List MazeDirection
        edges (x,y) =
            if (x,y) == (-mazeSize, -mazeSize) then
                []
            else if x == (-mazeSize) then
                     [South]
                 else if y == (-mazeSize) then
                          [West]
                      else
                          [South, West]
                              
        initialEdges : Dict (Int, Int) (List MazeDirection)
        initialEdges =
            Dict.insert (mazeSize+3,2) [North] <|
            Dict.insert (mazeSize+3,3) [North] <|
            Dict.insert (mazeSize+3,4) [East] <|
            Dict.insert (mazeSize+4,4) [East] <|
            Dict.insert (mazeSize+5,4) [South] <|
            Dict.insert (mazeSize+5,3) [South] <|
            Dict.insert (mazeSize+5,2) [West] <|
            Dict.insert (mazeSize+4,2) [West] <|
            (List.foldl (\v dict -> Dict.insert v (edges v) dict ) (Dict.empty) dualV)
                
        remove: (Int, Int) -> (Int, Int) -> Dict (Int,Int) (List MazeDirection) -> Dict (Int,Int) (List MazeDirection)
        remove (fromX, fromY) (toX, toY) dict =
            let
                dx = toX-fromX
                dy = toY-fromY
                pdir = if dx > 0 then
                          East
                      else if dx < 0 then
                               West
                           else if dy > 0 then
                                    North
                                else
                                   South
            in
                case pdir of
                    West ->
                        let
                            leftAbove = Maybe.withDefault [] <| Dict.get (fromX, (fromY+1)) dict
                            leftBelow = Maybe.withDefault [] <| Dict.get (fromX, (fromY-1)) dict
                        in
                            Dict.insert (fromX, (fromY-1)) (List.filter (\dir -> dir /= North) leftBelow) <|
                            Dict.insert (fromX, (fromY+1)) (List.filter (\dir -> dir /= South) leftAbove) dict
                    East ->
                        let
                            rightAbove = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY+1)) dict
                            rightBelow = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY-1)) dict
                        in
                            Dict.insert ((fromX+1), (fromY-1)) (List.filter (\dir -> dir /= North) rightBelow) <|
                            Dict.insert ((fromX+1), (fromY+1)) (List.filter (\dir -> dir /= South) rightAbove) dict
                    South ->
                        let
                            leftAbove = Maybe.withDefault [] <| Dict.get ((fromX), (fromY)) dict
                            rightAbove = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY)) dict
                        in
                            Dict.insert ((fromX+1), (fromY)) (List.filter (\dir -> dir /= West) rightAbove) <|
                            Dict.insert ((fromX), (fromY)) (List.filter (\dir -> dir /= East) leftAbove) dict
                    North ->
                        let
                            leftBelow = Maybe.withDefault [] <| Dict.get ((fromX), (fromY+1)) dict
                            rightBelow = Maybe.withDefault [] <| Dict.get ((fromX+1), (fromY+1)) dict
                        in
                            Dict.insert ((fromX+1), (fromY+1)) (List.filter (\dir -> dir /= West) rightBelow) <|
                            Dict.insert ((fromX), (fromY+1)) (List.filter (\dir -> dir /= East) leftBelow) dict
    in
            Dict.foldl
                (\(x,y) dirs dirList->
                     dirList++
                     (List.map (\dir -> {x=x
                                       ,y=y
                                       ,dir= case dir of
                                                 North -> 1
                                                 South -> 3
                                                 East -> 0
                                                 West -> 2
                                       }
                              )
                     dirs)
                )[] <|
                Dict.foldl (\k v dict -> remove k v dict) (remove (mazeSize, 0) (mazeSize+1, 0) initialEdges) primal

                    
nextDir: (Int, Int) -> Int -> Random.Generator MazeDirection
nextDir (x,y) size =
    let
        east = if x < size then
                   [East]
               else
                   []
        west = if x > (-size) then
                   [West]
               else
                   []
        north = if y < size then
                   [North]
               else
                   []
        south = if y > (-size) then
                   [South]
               else
                   []
        pos = (x,y) 
        dirs = List.concat [east, west, south, north]
    in
        Random.uniform
            (Maybe.withDefault West <| List.head dirs)
            (List.drop 1 dirs)

vertexList: Int -> List (Int, Int)
vertexList s =
    List.filter (\(x,y) -> x /= s || y /= s) <|
        List.concat <|
            List.map (\x ->
                          List.map (\y -> (x,y)) (List.range (-s) s)
                     )
                (List.range (-s) s)

addToMaze: MazeDirection -> MazeModel -> MazeModel
addToMaze dir model =
    let
        x = Tuple.first model.currentPos
        y = Tuple.second model.currentPos
        next = case dir of
                   North -> (x, y+1)
                   South -> (x, y-1)
                   East -> (x+1, y)
                   West -> (x-1, y)
        newMaze = Dict.insert model.currentPos next model.maze
                  
        delete: (Int,Int) -> Maze -> List (Int, Int) -> List (Int, Int)
        delete p maze outOfTree =
            if (Tuple.first p) > mazeSize then
                outOfTree
            else if List.member p outOfTree then
                     delete (Maybe.withDefault (mazeSize+1,0) <| Dict.get p newMaze)
                         newMaze  (List.filter (\q -> p /= q) outOfTree)
                 else
                     outOfTree
        newOutOfTree = if List.member next model.outOfTree then
                           model.outOfTree
                       else -- in tree
                           delete model.lerwStart newMaze model.outOfTree
        newStart = if List.member next model.outOfTree then
                       next
                   else -- in tree
                       --Maybe.withDefault (0,0) <| List.head model.outOfTree
                       Maybe.withDefault (0,0) <| List.head newOutOfTree
                           
        newCurrentPos = if List.member next model.outOfTree then
                            next
                        else -- in tree
                            newStart
    in
        {model |
         maze = newMaze
        ,lerwStart = newStart
        ,outOfTree = newOutOfTree
        ,currentPos = newCurrentPos
        }
                    
turnLeft: Player -> Player
turnLeft p = {p|theta=p.theta+(3*pi/180)}          

turnRight: Player -> Player
turnRight p = {p|theta=p.theta-(3*pi/180)}          

moveForward: Model -> {newMe:Player, caught:List Player}
moveForward model =
    let
        wallWidth = 0.1
        playerRadius = 0.5
        d = wallWidth + playerRadius
        p = model.me
        extraWalls = List.map (\y -> {x=-mazeSize+1, y=y, dir=1})  (List.range (-mazeSize) mazeSize)
        walls = if model.started && model.timeLeft > 0 then
                    model.mazeData.dual ++ extraWalls
                else
                    model.mazeData.dual
        cx = Debug.log "cx" <| floor (p.x/3)
        cy = Debug.log "cy" <| floor (p.y/3)
        bot = Debug.log "south wall" <| ((List.member {x=cx,y=cy,dir=0} walls) || (List.member {x=(cx+1),y=cy,dir=2} walls))
        theta = Debug.log "theta" <| model.me.theta - 2*pi*(toFloat <| floor (model.me.theta/(2*pi)))
        northBorder = if ((List.member {x=cx,y=(cy+1),dir=0} walls) || (List.member {x=(cx+1),y=(cy+1),dir=2} walls)) then
                          (toFloat (3*(cy+1))) - d
                      else
                          10000
        southBorder = if ((List.member {x=cx,y=cy,dir=0} walls) || (List.member {x=(cx+1),y=cy,dir=2} walls)) then
                          (toFloat (3*cy)) + d
                      else
                          -10000
        westBorder = if ((List.member {x=cx,y=cy,dir=1} walls) || (List.member {x=cx,y=(cy+1),dir=3} walls)) then
                          (toFloat (3*cx)) + d
                      else
                          -10000
        eastBorder = if ((List.member {x=(cx+1),y=cy,dir=1} walls) || (List.member {x=(cx+1),y=(cy+1),dir=3} walls)) then
                          (toFloat (3*(cx+1))) - d
                      else
                          10000
        newX = if (cos p.theta) >= 0 then
                   Basics.min (p.x + 0.15*(cos p.theta)) eastBorder
               else
                   Basics.max (p.x + 0.15*(cos p.theta)) westBorder
        newY = if (sin p.theta) >= 0 then
                   Basics.min (p.y + 0.15*(sin p.theta)) northBorder
               else
                   Basics.max (p.y + 0.15*(sin p.theta)) southBorder
        dist player1 player2 =
            sqrt (((player1.x-player2.x)^2) + ((player1.y-player2.y)^2))
        newlyCaught = if model.me.oni && model.started then
                     List.filter (\player -> (not player.oni)
                                 && (dist model.me player) < 1
                                 ) model.others
                 else
                     []
    in
        {newMe = {p| x = newX, y = newY}, caught=newlyCaught}

moveBackward: Player -> Player
moveBackward p =
    let
        newX = p.x - 0.5*(cos p.theta)
        newY = p.y - 0.5*(sin p.theta)
    in
        {p| x = newX, y = newY}


wallView: MazeModel ->  List (Scene3d.Entity coordinates)
wallView mazemodel =
    let
        materialBrown =
            Material.nonmetal
                { baseColor = Color.brown
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }
        wallHeight=0.7
        wallWidth=0.1
        wallEntity: {x:Int, y:Int, dir:Int} -> Scene3d.Entity coordinates
        wallEntity wall =
            case wall.dir of
                0 -> Scene3d.block materialBrown -- East
                        <| Block3d.from
                            (Point3d.meters (toFloat (3*wall.x)) ((toFloat (3*wall.y))-wallWidth) 0)
                            (Point3d.meters (toFloat ((3*wall.x)+3)) ((toFloat (3*wall.y))+wallWidth) wallHeight)
                2 -> Scene3d.block materialBrown -- West
                     <| Block3d.from
                         (Point3d.meters (toFloat (3*wall.x)) ((toFloat (3*wall.y))-wallWidth) 0)
                         (Point3d.meters (toFloat ((3*wall.x)-3)) ((toFloat (3*wall.y))+wallWidth) wallHeight)
                1 -> Scene3d.block materialBrown -- North
                     <| Block3d.from
                         (Point3d.meters ((toFloat (3*wall.x))-wallWidth) ((toFloat (3*wall.y))) 0)
                         (Point3d.meters ((toFloat (3*wall.x))+wallWidth) ((toFloat (3*wall.y))+3) wallHeight)
                3 -> Scene3d.block materialBrown -- South
                     <| Block3d.from
                         (Point3d.meters ((toFloat (3*wall.x))-wallWidth) ((toFloat (3*wall.y))) 0)
                         (Point3d.meters ((toFloat (3*wall.x))+wallWidth) ((toFloat (3*wall.y))-3) wallHeight)
                _ -> Scene3d.block materialBrown -- South
                     <| Block3d.from
                         (Point3d.meters ((toFloat (3*wall.x))-wallWidth) ((toFloat (3*wall.y))) 0)
                         (Point3d.meters ((toFloat (3*wall.x))+wallWidth) ((toFloat (3*wall.y))-1) wallHeight)
    in
        List.map wallEntity mazemodel.dual


jailView: List (Scene3d.Entity coordinates)
jailView =
    let
        materialGray = Material.metal
                       { baseColor = Color.gray
                       , roughness = 0.0 -- varies from 0 (mirror-like) to 1 (matte)
                       }
        jailHeight=3
        pole: Int -> Int -> Scene3d.Entity coordinates
        pole x y =
            Scene3d.cylinderWithShadow materialGray
                <| Cylinder3d.along
                    (Axis3d.through (Point3d.meters
                                         ((toFloat ((mazeSize+3)*3))+0.5*(toFloat (x)))
                                         (6+0.5*(toFloat (y)))
                                         0
                                    )
                         Direction3d.z
                    )
                    { start = Length.meters 0
                    , end = Length.meters jailHeight
                    , radius = Length.meters 0.05
                    }
    in
        (List.map (\y -> pole y 0) (List.range 0 12))
        ++(List.map (\y -> pole y 12) (List.range 0 12))
        ++(List.map (\x -> pole 0 x) (List.range 0 12))
        ++(List.map (\x -> pole 12 x) (List.range 0 12))
    
            
grateView: Int ->  List (Scene3d.Entity coordinates)
grateView timeLeft =
    let
        materialGray =
            Material.metal
                { baseColor = Color.gray
                , roughness = 0.0 -- varies from 0 (mirror-like) to 1 (matte)
                }
        wallHeight=0.7
        jailHeight=2*(toFloat timeLeft)/30
        jailEntity: Int -> Scene3d.Entity coordinates
        jailEntity i =
            Scene3d.cylinderWithShadow materialGray
                <| Cylinder3d.along
                    (Axis3d.through (Point3d.meters (-(((toFloat mazeSize)-1)*3)) (-15+0.5*(toFloat (i)))  0) Direction3d.z)
                         { start = Length.meters 0
                         , end = Length.meters jailHeight
                         , radius = Length.meters 0.1
                         }
    in
        if timeLeft <= 0 then
            []
        else
            List.map jailEntity (List.range 1 30)
    
            
view: Model -> Html Msg
view model =
    div [align "center"
        ]
    (case model.me.id of
         Nothing -> [div []
                         [text "Room Id は友達と決めてください。Nicknameは適当に決めてください。"]
                    ,input
                         [ type_ "text"
                         , placeholder "Room ID"
                         , onInput RoomChanged
                         , on "keydown" (ifIsEnter Join)
                         , value model.room
                         ]
                         []
                    ,input
                         [ type_ "text"
                         , placeholder "Nickname"
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

                 materialGray =
                     Material.nonmetal
                         { baseColor = Color.gray
                         , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                         }

                 materialBrown =
                     Material.nonmetal
                         { baseColor = Color.brown
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
                           , radius = Length.meters 0.3
                           }
                  
                 left = Scene3d.sphere materialWhite
                        <| Sphere3d.atPoint
                            (Point3d.meters
                                 (0.7*(cos (model.me.theta+(-pi/10))))
                                 (0.7*(sin (model.me.theta+(-pi/10))))
                                 1
                            )
                            (Length.meters 0.3)
                 right = Scene3d.sphere materialWhite
                         <| Sphere3d.atPoint
                             (Point3d.meters
                                  (0.7*(cos (model.me.theta+(pi/10))))
                                  (0.7*(sin (model.me.theta+(pi/10))))
                                  1
                             )
                             (Length.meters 0.3)
                 lb = Scene3d.sphere materialBlack
                      <| Sphere3d.atPoint
                          (Point3d.meters
                               (0.8*(cos (model.me.theta-(pi/10))))
                               (0.8*(sin (model.me.theta-(pi/10))))
                               1
                          )
                          (Length.meters 0.22)
                 rb = Scene3d.sphere materialBlack
                      <| Sphere3d.atPoint
                          (Point3d.meters
                               (0.8*(cos (model.me.theta+(pi/10))))
                               (0.8*(sin (model.me.theta+(pi/10))))
                               1
                          )
                          (Length.meters 0.22)

                 robot = thiefView (Player (Just "") 0 "test" 1.5 1.5 0 False)
                 walls = wallView model.mazeData
                 grate = grateView model.timeLeft

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
                                   , eyePoint = (Point3d.meters ex ey 1.5 )
                                   , upDirection = Direction3d.positiveZ
                                   }
                             , verticalFieldOfView = Angle.degrees 90
                             }
                 relativePos event =
                     {x=Tuple.first event.pointer.offsetPos
                     ,y=Tuple.second event.pointer.offsetPos}
             in
                 (
                  (if (not model.started) && model.host then
                       [startButton]
                   else
                       []
                  )
                      ++
                      [div [Html.Attributes.id "doro"
                           ,style "position" "absolute"
                           ,style "top" "100px"
                           ,style "left" "0px"]
                           [text "どろ"
                           ,ul [] (
                                   (List.map
                                        (\player -> li []
                                             [text player.name]
                                        )
                                        (List.filter (\player -> not player.oni)
                                             model.others
                                        )
                                   )++(if model.me.oni then
                                           []
                                       else
                                           [li [][text model.me.name]]
                                      )
                                  )
                           ]
                      ]
                      ++
                      [div [Html.Attributes.id "doro"
                           ,style "position" "absolute"
                           ,style "top" "100px"
                           ,style "right" "100px"]
                           [text "けー"
                           ,ul [](
                                  (List.map
                                       (\player -> li []
                                            [text player.name]
                                       )
                                       (List.filter (\player -> player.oni)
                                            model.others
                                       )
                                  )++(if model.me.oni then
                                          [li [][text model.me.name]]
                                      else
                                          []
                                     )
                                 )
                           ]
                      ]
                 ++[Scene3d.sunny
                        { camera = camera
                        , clipDepth = Length.centimeters 0.5
                        , dimensions = ( Pixels.int 1200, Pixels.int 1000 )
                        , background = Scene3d.transparentBackground
                        , entities = [plane]++walls++grate++goalView++jailView++(List.map playerView model.others)
                        , shadows = True
                        , upDirection = Direction3d.z
                        , sunlightDirection = Direction3d.xz (Angle.degrees -60)
                        }
                 ]
                 )
         )


startButton: Html Msg
startButton =
    div []
        [button [id "startbutton"
                ,onClick StartGame]
             [text "start"]
        ]

goalView: List (Scene3d.Entity coordinates)
goalView = 
    let
        material = Material.nonmetal
                    { baseColor = Color.red
                    , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                    }
            
        pole1 = Scene3d.cylinder material
             <| Cylinder3d.along
                (Axis3d.through (Point3d.meters (toFloat(3*(mazeSize+1))) 0  0) Direction3d.z)
                { start = Length.meters 0 
                , end = Length.meters 3
                , radius = Length.meters 0.2
                }

        pole2 = Scene3d.cylinder material
             <| Cylinder3d.along
                (Axis3d.through (Point3d.meters (toFloat(3*(mazeSize+1))) 3  0) Direction3d.z)
                { start = Length.meters 0 
                , end = Length.meters 3
                , radius = Length.meters 0.2
                }
        bar  = Scene3d.cylinder material
             <| Cylinder3d.along
                (Axis3d.through (Point3d.meters (toFloat(3*(mazeSize+1))) 0  3) Direction3d.y)
                { start = Length.meters 0 
                , end = Length.meters 3
                , radius = Length.meters 0.2
                }

    in
        [pole1, pole2, bar]
    

playerView: Player -> Scene3d.Entity coordinates
playerView player =
    if player.oni then
        copView player
    else
        thiefView player
            
copView: Player -> Scene3d.Entity coordinates
copView player =
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

              
        tsubaL = Scene3d.block material
              <| Block3d.from
                  (Point3d.meters player.x player.y 1.02)
                  (Point3d.meters 
                       (player.x + 1.0*(cos (player.theta+pi/6)))
                       (player.y + 1.0*(sin (player.theta+pi/6)))
                       1.05
                  )
        tsubaR = Scene3d.block material
              <| Block3d.from
                  (Point3d.meters player.x player.y 1.02)
                  (Point3d.meters 
                       (player.x + 1.0*(cos (player.theta-pi/6)))
                       (player.y + 1.0*(sin (player.theta-pi/6)))
                       1.05
                  )
                
        cyl = Scene3d.cylinder material
              <| Cylinder3d.along
                  (Axis3d.through (Point3d.meters player.x player.y 0) Direction3d.z)
                  { start = Length.meters 0
                  , end = Length.meters 1.5
                  , radius = Length.meters 0.5
                  }
                  
        left = Scene3d.sphere materialWhite
               <| Sphere3d.atPoint
                   (Point3d.meters
                        (player.x + 0.29*(cos (player.theta+(-pi/9))))
                        (player.y + 0.29*(sin (player.theta+(-pi/9))))
                        0.8
                   )
                   (Length.meters 0.25)
        right = Scene3d.sphere materialWhite
                <| Sphere3d.atPoint
                    (Point3d.meters
                         (player.x + 0.29*(cos (player.theta+(pi/9))))
                         (player.y + 0.29*(sin (player.theta+(pi/9))))
                         0.8
                    )
                    (Length.meters 0.25)
        lb = Scene3d.sphere materialBlack
             <| Sphere3d.atPoint
                 (Point3d.meters
                      (player.x + 0.37*(cos (player.theta-(pi/9))))
                      (player.y + 0.37*(sin (player.theta-(pi/9))))
                      0.8
                 )
                 (Length.meters 0.185)
        rb = Scene3d.sphere materialBlack
             <| Sphere3d.atPoint
                 (Point3d.meters
                      (player.x + 0.37*(cos (player.theta+(pi/9))))
                      (player.y + 0.37*(sin (player.theta+(pi/9))))
                      0.8
                 )
                 (Length.meters 0.185)

        robot = Scene3d.group [cyl,left,right,lb,rb,tsubaR, tsubaL]
    in
        robot
        

thiefView: Player -> Scene3d.Entity coordinates
thiefView player =
    let
        material =
            Material.nonmetal
                { baseColor = Color.lightOrange
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
        eyeHight = 1.0
                
        cyl = List.map (\i -> 
                        Scene3d.cylinder (if (modBy 2 i)==0 then 
                                            materialWhite
                                        else 
                                            materialBlack)
                        <| Cylinder3d.along
                        (Axis3d.through (Point3d.meters player.x player.y 0) Direction3d.z)
                        { start = Length.meters (0.1*(toFloat i))
                        , end = Length.meters (0.1*(toFloat (i+1)))
                        , radius = Length.meters 0.5
                        })
                        <| List.range 0 5


        face =  Scene3d.cylinder  
                        materialBlack
                        <| Cylinder3d.along
                        (Axis3d.through (Point3d.meters player.x player.y 0) Direction3d.z)
                        { start = Length.meters 0.6
                        , end = Length.meters 1.2
                        , radius = Length.meters 0.5
                        }
                  
        left = Scene3d.sphere materialWhite
               <| Sphere3d.atPoint
                   (Point3d.meters
                        (player.x + 0.29*(cos (player.theta+(-pi/9))))
                        (player.y + 0.29*(sin (player.theta+(-pi/9))))
                        eyeHight
                   )
                   (Length.meters 0.25)
        right = Scene3d.sphere materialWhite
                <| Sphere3d.atPoint
                    (Point3d.meters
                         (player.x + 0.29*(cos (player.theta+(pi/9))))
                         (player.y + 0.29*(sin (player.theta+(pi/9))))
                         eyeHight
                    )
                    (Length.meters 0.25)
        lb = Scene3d.sphere materialBlack
             <| Sphere3d.atPoint
                 (Point3d.meters
                      (player.x + 0.37*(cos (player.theta-(pi/9))))
                      (player.y + 0.37*(sin (player.theta-(pi/9))))
                      eyeHight
                 )
                 (Length.meters 0.185)
        rb = Scene3d.sphere materialBlack
             <| Sphere3d.atPoint
                 (Point3d.meters
                      (player.x + 0.37*(cos (player.theta+(pi/9))))
                      (player.y + 0.37*(sin (player.theta+(pi/9))))
                      eyeHight
                 )
                 (Length.meters 0.185)

        cap = Scene3d.sphere materialBlack
            <| Sphere3d.atPoint
                (Point3d.meters
                    player.x 
                    player.y 
                    1.2)
                    (Length.meters 0.5)

        robot = Scene3d.group (cyl ++[left,right,lb,rb, face, cap])
    in
        robot
          
subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
        [othersMove OthersMoved
        ,othersLogin OthersLoggedIn
        ,gameStarted GameStarted
        ,skywayId IdDefined
        ,handsReceiver Hands
        ,toGoJail GoToJail
        ,wallInfo WallBuilt
        ,Time.every 5000 (SendWall model.mazeData.dual)
        ,Time.every 1000 Elapsed
        ,Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> LocateHands)
        ]

ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")
        
onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (D.map tagger keyCode)
