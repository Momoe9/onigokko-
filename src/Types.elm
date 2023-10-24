module Types exposing (..)

import Dict exposing (Dict)
import Duration exposing (Duration)
import Time

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
                   ,mazeData: MazeModel
                   ,state : State
                   ,angle : Float
                   ,start : Maybe {x:Float, y:Float}
                   ,hands : List {x:Float, y:Float, z:Float}
                   ,prevHands : List {x:Float, y:Float, z:Float}
                   ,onHomePosition : Bool
                   ,elapsed: Int
                   }

type Msg = OthersLoggedIn Player
         | OthersMoved Player
         | IdDefined {id:String, num:Int}
         | RoomChanged String
         | NameChanged String
         | Join
         | RandomPlayerGenerated Player
         | KeyDown Int
         | Hands (List {x:Float, y:Float, z:Float})
         | NextGen MazeDirection
         | WallBuilt (List {x:Int, y:Int, dir:Int})
         | SendWall (List {x:Int, y:Int, dir:Int}) Time.Posix
         | LocateHands Duration
--         | Elapsed Duration
           
type Direction = Left
               | Right
               | Other
               | Forward
               | Backward

type alias Maze = Dict (Int, Int) (Int, Int)
       
type alias MazeModel = {maze: Maze
                       ,outOfTree: (List (Int, Int))
                       ,currentPos: (Int, Int)
                       ,lerwStart: (Int, Int)
                       --,dual: Dict (Int, Int) (List MazeDirection)
                       ,dual: List {x:Int, y:Int, dir:Int}
                       }

type MazeDirection = North
                   | South
                   | East
                   | West

type State = Waiting
           | MovingFront
           | MovingLeft
           | MovingTop
