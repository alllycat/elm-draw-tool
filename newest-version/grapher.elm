--coded in ELM version 0.15--

--x - - - -  I M P O R T S  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - x--


import Signal
import Window
import Graphics.Collage exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import List
import Text exposing (fromString)
import Mouse
import Graphics.Input exposing (..)



--x - - - -  M O D E L - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - x--




type alias Model = {forms : List Form, points : List (Int, Int), shapes : List Shape, selection : Selection, currentColor : Color}
type DrawMode = Filled | Outlined
type Selection = Waiting | PickingColor | PickingMode


initialModel = {forms = [] , points = [], shapes = [], selection = Waiting, currentColor = red}





--x - - - -  V I E W - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - x--




view (w,h) model = flow right [graph (h,h) model, flow down [buttons ((w - h), (h // 2)) model, drawCode ((w - h), (h // 2)) model]]

fixFromMouse (w,h) (x,y) = ((toFloat x) - ((toFloat w)/2), (-1) * ((toFloat y) - ((toFloat h)/2)))

graph (w,h) model = 
    layers [graphNumbers (w,h), graphLines (w,h), drawShapes (w,h) model, drawPoints (w,h) model]


graphNumbers (w,h) = 
    let
        numbers = List.filter (\i -> i % 50 == 0) [-h..h]
        textNumbers = List.map (\i -> text (fromString (toString i))) numbers
        movedTextNumbersX = List.map2 (\ num f -> moveX (toFloat num) f) numbers textNumbers
        movedTextNumbersY = List.map2 (\ num f -> moveY (toFloat num) f) numbers textNumbers
    in
        collage w h (movedTextNumbersX ++ movedTextNumbersY)

graphLines (w,h) =
    let 
        numbers = List.filter (\i -> i % 50 == 0) [-h..h]
        graphLinesX = List.map (\x -> traced defaultLine <| segment (toFloat x, toFloat h) (toFloat x, -(toFloat h))) numbers
        graphLinesY = List.map (\y -> traced defaultLine <| segment (-(toFloat w), toFloat y) (toFloat w, toFloat y)) numbers
    in
        collage w h (graphLinesX ++ graphLinesY)


drawPoints (w,h) model = 
    collage w h (List.map (\(x,y) -> move (fixFromMouse (w,h) (x,y)) (outlined (solid black) (circle 5))) model.points)

drawShapes (w,h) model = 
    collage w h (List.reverse (List.map (\ f -> move (toFloat h/16, toFloat h/4) f) model.forms))
    



ourMailbox = Signal.mailbox (Empty)



buttons (w,h) model = flow down 
        (case model.selection of 
            PickingColor ->
                [
                    button (Signal.message ourMailbox.address (ChangedColor red)) "Red",
                    button (Signal.message ourMailbox.address (ChangedColor orange)) "Orange",
                    button (Signal.message ourMailbox.address (ChangedColor yellow)) "Yellow",
                    button (Signal.message ourMailbox.address (ChangedColor green)) "Green",
                    button (Signal.message ourMailbox.address (ChangedColor blue)) "Blue",
                    button (Signal.message ourMailbox.address (ChangedColor purple)) "Purple",
                    button (Signal.message ourMailbox.address (ChangedColor brown)) "Brown",
                    button (Signal.message ourMailbox.address (ChangedColor black)) "Black",
                    button (Signal.message ourMailbox.address (ChangedColor white)) "White"
               --     button (Signal.message ourMailbox.address (Chn))
                ]
            PickingMode ->
                [
                    button (Signal.message ourMailbox.address (ChangedMode Filled (w,h))) "Filled",
                    button (Signal.message ourMailbox.address (ChangedMode Outlined (w,h))) "Outlined"
                ]
            Waiting ->
            [
                button (Signal.message ourMailbox.address (Done)) "Done"
            ])

drawCode (w,h) model = flow down
    (List.map (\ps -> collage w 20 [text (fromString ("polygon [" ++ (toString (List.map (\ (x,y) -> (x + (toFloat h / 16) + 20, y + (toFloat h / 4) + 80)) ps)) ++ "]"))]) model.shapes) 
    



--x - - - -  U P D A T E - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - x--




type Action = MouseClick (Int, Int) | Make (Int, Int) | Empty | ChangedMode DrawMode (Int, Int) | ChangedColor Color | Done

update action model =
    case action of 
        MouseClick pos ->
            {model | points <- pos :: model.points}
        Empty ->
            model
        Done ->
            {model | selection <- PickingColor}
        ChangedColor c ->
            {model | selection <- PickingMode, currentColor <- c}
        ChangedMode m (w,h) -> 
            let
                correctCoord p = (List.map (\(a,b) -> (toFloat a - ((toFloat w) / 2), ((toFloat h) / 2) - toFloat b)) ((List.drop 3 p)))
                make = 
                    case m of 
                        Filled -> filled model.currentColor
                        Outlined -> outlined {defaultLine | color <- model.currentColor}
            in
                {model | 
                    forms <- (make <| polygon (correctCoord model.points)) :: model.forms
                    , shapes <- (polygon (correctCoord model.points)) :: model.shapes
                    , points <- []
                    , selection <- Waiting}




--x - - - -  M A I N - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - x--




signal = Signal.foldp update initialModel 
        (Signal.merge
            (ourMailbox.signal)
            (Signal.map MouseClick (Signal.sampleOn (Mouse.clicks) Mouse.position)))

main : Signal Element
main = Signal.map2 view (Signal.constant (1366,643)) signal
