--coded in ELM version 0.14--

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window
import Text exposing (..)

---------- YOUR SHAPES HERE -------------

yourShapes = group [
        ]

---------- IGNORE CODE BELOW. ***READ: FOR DRAWING, TO CHANGE FROM FILLED DRAW TO LINE DRAW, SCROLL AND FIND THE 
---------- LONG DASHED LINE TO SWITCH! IF YOU'RE CONFUSED, ASK AN INSTRUCTOR! ------------------------------

main : Signal Element
-- signal is a value that you want to change over time
-- this can be things like numbers, or even html
main =
  -- 3 arguments are pattern matched to scene [1] (w,h) [2] locs [3] (a,b)
  Signal.map3 scene Window.dimensions clickLocations Mouse.position -- takes 3 arguments

clickLocations : Signal (List (Int,Int))
clickLocations =
  -- fold allows signal to start at 0
  Signal.foldp (::) [] (Signal.sampleOn Mouse.clicks Mouse.position)  --TECHNICALLY....AN UPDATE
-- foldp listens for events to happen. It tells elm when the next event occurs
-- thereforE telling everyone else to update their shit

--2 different types: (1) type alias (2) just a type
--records ({fields, names of fields}), type alias, good for multiple components (e.g x,y + a colour)
--type constructor (outside/inside) case statement

scene : (Int,Int) -> List (Int,Int) -> (Int,Int) -> Element
-- 3 scene arguments are pattern matched to main:
    -- [1] Window.dimensions
    -- [2] clickLocations
    -- [3] Mouse.position
scene (w,h) locs (a,b) =

  -- let = definitions
  let

    drawLines (locs) = 
        let
           floatlocs = List.map (\(a,b) -> (toFloat (a - (w//2)), (-1) * (toFloat (b - (h//2))))) locs
           -- \ (arguments) anonymous functions, quick functions
        in
-------- SWITCH THESE TO GET FILLED VS LINES ----------------------------------------------------------------------
            
            --traced (solid black) (path floatlocs)
            polygon floatlocs |> filled red
    -- pointer code
    (xCentered,yCentered) =
    (toFloat a - toFloat w/2, toFloat h/2 - toFloat b)
    -- updating point position next to pointer
    xy = (text <| bold <| fromString <| "(x, y) = " ++ (toString (a - (w//2),b - (h//2))))
                  |> move (xCentered + 70, yCentered)
    -- draws points
    drawPoint (x,y) =
          circle 5
          -- creates the circle for each point
            |> filled (hsla (toFloat x) 0.99 0.5 0.99) -- points change colour
            |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
            |> rotate (toFloat x) -- only used if the user wants to change the circle to another shape
  -- in = things that execute
    
    showCoordinate (x,y) =   
    -- For each 
          text (fromString <| "                        " ++ (toString (x - (w//2), y - (h//2))))
            |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
          
  in
      layers
        [ collage w h [pointer |> move (xCentered,yCentered)]
        , collage w h [xy]
        , collage w h [drawLines locs]
        , collage w h [yourShapes]
        , collage w h [cartesianPlane]
        , collage w h (List.map showCoordinate locs)
        , collage w h (List.map drawPoint locs)

        
        ]


------------- V - - - v ALL GRAPHICS BELOW v - - - V --------------

outlineOption = group [ (outlined (solid black) (ngon 4 20))
            ]

filledOption = group [ ngon 4 20
        |> filled blue
        |> move (0,0)
        ]   

pointer = group [ (outlined (solid black) (circle 5))
            ]

undoButton = group [ circle 30
        |> filled yellow
        |> move (0,0)
    , circle 20
        |> filled black
        |> move (0,0)
    , circle 10
        |> filled yellow
        |> move (0,0)
    , rect 17 15
        |> filled yellow
        |> move (-15, 5)
        |> rotate (degrees 10)
    , ngon 3 10
        |> filled black
        |> move (-11, 10)
        |> rotate (degrees -40)
    ]


garbageCan = group [ rect 10 50
        |> filled lightRed
        |> move (0,0)
        |> rotate (degrees 43)
    , rect 10 50
        |> filled lightRed
        |> move (0,0)
        |> rotate (degrees -43)        
    , circle 6
        |> filled white
        |> move (0,14)           
    , circle 5
        |> filled darkGrey
        |> move (0,14)   
    , rect 22 32
        |> filled white
        |> move (0,0)
    , circle 4
        |> filled white
        |> move (0,13.5)        
    , rect 20 30
        |> filled darkGrey
        |> move (0,0)  
    , rect 20 0.7
        |> filled white
        |> move (0,10)
    , rect 0.3 26
        |> filled white
        |> move (-7,-2.5)
    , rect 0.3 26
        |> filled white
        |> move (-3,-2.5)
    , rect 0.3 26
        |> filled white
        |> move (1,-2.5)
    , rect 0.3 26
        |> filled white
        |> move (5,-2.5)
    , rect 0.3 26
        |> filled white
        |> move (9,-2.5)             
    ]

darkGrey : Color
darkGrey =
  rgba 111 111 111 1

buttonMenu = group
    [ rect 225 50
         |> filled black
         |> move (2.5,25)
    , rect 224 49
         |> filled white
         |> move (2.5,25)
    ]

cartesianPlane = group
    [    
     move (0,0) (filled black (rect 2 700)) |> rotate (degrees 90 )    
    , move (0,0) (filled black (rect 2 700))
       -- black cartesian arrows --
    , move (0, 255) (filled black (ngon 3 5)) |> rotate (degrees 90 )
    , move (0, -255) (filled black (ngon 3 5)) |> rotate (degrees 270 )
    , move (255, 0) (filled black (ngon 3 5))
    , move (-255, 0) (filled black (ngon 3 5))  |> rotate (degrees 180 )
       -- grey grid lines --
    , move (50,0) (filled clearGrey (rect 2 700))
    , move (100,0) (filled clearGrey (rect 2 700))
    , move (150,0) (filled clearGrey (rect 2 700))
    , move (200,0) (filled clearGrey (rect 2 700))
    , move (250,0) (filled clearGrey (rect 2 700))
    , move (300,0) (filled clearGrey (rect 2 700))
    , move (350,0) (filled clearGrey (rect 2 700))
    , move (-50,0) (filled clearGrey (rect 2 700))
    , move (-100,0) (filled clearGrey (rect 2 700))
    , move (-150,0) (filled clearGrey (rect 2 700))
    , move (-200,0) (filled clearGrey (rect 2 700))
    , move (-250,0) (filled clearGrey (rect 2 700))
    , move (-300,0) (filled clearGrey (rect 2 700))
    , move (-350,0) (filled clearGrey (rect 2 700))
    , move (0,50) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,100) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,150) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,200) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )    
    , move (0,250) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,300) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,350) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,-50) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,-100) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,-150) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,-200) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )    
    , move (0,-250) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,-300) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
    , move (0,-350) (filled clearGrey (rect 2 700)) |> rotate (degrees 90 )
         -- black co-ordinate ticks --
    , move (50,0) (filled black (rect 2 6))
    , move (100,0) (filled black (rect 2 6))
    , move (150,0) (filled black (rect 2 6))
    , move (200,0) (filled black (rect 2 6))
    , move (250,0) (filled black (rect 2 6))
    , move (300,0) (filled black (rect 2 6))
    , move (350,0) (filled black (rect 2 6))
    , move (-50,0) (filled black (rect 2 6))
    , move (-100,0) (filled black (rect 2 6))
    , move (-150,0) (filled black (rect 2 6))
    , move (-200,0) (filled black (rect 2 6))
    , move (-250,0) (filled black (rect 2 6))
    , move (-300,0) (filled black (rect 2 6))
    , move (-350,0) (filled black (rect 2 6))
    , move (0,50) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,100) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,150) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,200) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,250) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,300) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,350) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,-50) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,-100) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,-150) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,-200) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,-250) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,-300) (filled black (rect 2 6)) |> rotate (degrees 90 )
    , move (0,-350) (filled black (rect 2 6)) |> rotate (degrees 90 )
          -- numbers on the plane, x axis --
    , text (fromString "0")
    , text (fromString "50") |> move (50,-20)    
    , text (fromString "100") |> move (100,-20)       
    , text (fromString "150") |> move (150,-20)        
    , text (fromString "200") |> move (200,-20)  
    , text (fromString "250") |> move (250,-20)       
    , text (fromString "300") |> move (300,-20)        
    , text (fromString "350") |> move (350,-20)   
    , text (fromString "-50") |> move (-50,-20)  
    , text (fromString "-100") |> move (-100,-20)  
    , text (fromString "-150") |> move (-150,-20)  
    , text (fromString "-200") |> move (-200,-20)
    , text (fromString "-250") |> move (-250,-20)  
    , text (fromString "-300") |> move (-300,-20)  
    , text (fromString "-350") |> move (-350,-20)  
          -- numbers on the plane, y axis --
    , text (fromString "50") |> move (20,50)    
    , text (fromString "100") |> move (20,100)        
    , text (fromString "150") |> move (20,150)        
    , text (fromString "200") |> move (20,200)
    , text (fromString "250") |> move (20,250)        
    , text (fromString "300") |> move (20,300)        
    , text (fromString "350") |> move (20,350)  
    , text (fromString "-50") |> move (20,-50)   
    , text (fromString "-100") |> move (20,-100)   
    , text (fromString "-150") |> move (20,-150)   
    , text (fromString "-200") |> move (20,-200)
    , text (fromString "-250") |> move (20,-250)   
    , text (fromString "-300") |> move (20,-300)   
    , text (fromString "-350") |> move (20,-350)
    
    ]
    
clearGrey : Color
clearGrey =
  rgba 111 111 111 0.3
                                    
GRAPH BACKGROUND CODE

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window
import Text exposing (..)

main = Signal.map2 layout Mouse.position Window.dimensions

layout (x,y) (width,height) =
 	let (xCentered,yCentered) =
 	(toFloat x - toFloat width/2, toFloat height/2 - toFloat y)
 	in collage width height
            	[ yourShapes
            	, cartesianPlane |> move (0,0)  
            	, pointer |> move (xCentered,yCentered)
            	, (text <| bold <| fromString <| "(x, y) = " ++ (toString (x - (width//2),y - (height//2))))
              	|> move (xCentered + 70, yCentered)
            	]
           	 
           	 
------------------ IGNORE CODE ABOVE THIS LINE -----------------------           	 
           	 
           	 
yourShapes = group
         	[
        	 
         	--insert your shapes here--
        	 
        	 
         	]
        	 

-------------------- IGNORE CODE BELOW THIS LINE ---------------------


pointer = group [ (filled red (circle 5))
        	]

cartesianPlane = group
	[    
 	move (0,0) (filled black (rect 2 650)) |> rotate (degrees 90 )    
	, move (0,0) (filled black (rect 2 650))
   	-- black cartesian arrows --
	, move (0, 255) (filled black (ngon 3 5)) |> rotate (degrees 90 )
	, move (0, -255) (filled black (ngon 3 5)) |> rotate (degrees 270 )
	, move (255, 0) (filled black (ngon 3 5))
	, move (-255, 0) (filled black (ngon 3 5))  |> rotate (degrees 180 )
   	-- grey grid lines --
	, move (50,0) (filled clearGrey (rect 2 650))
	, move (100,0) (filled clearGrey (rect 2 650))
	, move (150,0) (filled clearGrey (rect 2 650))
	, move (200,0) (filled clearGrey (rect 2 650))
	, move (250,0) (filled clearGrey (rect 2 650))
	, move (300,0) (filled clearGrey (rect 2 650))
	, move (350,0) (filled clearGrey (rect 2 650))
	, move (-50,0) (filled clearGrey (rect 2 650))
	, move (-100,0) (filled clearGrey (rect 2 650))
	, move (-150,0) (filled clearGrey (rect 2 650))
	, move (-200,0) (filled clearGrey (rect 2 650))
	, move (-250,0) (filled clearGrey (rect 2 650))
	, move (-300,0) (filled clearGrey (rect 2 650))
	, move (-350,0) (filled clearGrey (rect 2 650))
	, move (0,50) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,100) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,150) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,200) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )    
	, move (0,250) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,300) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,350) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,-50) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,-100) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,-150) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,-200) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )    
	, move (0,-250) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,-300) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
	, move (0,-350) (filled clearGrey (rect 2 650)) |> rotate (degrees 90 )
     	-- black co-ordinate ticks --
	, move (50,0) (filled black (rect 2 6))
	, move (100,0) (filled black (rect 2 6))
	, move (150,0) (filled black (rect 2 6))
	, move (200,0) (filled black (rect 2 6))
	, move (250,0) (filled black (rect 2 6))
	, move (300,0) (filled black (rect 2 6))
	, move (350,0) (filled black (rect 2 6))
	, move (-50,0) (filled black (rect 2 6))
	, move (-100,0) (filled black (rect 2 6))
	, move (-150,0) (filled black (rect 2 6))
	, move (-200,0) (filled black (rect 2 6))
	, move (-250,0) (filled black (rect 2 6))
	, move (-300,0) (filled black (rect 2 6))
	, move (-350,0) (filled black (rect 2 6))
	, move (0,50) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,100) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,150) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,200) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,250) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,300) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,350) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,-50) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,-100) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,-150) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,-200) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,-250) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,-300) (filled black (rect 2 6)) |> rotate (degrees 90 )
	, move (0,-350) (filled black (rect 2 6)) |> rotate (degrees 90 )
      	-- numbers on the plane, x axis --
	, text (fromString "0")
	, text (fromString "50") |> move (50,-20)    
	, text (fromString "100") |> move (100,-20)  	 
	, text (fromString "150") |> move (150,-20)   	 
	, text (fromString "200") |> move (200,-20)  
	, text (fromString "250") |> move (250,-20)  	 
	, text (fromString "300") |> move (300,-20)   	 
	, text (fromString "350") |> move (350,-20)   
	, text (fromString "-50") |> move (-50,-20)  
	, text (fromString "-100") |> move (-100,-20)  
	, text (fromString "-150") |> move (-150,-20)  
	, text (fromString "-200") |> move (-200,-20)
	, text (fromString "-250") |> move (-250,-20)  
	, text (fromString "-300") |> move (-300,-20)  
	, text (fromString "-350") |> move (-350,-20)  
      	-- numbers on the plane, y axis --
	, text (fromString "50") |> move (20,50)    
	, text (fromString "100") |> move (20,100)   	 
	, text (fromString "150") |> move (20,150)   	 
	, text (fromString "200") |> move (20,200)
	, text (fromString "250") |> move (20,250)   	 
	, text (fromString "300") |> move (20,300)   	 
	, text (fromString "350") |> move (20,350)  
	, text (fromString "-50") |> move (20,-50)   
	, text (fromString "-100") |> move (20,-100)   
	, text (fromString "-150") |> move (20,-150)   
	, text (fromString "-200") |> move (20,-200)
	, text (fromString "-250") |> move (20,-250)   
	, text (fromString "-300") |> move (20,-300)   
	, text (fromString "-350") |> move (20,-350)
    
	]

stopX (x,y)  = (if | x > 100	-> 100
               	| otherwise -> x  	 
           	, y)

stopY (x,y)  = (x,
           	if | y < -200	-> -200
              	| otherwise -> y
           	)

broccoli (x,y)  = (if | x > 100   -> 100
                  	| x < -100	-> -100
                  	| otherwise -> x
               	, y)

doubleSpeedX (x,y) = (if | x == 0 -> 0
                     	| otherwise -> 2*x
                 	, y)
              	 
distance (x1,y1) (x2,y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2) 	 


clearGrey : Color
clearGrey =
  rgba 111 111 111 0.3
