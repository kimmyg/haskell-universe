import Graphics.Gloss.Interface.Pure.Game

data Game = Game Int Int Phase

data Phase = Play Float Float Point Point

pp t = if (t < -15 || t >= 15) then [(ThickArc (t-15) (t+15) 360 8)] else [ThickArc (t-15) 359.99 360 8,ThickArc 0 (t+15) 360 8]

atan2' y x = let t = atan2 y x in if t < (-pi)/2 then t + pi * 2  else t

drawPhase (Play p0 p1 (x,y) (vx,vy)) = pictures ([Color (greyN 0.25) (ThickCircle 360 8)]
                 		   	         ++(map (color white) (pp p0))
					         ++(map (color white) (pp p1))
					         ++[color white (Translate x y (circleSolid 8)),
					            color red (text (show p1))])
					
draw (Game _ _ p) = drawPhase p

eventPhase (EventMotion (sx,sy)) (Play p0 p1 (x,y) (vx,vy)) = Play (max 105 (min (180-sy/800*150) 255)) (max (-75) (min (sy/800*150) 75)) (x,y) (vx,vy)
eventPhase _ p = p


event e (Game s0 s1 p) = Game s0 s1 (eventPhase e p)

--event (EventMotion (sx,sy)) (Game s0 s1 p0 p1 (x,y) (vx,vy)) = World (max (-75) (min ((atan2 sy sx)*57.29578) 75)) (max 105 (min ((atan2' sy sx)*57.29578) 255)) (x,y) (vx,vy)
--event (EventMotion (sx,sy)) (World p0 p1 (x,y) (vx,vy)) = World ((400-sy)/400*75) sy (x,y) (vx,vy)
--event _ w = w

updatePhase dt (Play p0 p1 (x,y) (vx,vy)) = let (x,y) = (x+dt*vx,y*dt*vy) in (Play p0 p1 (x,y) (vx,vy))
       	  	       	     	              --let m = (mag x y) in
					        --if m < (400-8) then (Play p0 p1 (x,y) (vx,vy)) else (Play p0 p1 (x*m/400,y*m/400) (0,0))
						  --where mag x y = sqrt( x*x + y*y )

update dt (Game s0 s1 p) = Game s0 s1 (updatePhase dt p)

main = play (InWindow "Test" (800,800) (32,32))
            black
            1
	    (Game 0 0 (Play 180 0 (0,0) (10,10)))
	    draw
	    event
	    update
