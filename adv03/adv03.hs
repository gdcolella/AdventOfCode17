data Direction = L | R | U | D deriving(Show)
data Point = Point {
    x :: Int,
    y :: Int,
    index :: Int,
    sideLength :: Int,
    sideProgress :: Int,
    direction :: Direction
} deriving (Show)


updateDirection p1 = case direction p1 of
    L -> p1 { direction = D}
    R -> p1 { direction = U}
    U -> p1 { direction = L, sideLength = 1 + sideLength p1}
    D -> p1 { direction = R, sideLength = 1 + sideLength p1}

inc :: Point -> Point
inc p1 = do
    let m1 = if sideProgress p1 == sideLength p1
        then
            let updated = updateDirection p1
            in updated { sideProgress = 0}
        else
            p1

    let m2 = case direction m1 of
            L -> m1 {x = x m1 - 1}
            R -> m1 {x = x m1 + 1}
            U -> m1 {y = y m1 + 1}
            D -> m1 {y = y m1 - 1}
    let m3 = m2 { sideProgress = sideProgress m2 + 1, index = index m2+1}
    m3

spiral = Point{
    x=0,y=0,index=1,sideLength=1,sideProgress=0,direction=R} : map inc spiral

dist index = 
    let pt = spiral !! (index-1)
    in (abs (x pt)) + (abs (y pt))

main = do
    --print $ take 6 spiral
    print $ dist 289326