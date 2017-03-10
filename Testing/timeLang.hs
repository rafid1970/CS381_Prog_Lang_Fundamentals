type Hour = Int
type Minute = Int

data Time = Midnight
          | Noon
          | AM Hour
          | PM Hour
          | After Minute Time
          | Before Minute Time

checkHour :: Int -> Bool
checkHour h = h >= 0 && h <= 12

time :: Time -> Maybe Int
time (Midnight) = Just 0
time (Noon) = Just (12 * 60)
time (AM h) = case checkHour h of
              True -> Just (60 * h)
              False -> Nothing
time (PM h) = case checkHour h of
              True -> Just (60 * h + 12 * 60)
              False -> Nothing
time (After m t) = case time t of
                  Just i ->  Just (i + m )
                  _      -> Nothing
time (Before m t) = case time t of
                   Just i -> Just (i - m)
                   _      -> Nothing
