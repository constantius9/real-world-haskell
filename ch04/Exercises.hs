safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:_) = Just h

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:t) = Just t

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [a] = Just a
safeLast (_:t) = safeLast t

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (h:a:[]) = Just [h]
safeInit (h:t) = fmap ([h] ++ ) (safeInit t)
