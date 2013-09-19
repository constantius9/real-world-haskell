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
safeInit (h:t) = fmap (h : ) (safeInit t)

splitWith p l =
	reverse $ accumulate [] [] p l

accumulate a1 a2 p [] =
	if a2 /= []
		then reverse a2 : a1
		else a1
accumulate a1 a2 p (h:t) =
	if p h
		then accumulate a1 (h:a2) p t
		else
			if a2 /= []
				then accumulate (reverse a2 : a1) [] p t
				else accumulate a1 [] p t