-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Parser.Instances
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Rummy.Rules
import Data.List
import Data.Ord

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
{- type ActionFunc
  = Card            -- ^ card on top of the discard pile
  -> (Score, Score) -- ^ scores of (player, opponent) as of last round
  -> Maybe String
  -- ^ player's memory, on first player turn in the first round it will be Nothing
  -> Maybe Draw -- ^ opponent's chosen action, on first game turn it will be Nothing
  -> [Card]     -- ^ the player's hand
  -> (Draw, String) -- ^ which pile did the player chose to draw from and memory -}
pickCard :: ActionFunc
pickCard card s Nothing o h = pickCard card s (Just (show mem)) o h -- Create a default memory and re-call pickCard
    where
        melds = findOptimalMelds h
        mem = Memory h (primaryCardsNeeded melds) (secondaryCardsNeeded melds) [] melds s 0

pickCard card s (Just memory) opp h
    | Player.score mem /= s = pickCard card s Nothing opp h  -- Check if a new game has started
    | otherwise = (choice, show (mem {cardsSeen = card : cardsSeen mem, discard = updateDisc, turnCounter = 1 + turnCounter mem}))
        where
            unwantedCards = length (filterByList (concat $ [cardsSeen, cardsWanted, secCardsWanted] <*> [mem]) allCards)  -- Generate unwanted cards in stack
            mem = extractMemory memory
            updateDisc -- Check if the opponent picked up the card from the most recent discard pile
                | null (discard mem) = [card]
                | oppDiscard opp = tail (discard mem)
                | head (discard mem) == card = discard mem
                | otherwise = card : discard mem
            oppDiscard (Just Discard) = True
            oppDiscard _ = False
            choice -- Makes a basic choice for whether the likelihood of getting a card that would extend a meld is greater than chance of getting an unwanted card
                | card `elem` cardsWanted mem = Discard -- cardsWanted is cards that would instantly produce or extend a meld
                | 2 * unwantedCards > length (cardsWanted mem) && card `elem` secCardsWanted mem = Discard -- secCardsWanted is cards that would contribute towards existing melds
                | otherwise = Stock

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
{- type PlayFunc
  = Card              -- ^ picked card
  -> (Score, Score)   -- ^ scores of (player, opponent) as of last round
  -> String           -- ^ the player's memory
  -> [Card]           -- ^ the player's hand (without new card)
  -> (Action, String) -- ^ the player's chosen card and new memory -}
playCard :: PlayFunc
playCard c _ memory h -- Knocks and calls Gin whenever possible
    | gin = (Action Gin worst, show mem)
    | knock = (Action Knock worst, show mem)
    | otherwise = (Action Drop worst, show mem)
        where
            oldMem = extractMemory memory -- Old version of memory before updating
            -- Get cards of deadwood cards from given list of melds
            deads melds = concat $ meldCards <$> filter isDeadwood melds
            -- Get cards of deadwood cards from melds of hand including new card
            newDeads = filter (c/=) (deads $ findOptimalMelds (c : h))

            maxBefore = maxCard (deads (currMelds oldMem)) -- Max deadwood card in old hand
            maxAfter = maxCard $ filterNotNear newDeads -- Max deadwood card in new hand that isn't near a meld

            filterNotNear = filter (null . checkIfOneOff) -- Deadwood cards that aren't 1 card away from being a meld
            checkIfOneOff card = filter (\l -> (1 == length l) && not (all (`elem` cardsSeen oldMem) l)) (nearMeld [Deadwood card])

            worst = check maxAfter maxBefore -- check to see if maxAfter was found, else if maxBefore was found
                where 
                    check (Just v) _ = v
                    check Nothing (Just v) = v
                    check _ _ = minimum h

            -- Generate items to update memory
            newHand = c : filter (worst/=) h
            newMelds = findOptimalMelds newHand

            newCardsWanted = filterByList (cardsSeen oldMem) (primaryCardsNeeded newMelds) -- cards needed that are 1 removed from melds
            newSecCardsWanted = filterByList (cardsSeen oldMem) (secondaryCardsNeeded newMelds) -- cards needed that are 2 removed from melds

            gin = not (any isDeadwood newMelds) && turnCounter oldMem > 1  -- Check if can call gin
            knock = sum (cardPoints <$> newMelds) < 10 && turnCounter oldMem > 1 -- Check if can call knock
            mem = oldMem {cardsWanted = newCardsWanted, secCardsWanted = newSecCardsWanted, currMelds = newMelds, discard = tail $ discard oldMem}

-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
{- type MeldFunc
  = (Score, Score) -- ^ scores of (player, opponent) as of last round
  -> String        -- ^ the player's memory
  -> [Card]        -- ^ cards in player's hand
  -> [Meld]        -- ^ elected melds -}
makeMelds :: MeldFunc
makeMelds _ memory _ = currMelds $ extractMemory memory

-- Find max rank card
maxCard :: [Card] -> Maybe Card
maxCard [] = Nothing
maxCard l = Just $ maximumBy (comparing rank) l

-- All possible cards
allCards :: [Card]
allCards = [Card s n | s <- [Spade .. Heart], n <- [Ace .. King]]

-------- CARD PICKING FUNCTIONS -------
-- Takes filtering condition, list of list of cards, and list of melds, and flattens, removes duplicates, and sorts them
-- Only keeps the lists that follow the condition passed in
filterCardsNeeded :: (Ord a, Eq a) => ([a] -> Bool) -> [[a]] -> [a]
filterCardsNeeded = ((sort . nub . concat) .) . filter

-- Pass in a list of melds to find all cards needed that would extend current melds if 1 more card was gievn
primaryCardsNeeded :: [Meld] -> [Card]
primaryCardsNeeded m =  filterCardsNeeded ((1==) . length) (nearMeld m)

-- Pass in a list of melds to find all cards needed that would extend current melds if 2 more cards were given
secondaryCardsNeeded :: [Meld] -> [Card]
secondaryCardsNeeded m = filterCardsNeeded (const True) (findComplementCards <$> filterOutDeads m)

-- Find cards which would extend current melds by one, doesn't handle deadwood extensions
findComplementCards :: Meld -> [Card]
findComplementCards (Set3 a b c) = completeSet [a, b, c]
findComplementCards (Straight3 a _ c) = adjCards a c
findComplementCards (Straight4 a _ _ d) = adjCards a d
findComplementCards (Straight5 a _ _ _ e) = adjCards a e
findComplementCards _ = []

-- Takes in a list of melds
-- Returns cards needed to push deadwoods of those melds into a set or straight
nearMeld :: [Meld] -> [[Card]]
nearMeld melds = sortOn length $ filter (not . null) $ nearSet ++ nearStraight
    where
        deads = concat $ meldCards <$> filter isDeadwood melds
        nearMeldType accF f = foldr accF [] $ f deads
        nearStraight = completeStraight <$> nearMeldType aggregateStraight sort -- Find cards which are 1 off a straight
        nearSet = completeSet <$> nearMeldType aggregateSet (sortOn suit) -- Find cards which are 1 off a set

-- Takes in a given card and a list containing near sequential straights
-- If the card is same suit and sequential or 1 off being sequential, it will add it to the most recent list
-- If doesn't match the above conditions, it creates a new list to work off
aggregateStraight :: Card -> [[Card]] -> [[Card]]
aggregateStraight c [] = [[c]]
aggregateStraight c (x:xs)
    | match c (head x) = (c : x) : xs
    | otherwise = [c]:x:xs
        where
            match c1 c2 = suit c1 == suit c2 && (succ (rank c1) == rank c2 || succ (rank c1) == pred (rank c2))

-- Takes in a given card and a list containing sets
-- If the card is same suit and rank, it will add it to the most recent list
-- If doesn't match the above conditions, it creates a new list to work off
aggregateSet :: Card -> [[Card]] -> [[Card]]
aggregateSet c [] = [[c]]
aggregateSet c (x:xs)
    | rank c == rank (head x) = (c : x) : xs
    | otherwise = [c]:x:xs

-- Find adjacent cards bound by limits of Bounded
adjCards :: Card -> Card -> [Card]
adjCards a b 
    | rank a == Ace = [Card (suit a) ((succ . rank) b)]
    | rank b == King = [Card (suit a) ((pred . rank) a)]
    | otherwise = [Card (suit a) ((pred . rank) a), Card (suit a) ((succ . rank) b)]

-- Find rest of cards needed for the set
completeSet :: [Card] -> [Card]
completeSet [] = []
completeSet c = [Card x (rank (head c)) | x <- filterByList (suit <$> c) [Spade .. Heart]]

-- Find rest of cards needed for the straight
completeStraight :: [Card] -> [Card]
completeStraight [] = []
completeStraight l = filterByList l [Card (suit (head l)) x | x <- [(rank (head l)) .. (rank (last l))]]

-------- MELD FINDING FUNCTIONS --------
-- The optimal meld finding is using a back tracking algorithm adapted from psuedocode given https://stackoverflow.com/questions/62707039/gin-rummy-algorithm-for-determining-optimal-melding for haskell
data MeldNode = MeldNode {
    cardsUsed :: [Card],
    meldUsed :: Maybe Meld,
    value :: Int,
    parent :: Maybe MeldNode
}

-- First argument is parent of the node, second argument is the meld it will represent
-- Creates a MeldNode out of the two given arguments that represents a MeldNode in the tree
newMeldNode :: Maybe MeldNode -> Meld -> MeldNode
newMeldNode Nothing m = MeldNode {cardsUsed = meldCards m, meldUsed = Just m, parent = Nothing, value = sum $ toPoints <$> meldCards m}
newMeldNode (Just p) m = MeldNode {cardsUsed = meldCards m, meldUsed = Just m, parent = Just p, value = sum (toPoints <$> meldCards m) + value p}

-- Inspiration from https://stackoverflow.com/questions/55053785/how-to-check-for-an-empty-intersection-of-lists-with-haskell
-- Generalised out to filterByList below, however custom version is needed to extract cards from melds before checking
cleanMelds :: [Meld] -> [Card] -> [Meld] 
cleanMelds melds cards = filter (\m -> null [x | x <- meldCards m, x `elem` cards]) melds 

-- Recursively finds the optimal nodes by constructing a backtracking tree
-- First argument is all melds that need to be considered, second argument is the parent node
getBestNode :: [Meld] -> Maybe MeldNode -> MeldNode
getBestNode [] Nothing = MeldNode [] Nothing 0 Nothing
getBestNode [] (Just n) = n
getBestNode (m:ms) n = maximumBy (comparing value) [getBestNode ms n, getBestNode (cleanMelds ms (meldCards m)) $ Just $ newMeldNode n m]

-- Finds all possible combinations of melds out of a given hand, excluding Deadwoods
findMelds :: [Card] -> [Meld]
findMelds h = filterOutDeads $ straightMelds ++ setMelds
    where
        setMelds = foldl makeSet [] (sortOn rank h)
        straightMelds = foldl makeStraight [] (sort h)

-- Given a starting node, extracts all melds used by it and it's parents
getMeldsFromNode :: Maybe MeldNode -> [Meld]
getMeldsFromNode Nothing = []
getMeldsFromNode (Just m) = maybe rest (:rest) (meldUsed m)
    where
        rest = getMeldsFromNode (parent m)

-- Finds the optimal melds for a given hand, and converts any unused cards to deadwood
findOptimalMelds :: [Card] -> [Meld]
findOptimalMelds h = deadwoodRest h $ getMeldsFromNode $ Just $ getBestNode ((findMelds . sort) h) Nothing

-- Given a set of melds and a hand, converts any cards that aren't used in the melds into deadwoods
deadwoodRest :: [Card] -> [Meld] -> [Meld]
deadwoodRest h melds = melds ++ (Deadwood <$> filter (not . (`elem` meldCardsUsed)) h)
    where
        meldCardsUsed = concat $ meldCards <$> melds

-- Filter a list by another list
filterByList :: (Foldable t, Eq a) => t a -> [a] -> [a]
filterByList a = filter (`notElem` a)

-- Removes any deadwoods from the list of melds
filterOutDeads :: [Meld] -> [Meld]
filterOutDeads = filter (not . isDeadwood)

-- Checks if a given meld is a Deadwood
isDeadwood :: Meld -> Bool
isDeadwood (Deadwood _) = True
isDeadwood _ = False

-- Extracts cards from meld
meldCards :: Meld -> [Card]
meldCards m = case m of
    Deadwood a -> [a]
    Set3 a b c -> [a, b, c]
    Set4 a b c d -> [a, b, c, d]
    Straight3 a b c -> [a, b, c]
    Straight4 a b c d -> [a, b, c, d]
    Straight5 a b c d e -> [a, b, c, d, e]

-- Makes all straights where possible when given a new card from a hand
makeStraight :: [Meld] -> Card -> [Meld]
makeStraight h card = case meld of
    Just (Deadwood a)
        | consecutive a card && length h > 1 && consecutive prev a -> [Straight3 prev a card] ++ [Deadwood card] ++ h
    Just (Straight3 a b c) 
        | consecutive c card -> [Straight4 a b c card, Straight3 b c card] ++ [Deadwood card] ++ h
    Just (Straight4 a b c d )
        | consecutive d card -> [Straight5 a b c d card, Straight4 b c d card, Straight3 c d card] ++ [Deadwood card] ++ h
    Just (Straight5 _ b c d e)
        | consecutive e card -> [Straight5 b c d e card, Straight4 c d e card, Straight3 d e card] ++ [Deadwood card] ++ h 
    _ -> Deadwood card : h
    where
        meld | (not . null) h = Just $ head h
             | otherwise = Nothing
        prev = getCard (head (tail h))
        getCard a = (head . meldCards) a
        consecutive a b = length [rank a .. rank b] == 2 && suit a == suit b

-- Makes all sets where possible when given a new card from a hand
makeSet :: [Meld] -> Card -> [Meld]
makeSet h card = case meld of
    Just (Deadwood a)
        | length h > 1 && sameRank [a, prev] -> [Set3 prev a card] ++ [Deadwood card] ++ h 
    Just (Set3 a b c) 
        | sameRank [a, b, c] -> [Set3 a c card, Set3 a b card, Set3 b c card, Set4 a b c card] ++ [Deadwood card] ++ h
    _ -> Deadwood card : h
    where
        meld | (not . null) h = Just $ head h
             | otherwise = Nothing
        sameRank = same rank card
        prev = getCard (head (tail h))
        getCard a = (head . meldCards) a

suit :: Card -> Suit
suit (Card a _) = a

rank :: Card -> Rank
rank (Card _ b) = b

-------- SHOW INSTANCES --------
instance Show Draw where
    show Stock = "S"
    show Discard = "D"

instance Show Action where
    show (Action a b) = show a ++ " " ++ show b

instance Show Card where
    show (Card a b) = show a ++ " " ++ show b

instance Show Suit where
    show Spade = "s"
    show Club = "c"
    show Diamond = "d"
    show Heart = "h"

instance Show Rank where
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show v = show (fromEnum v)

instance Show Meld where
    show (Deadwood a) = "D " ++ show a          -- An unmelded card
    show (Set3 a b c) = "SE " ++ showCards [a, b, c]     -- 3 cards of same rank different suit
    show (Set4 a b c d) = "SE " ++ showCards [a, b, c, d] -- 4 cards of same rank different suit
    show (Straight3 a b c) = "ST " ++ showCards [a, b, c] -- 3 cards of same suit, sequential ranks
    show (Straight4 a b c d) = "ST " ++ showCards [a, b, c, d] -- 4 cards of same suit, sequential ranks
    show (Straight5 a b c d e) = "ST " ++ showCards [a, b, c, d, e] -- 5 cards of same suit, sequential ranks

instance Show Memory where
    show (Memory a b c d f g h) = concat $ (show <$> [a, b, c, d]) ++ [show f, show g, show h]

showCards :: [Card] -> String
showCards l = unwords $ show <$> l

-------- PARSER AND MEMORY FUNCTIONS --------
data Memory = Memory 
    { cardsSeen :: [Card]
    , cardsWanted :: [Card]
    , secCardsWanted :: [Card]
    , discard :: [Card]
    , currMelds :: [Meld]
    , score :: (Int, Int)
    , turnCounter :: Int
    }
-- Functions taken from previous tutes and notes

-- Parses a digit
digit :: Parser Char -- Inspiration taken from parser notes
digit = is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

-- Parses 0 or more spaces
spaces :: Parser () -- Inspiration taken from parser notes
spaces = (is ' ' >> spaces) ||| pure ()

-- Applies the first parser, runs the third parser keeping the result,
-- then runs the second parser and produces the obtained result.
between :: Parser o -> Parser c -> Parser a -> Parser a -- Taken from JSON parser tute
between p1 p2 p3 = do
  _ <- p1
  x <- p3
  _ <- p2
  return x

-- Applies the given parser in between the two given characters.
betweenCharTok :: Char -> Char -> Parser a -> Parser a -- Taken from JSON parser tute
betweenCharTok a b = between (charTok a) (charTok b)

-- Applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
tok :: Parser a -> Parser a -- Taken from Week 11 parser tute
tok p = do
    r <- p
    spaces
    return r

-- Parses the given char followed by 0 or more spaces
charTok :: Char -> Parser Char -- Taken from Week 11 parser tute
charTok = tok . is

-- Parses a comma ',' followed by 0 or more spaces.
commaTok :: Parser Char -- Taken from Week 11 parser tute
commaTok =  charTok ','

-- | Produces a non-empty list of values coming off the
-- given parser (which must succeed at least once), separated by the second
-- given parser.
sepby1 :: Parser a -> Parser s -> Parser [a] -- Taken from Week 11 parser tute
sepby1 a v =  do
    x <- a 
    y <- list (v >> a)
    return (x:y)

-- | Produces a list of values coming off the given
-- parser, separated by the second given parser.
sepby :: Parser a -> Parser s -> Parser [a] -- Taken from Week 11 parser tute
sepby a v = sepby1 a v ||| pure []

-- Returns a parser that continues producing a list of values from the given
-- parser.
list :: Parser a -> Parser [a] -- Taken from Week 11 parser tute
list p = (do
    p' <- p
    p'' <- list p
    pure (p':p'')) ||| pure []

-- Parses the given string (fails otherwise)
string :: String -> Parser String -- Taken from Week 11 parser tute
string = traverse is

-- Returns a parser that produces a character but fails if doesn't match predicate or is empty
satisfy :: (Char -> Bool) -> Parser Char -- Taken from Week 11 parser tute
satisfy f = character >>= \c ->
        if f c then pure c
        else unexpectedCharParser c

-- Write a function that parses one of the characters in the given string.
oneof :: String -> Parser Char -- Taken from Week 11 parser tute
oneof s = satisfy (`elem` s)

-- Parses any character, but fails if it is in the given string.
noneof :: String -> Parser Char -- Taken from Week 11 parser tute
noneof s = satisfy (`notElem` s)

betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a] -- Taken from JSON parser tute
betweenSepbyComma a b p = betweenCharTok a b (sepby p commaTok)

-- Card specific functions
-- Suit Parser
parseSuit:: Parser Suit
parseSuit = parseSpade ||| parseClub ||| parseDiamond ||| parseHeart
    where
        parseSpade = is 's' >> pure Spade
        parseClub = is 'c' >> pure Club
        parseDiamond = is 'd' >> pure Diamond
        parseHeart = is 'h' >> pure Heart

-- Rank Parser
parseRank :: Parser Rank
parseRank = digit ||| is 'J' ||| is 'Q' ||| is 'K' >>= getRank
    where
        getRank 'J' = pure Jack
        getRank 'Q' = pure Queen
        getRank 'K' = pure King
        getRank i = (pure . toEnum . read) [i]

-- Card Parser
parseCard :: Parser Card
parseCard = do
    s <- parseSuit
    spaces
    Card s <$> parseRank

-- List of cards parser
listOfCards :: Parser [Card]
listOfCards = betweenSepbyComma '[' ']' parseCard ||| pure []

-- Straight parser
parseStraight :: Parser Meld
parseStraight = do
    _ <- string "ST"
    spaces
    cards <- sepby parseCard spaces
    return $ straightFrom cards
        where
            straightFrom [a, b, c] = Straight3 a b c
            straightFrom [a, b, c, d] = Straight4 a b c d
            straightFrom [a, b, c, d, e] = Straight5 a b c d e
            straightFrom _ = error "Invalid straight in memory"

-- Set parser
parseSet :: Parser Meld
parseSet = do
    _ <- string "SE"
    spaces
    cards <- sepby parseCard spaces
    return $ setFrom cards
        where
            setFrom [a, b, c] = Set3 a b c
            setFrom [a, b, c, d] = Set4 a b c d
            setFrom _ = error "Invalid set in memory"

-- Deadwood parser
parseDeadwood :: Parser Meld
parseDeadwood = do
    _ <- charTok 'D'
    Deadwood <$> parseCard

-- Meld Parser
parseMeld :: Parser Meld
parseMeld = parseStraight ||| parseSet ||| parseDeadwood

-- List of melds parser
listOfMelds :: Parser [Meld]
listOfMelds = betweenSepbyComma '[' ']' parseMeld ||| pure []

-- Score parser
parseScore :: Parser (Int, Int)
parseScore = do
    s <- betweenSepbyComma '(' ')' (list digit)
    return (getInt (head s), getInt (head (tail s)))

-- String to Int converter
getInt :: String -> Int
getInt s = getNum $ readInt s
    where
        getNum Nothing = error "invalid number formatting"
        getNum (Just (v, _)) = v 

-- Memory parser
parseMemory :: Parser Memory
parseMemory = do
    a <- listOfCards
    b <- listOfCards
    c <- listOfCards
    d <- listOfCards
    m <- listOfMelds
    s <- parseScore
    t <- list digit
    return $ Memory a b c d m s (getInt t)

-- Takes in a string of memory and attempts to return a memory object
extractMemory :: String -> Memory
extractMemory s = extractResult $ parse parseMemory s
    where
        extractResult (Result _ x) = x
        extractResult x = error ("malformed memory:\n" ++ s ++ "\n" ++ show x)