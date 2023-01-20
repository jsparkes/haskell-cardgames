{-
war.hs part of haskell-cardgame
Revised by packrat


Original code: Minh Vo & Vuong Khuat CS330 - Functional Programming War Project mbvo14_vdkhuat16_war.hs
Original github: https://github.com/bminhvo/War_Blackjack

-}
{-
module War where
    Haskell Notes to self: 

     https://stackoverflow.com/questions/62625481/ghc-compiles-to-o-and-hi-but-no-executable
     commented "module" out to allow exe build via:
     ghc-9.2.5.exe --make war

     vscode doesn't currently (Jan 2023) support interactive commands so the autoWar option only will run within ide need to create exe to test interactiveWar
     https://hackage.haskell.org/package/phoityne-vscode-0.0.28.0#readme

     using hints to fix code doesn't work (Jan 2023) not directly a vscode issue see:
     https://github.com/haskell/haskell-language-server/issues/3385

     stack setup didn't work there's a recent problem install on windows with permissions issues so:
     had to install git
     so had add git to path omg why doesn't the install to that????
     the did stack upgrade --git

    -}
import Cards

main = autoWar

{-
autoWar plays through a game of War without any user input.
The function displays the result of each round until the game is finished.
-}
autoWar = do
    putStrLn "Welcome to the automatic version of War!"
    putStrLn "You will see the results of all rounds below."
    playingDecks <- getPlayingDecks
    results <- playGameAuto playingDecks
    -- Check to see which player has 0 cards.
    -- The one with 0 cards loses. The other one wins.
    if length (snd results) == 0
        then putStrLn "Player 1 wins the game!"
        else putStrLn "Player 2 wins the game!"

{-
interactiveWar plays through a game of War with user input.
The function asks the player to hit any key to play their next card.
-}
interactiveWar = do
    putStrLn "Welcome to the interactive version of War!"
    putStrLn "You are player 1 (P1). Computer is player 2 (P2)."
    playingDecks <- getPlayingDecks
    -- Ask for user's input to start the game.
    -- It can be any key. Thus, user's input does not need to be checked.
    putStrLn "Press any key to start the game!"
    userInput <- getLine
    results <- playGameInteractive playingDecks
    -- Check to see which player has 0 cards.
    -- The one with 0 cards loses. The other one wins.
    if (length (snd results)) == 0
        then putStrLn "You win the game!"
        else putStrLn "Computer wins the game..."

{-
getPlayingDecks deals the cards to two players after shuffling.
-}
getPlayingDecks :: IO ([PlayingCard], [PlayingCard])
getPlayingDecks = do
    deck <- shuffledDeck
    return ([deck !! i | i <- [0..((length deck `quot` 2) - 1)]],
          [deck !! j | j <- [((length deck) `quot` 2)..(length deck - 1)]])

{-
compareCards compare the rank of two cards.
-}
compareCards :: PlayingCard -> PlayingCard -> String
compareCards card1 card2
    | fst card1 > fst card2 = "Player 1"
    | fst card1 < fst card2 = "Player 2"
    | fst card1 == fst card2 = "Same"

{-
simulateWar runs when there is a war. It takes 3 arguments: The 2 decks of the players,
and a list of cards which are participating in the war.
it returns a 2-tuple of a string which says who won the war, and a list of cards
which are all of the cards which have been in the war.
-}
simulateWar :: [PlayingCard] -> [PlayingCard] -> [PlayingCard] -> IO (String, [PlayingCard])
simulateWar p1Deck p2Deck currentCardsAtStake
    -- at the start of a war, each player must remove 3
    | length p1Deck < 4 = do
        putStrLn "P1 does not have enough cards for war."
        return ("Player 2", currentCardsAtStake ++ p1Deck ++ (take (length p1Deck) (reverse p2Deck)))
    | length p2Deck < 4 = do
        putStrLn "P2 does not have enough cards for war."
        return ("Player 1", currentCardsAtStake ++ p2Deck ++ (take (length p2Deck) (reverse p1Deck)))
    | otherwise = do
        let newAtStake = currentCardsAtStake ++ (take 4 (reverse p1Deck)) ++ (take 4 (reverse p2Deck))
        let p1NextCard = p1Deck !! (length p1Deck - 4)
        let p2NextCard = p2Deck !! (length p2Deck - 4)
        putStr ("P1 has " ++ cardToString (p1NextCard) ++ ". ")
        putStr ("P2 has " ++ cardToString (p2NextCard) ++ ". ")
        if (compareCards p1NextCard p2NextCard) == "Player 1"
            then do
                putStrLn "P1 wins.\n"
                return ("Player 1", newAtStake)
            else
                if (compareCards p1NextCard p2NextCard) == "Player 2"
                    then do
                        putStrLn "P2 wins.\n"
                        return ("Player 2", newAtStake)
                    else do
                        putStrLn "\nWARRRRR!\n"
                        putStrLn "Each player takes out three cards face down."
                        simulateWar (take ((length p1Deck) - 4) p1Deck) (take ((length p2Deck) - 4) p2Deck) newAtStake

{-
The automatic version of the game.
playGameAuto takes in a 2-tuple of the decks of two players.
It returns the final decks of two players after the game is done.
In this one, no getLine is included as the game is played through without user's input.
-}
playGameAuto :: ([PlayingCard], [PlayingCard]) -> IO ([PlayingCard], [PlayingCard])
playGameAuto (p1Deck, p2Deck)
    -- Check if either of the player has 0 cards.
    -- This means to check if either of the players wins the whole game.
    | length p1Deck == 0 || length p2Deck == 0 = do
        return (p1Deck, p2Deck)
    | otherwise = do
        let p1NextCard = last p1Deck
        let p2NextCard = last p2Deck
        putStr ("P1 has " ++ (cardToString p1NextCard) ++ ". ")
        putStr ("P2 has " ++ (cardToString p2NextCard) ++ ". ")

        -- Check if player 1 wins the round.
        if compareCards p1NextCard p2NextCard == "Player 1"
            then do
                let cardsChanged = [p1NextCard] ++ [p2NextCard]
                let newp1Deck = cardsChanged ++ init p1Deck
                let newp2Deck = init p2Deck
                putStrLn "P1 wins."
                playGameAuto (newp1Deck, newp2Deck)

            else
                -- Check if player 2 wins the round.
                if compareCards p1NextCard p2NextCard == "Player 2"
                    then do
                        let cardsChanged = [p2NextCard] ++ [p1NextCard]
                        let newp1Deck = init p1Deck
                        let newp2Deck = cardsChanged ++ init p2Deck
                        putStrLn "P2 wins."
                        playGameAuto (newp1Deck, newp2Deck)

                -- If neither wins the round, two players have cards that are the same rank.
                -- Here, the players go into War, the process which is taken care of by function simulateWar above.
                else do
                    putStrLn "\n\nWARRRRR!"
                    putStrLn "Each player takes out three cards face down."
                    warResults <- simulateWar (take (length p1Deck - 1) p1Deck) (take (length p2Deck - 1) p2Deck) [p1NextCard, p2NextCard]
                    let winner = fst warResults
                    let cardsChanged = snd warResults
                    let amountChanged = length cardsChanged
                    -- Check if player 1 wins the War.
                    if winner == "Player 1"
                        then do
                            let newp1Deck = cardsChanged ++ (take ((length p1Deck) - amountChanged `quot` 2) p1Deck)
                            let newp2Deck = take ((length p2Deck) - amountChanged `quot` 2) p2Deck
                            playGameAuto (newp1Deck, newp2Deck)
                        else do
                            let newp2Deck = cardsChanged ++ (take ((length p2Deck) - amountChanged `quot` 2) p2Deck)
                            let newp1Deck = (take ((length p1Deck) - amountChanged `quot` 2) p1Deck)
                            playGameAuto (newp1Deck, newp2Deck)

{-
The interactive version of the game.
playGameInteractive takes in a 2-tuple of the decks of two players.
It returns the final decks of two players after the game is done.
In this one, getLine is included as the next card is only played after user's input.
-}
playGameInteractive :: ([PlayingCard], [PlayingCard]) -> IO ([PlayingCard], [PlayingCard])
playGameInteractive (p1Deck, p2Deck)
    -- Check if either of the player has 0 cards.
    -- This means to check if either of the players wins the whole game.
    | length p1Deck == 0 || length p2Deck == 0 = do
        return (p1Deck, p2Deck)
    | otherwise = do
        let p1NextCard = last p1Deck
        let p2NextCard = last p2Deck
        putStr ("P1 has " ++ (cardToString p1NextCard) ++ ". ")
        putStr ("P2 has " ++ (cardToString p2NextCard) ++ ". ")

        -- Check if player 1 wins the round.
        if compareCards p1NextCard p2NextCard == "Player 1"
            then do
                let cardsChanged = [p1NextCard] ++ [p2NextCard]
                let newp1Deck = cardsChanged ++ init p1Deck
                let newp2Deck = init p2Deck
                putStrLn "P1 wins."

                -- Ask for user's input to play the next card/start the next round.
                -- It can be any key. Thus, user's input does not need to be checked.
                putStrLn "Press any key to start the next round!"
                userInput <- getLine

                playGameInteractive (newp1Deck, newp2Deck)

            else
                -- Check if player 2 wins the round.
                if compareCards p1NextCard p2NextCard == "Player 2"
                    then do
                        let cardsChanged = [p2NextCard] ++ [p1NextCard]
                        let newp1Deck = init p1Deck
                        let newp2Deck = cardsChanged ++ init p2Deck
                        putStrLn "P2 wins."

                        -- Ask for user's input to play the next card/start the next round.
                        -- It can be any key. Thus, user's input does not need to be checked.
                        putStrLn "Press any key to start the next round!"
                        userInput <- getLine

                        playGameInteractive (newp1Deck, newp2Deck)

                    else do
                        -- If neither wins the round, two players have cards that are the same rank.
                        -- Here, the players go into War, the process which is taken care of by function simulateWar above.
                        putStrLn "\n\nWARRRRR!"
                        putStrLn "Each player takes out three cards face down."
                        warResults <- simulateWar (take (length p1Deck - 1) p1Deck) (take (length p2Deck - 1) p2Deck) [p1NextCard, p2NextCard]
                        let winner = fst warResults
                        let cardsChanged = snd warResults
                        let amountChanged = length cardsChanged
                        -- Check if player 1 wins the War.
                        if winner == "Player 1"
                            then do
                                let newp1Deck = cardsChanged ++ (take ((length p1Deck) - amountChanged `quot` 2) p1Deck)
                                let newp2Deck = take ((length p2Deck) - amountChanged `quot` 2) p2Deck

                                putStrLn "Press any key to start the next round!"
                                userInput <- getLine

                                playGameInteractive (newp1Deck, newp2Deck)
                            else do
                                let newp2Deck = cardsChanged ++ (take ((length p2Deck) - amountChanged `quot` 2) p2Deck)
                                let newp1Deck = (take ((length p1Deck) - amountChanged `quot` 2) p1Deck)

                                -- Ask for user's input to play the next card/start the next round.
                                -- It can be any key. Thus, user's input does not need to be checked.
                                putStrLn "Press any key to start the next round!"
                                userInput <- getLine

                                playGameInteractive (newp1Deck, newp2Deck)
