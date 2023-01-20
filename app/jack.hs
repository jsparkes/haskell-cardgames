{-
blackjack.hs
Revised by packrat

Original code: Minh Vo & Vuong Khuat CS330 - Functional Programming Blackjack Project mbvo14_vdkhuat16_jack.hs
Original github: https://github.com/bminhvo/War_Blackjack

-}

module Blackjack where
import Cards

main = blackjack

{-
blackjack is the primary function to run the game.
The function is divided into 4 phases:
startPhase: In this phase, user and dealer are dealt their first two cards;
userPhase: In this phase, check for Blackjack, deal additional card(s) to user
           until they bust or choose to stand.
dealPhase: In this phase, check for Blackjack, deal additional card(s) to dealer
           until they bust or the sum of their cards is larger than 16.
comparePhase: In this phase, if neither has Blackjack or has busted, compare
           the best value of the sum of their cards to determine the winner (or a tie).
Please refer to Design Assignment 7.4 to understand
the overall design of the program, which is also the representation
of the steps in this function.
-}
blackjack = do
    deck <- shuffledDeck
    startResults <- startPhase deck
    userResults <- userPhase startResults
    if someoneHasBlackjack userResults
        then putGameOverNotice
        else
            if not (someoneBusts userResults)
                then do
                    dealerResults <- dealerPhase userResults
                    if someoneHasBlackjack dealerResults
                        then putGameOverNotice
                        else
                            if not (someoneBusts dealerResults)
                                then do
                                    compareResults <- comparePhase dealerResults
                                    putStrLn(compareResults)
                                    putGameOverNotice
                                else putGameOverNotice
                else putGameOverNotice

{-
startPhase deals two cards to user and dealer.
It takes one argument: the full deck of cards.
It Returns a 4-tuple of a new deck after cards are dealt, the cards of the player,
the cards of the dealer, and the initial burst condition for the player's hand
before actually checking in the userPhase.
-}
startPhase :: [PlayingCard] -> IO ([PlayingCard], [PlayingCard], [PlayingCard], Bool)
startPhase deck = do
    putStrLn("Dealing 2 cards each to player and dealer")
    let playerHand = [deck !! 51, deck !! 49]
    let dealerHand = [deck !! 50, deck !! 48]

    putStrLn("Your initial cards are " ++ cardToString (playerHand !! 0)
            ++ " and " ++ cardToString (playerHand !! 1))
    putStrLn("Dealer's initial cards are " ++ cardToString (dealerHand !! 0)
            ++ " and one face-down card.")

    return (take 48 deck, playerHand, dealerHand, False)

{-
userPhase takes care of the actions needed to do for the user.
It takes one argument: the 4-tuple that is returned from the
startPhase function above.
It returns a 5-tuple of a new deck after card(s) is(are) dealt to the user,
the player's cards after the user finishes their turn,
the dealer's cards, the Boolean for whether the player's hand busts,
the Boolean for whether the player's hand is Blackjack.
-}
userPhase :: ([PlayingCard], [PlayingCard], [PlayingCard], Bool) -> IO ([PlayingCard], [PlayingCard], [PlayingCard], Bool, Bool)
userPhase (newDeck, playerHand, dealerHand, hasBusted) = do
    -- Check if the user has Blackjack
    if blackjackHand playerHand
        then do
            putStrLn("You have Blackjack!")
            -- Check if the dealer has Blackjack
            if blackjackHand dealerHand
                then do
                    putStrLn("Dealer's initial cards are " ++ cardToString (dealerHand !! 0)
                                    ++ " and " ++ cardToString (dealerHand !! 1))
                    putStrLn("Dealer also has Blackjack...")
                    putStrLn("It is a tie.")
                    return (newDeck, playerHand, dealerHand, hasBusted, blackjackHand playerHand)
                else do
                    putStrLn("Dealer's initial cards are " ++ cardToString (dealerHand !! 0)
                                    ++ " and " ++ cardToString (dealerHand !! 1))
                    putStrLn("Dealer does not have Blackjack!")
                    putStrLn("You win!")
                    putStrLn("Dealer loses!")
                    return (newDeck, playerHand, dealerHand, hasBusted, blackjackHand playerHand)

        else
            -- Check if the user's hand has busted
            if not hasBusted
                then do
                    putStrLn("Do you want to hit?")
                    putStrLn("Please enter Y (or y) to hit OR any other key to stand!")
                    userInput <- getLine
                    if userInput /= "Y" && userInput /= "y"
                        then do
                            putStrLn("Dealer's current cards are " ++ cardToString (dealerHand !! 0)
                                    ++ " and " ++ cardToString (dealerHand !! 1))
                            return (newDeck, playerHand, dealerHand, hasBusted, blackjackHand playerHand)
                        else do
                            let newPlayerHand = playerHand ++ [last newDeck]
                            putStrLn("Your current hand is " ++ handToString newPlayerHand)
                            userPhase (init newDeck, newPlayerHand, dealerHand, bust newPlayerHand)
                else do
                    putStrLn("The sum of your cards is more than 21.")
                    putStrLn("You bust...")
                    putStrLn("Dealer wins...")
                    return (newDeck, playerHand, dealerHand, hasBusted, blackjackHand playerHand)

{-
dealerPhase takes care of the actions needed to do for the dealer.
It takes one argument: the 5-tuple that is returned from the userPhase function above.
It returns a 5-tuple of a new deck after card(s) is(are) dealt to the dealer,
the player's cards (not important), the dealer's cards after the dealer is dealt
all cards possible, the Boolean for whether the dealer's hand busts,
the Boolean for whether the dealer's hand is Blackjack.
-}
dealerPhase :: ([PlayingCard], [PlayingCard], [PlayingCard], Bool, Bool) -> IO ([PlayingCard], [PlayingCard], [PlayingCard], Bool, Bool)
dealerPhase (newDeck, playerHand, dealerHand, hasBusted, hasBlackJack) = do
    -- Check if the dealer has Blackjack
    if blackjackHand dealerHand
        then do
            putStrLn("Dealer has Blackjack...")
            putStrLn("You lose...")
            putStrLn("Dealer wins...")
            return (newDeck, playerHand, dealerHand, hasBusted, blackjackHand dealerHand)

        else do
            -- Check if the dealer's hand has busted
            if not hasBusted
                then do
                    if (bestValue (getHandSum dealerHand)) >= 17
                        then do return (newDeck, playerHand, dealerHand, hasBusted, blackjackHand dealerHand)
                        else do
                            putStrLn("Dealer's hand sums up to less than 17")
                            putStrLn("Dealer has to draw another card.\n")
                            let newDealerHand = dealerHand ++ [last newDeck]
                            putStrLn("Dealer's current cards are " ++ handToString newDealerHand)
                            dealerPhase (init newDeck, playerHand, newDealerHand, bust newDealerHand, blackjackHand newDealerHand)
                else do
                    putStrLn("The sum of dealer's cards is more than 21.")
                    putStrLn("Dealer busts!")
                    putStrLn("You win!")
                    return (newDeck, playerHand, dealerHand, hasBusted, blackjackHand dealerHand)

{-
comparePhase takes in a 5-tuple that is returned from dealerPhase
and returns a String of the result of the game.
Then, it comepares the bestValue for the sum of the player's hand
and the bestValue for the sum of the dealer's hand
to determine the result of the game.
-}
comparePhase :: ([PlayingCard], [PlayingCard], [PlayingCard], Bool, Bool) -> IO String
comparePhase (newDeck, playerHand, dealerHand, hasBusted, hasBlackJack)
    | playerSum > dealerSum = do return (resultString ++ "\nYou have the higher hand!\nYou win!\nDealer loses!")
    | playerSum < dealerSum = do return (resultString ++ "\nDealer has the higher hand...\nYou lose...\nDealer wins...")
    | playerSum == dealerSum = do return (resultString ++ "\nYou and the dealer have equal hands.\nIt is a tie.")
    where playerSum = bestValue (getHandSum playerHand)
          dealerSum = bestValue (getHandSum dealerHand)
          resultString = "The sum of your cards is " ++ show playerSum
                          ++ "\nThe sum of dealer's cards is " ++ show dealerSum

{-
getCardValues takes in a card and
returns the value of the card based on its rank.
-}
getCardValue :: PlayingCard -> Int
getCardValue (Two, _) = 2
getCardValue (Three, _) = 3
getCardValue (Four, _) = 4
getCardValue (Five, _) = 5
getCardValue (Six, _) = 6
getCardValue (Seven, _) = 7
getCardValue (Eight, _) = 8
getCardValue (Nine, _) = 9
getCardValue (Ten, _) = 10
getCardValue (Jack, _) = 10
getCardValue (Queen, _) = 10
getCardValue (King, _) = 10
getCardValue (Ace, _) = 11

-- smallAceValue is the value to be used when Ace(s) is(are) counted as 1.
smallAceValue :: Int
smallAceValue = 1

{-
someoneBusts takes a 5-tuple that is returned from userPhase and dealerPhase.
It then checks for the fourth element in the 5-tuple to see if the bust
condition in the game is True. If it is, it returns True, else False.
-}
someoneBusts :: ([PlayingCard], [PlayingCard], [PlayingCard], Bool, Bool) -> Bool
someoneBusts (deck, playerHand, dealerHand, True, hasBlackJack) = True
someoneBusts (deck, playerHand, dealerHand, False, hasBlackJack) = False

{-
-- someoneHasBlackjack takes a 5-tuple that is returned from userPhase and dealerPhase.
-- It then checks for the fifth element in the 5-tuple to see if the Blackjack
-- condition in the game is True. If it is, it returns True, else False.
-}
someoneHasBlackjack :: ([PlayingCard], [PlayingCard], [PlayingCard], Bool, Bool) -> Bool
someoneHasBlackjack (deck, playerHand, dealerHand, hasBusted, True) = True
someoneHasBlackjack (deck, playerHand, dealerHand, hasBusted, False) = False

{-
handToString takes in a list of cards
and returns the string of those cards with ", " to separate them.
This function is for the purpose of displaying player's or dealer's hand.
-}
handToString :: [PlayingCard] -> String
handToString hand = foldl1 (++) ((map (++ ", ") (init list)) ++ [last list])
    where list = map cardToString hand

{-
blackjackHand takes in a list of cards and returns a Boolean value.
If the cards make up a Blackjack hand, it returns True, else False.
-}
blackjackHand :: [PlayingCard] -> Bool
blackjackHand hand
    | length hand /= 2 = False
    | otherwise = ((fst (hand !! 0) == Ace || fst (hand !! 1) == Ace)
            && (getCardValue (hand !! 0) == 10 || getCardValue (hand !! 1) == 10))

{-
Given a hand, bestValue returns the highest possible sum of it
without making the sum larger than 21 (bust) by adjusting the value of the Ace card(s).
bestValue takes in an integer and returns an integer.
-}
bestValue:: Int -> Int
bestValue currentSum
    | currentSum <= 21 = currentSum
    | otherwise = bestValue (currentSum - acesDifference)
        where acesDifference = getCardValue (Ace, Clubs) - smallAceValue

{-
getHandSum takes in a list of cards and returns the sum of their values,
counting Aces as 11.
-}
getHandSum :: [PlayingCard] -> Int
getHandSum hand = sum [getCardValue x | x <- hand]

{-
bust takes in a list of cards and returns a Boolean.
bust returns False when the sum of all the cards in the hand are greater than 21,
with the values of all Aces being 1. It returns True otherwise.
-}
bust :: [PlayingCard] -> Bool
bust hand = (sumOfAces + sumOfNonAces) > 21
            where sumOfAces = smallAceValue * (length aceList)
                  sumOfNonAces = sum (map getCardValue noAceList)
                  aceList = [x | x <- hand, fst x == Ace]
                  noAceList = [x | x <- hand, notElem x aceList]

putGameOverNotice :: IO ()
putGameOverNotice = putStrLn("The game has finished.")
