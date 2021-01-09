The code that runs this application can be found on [My Github](https://github.com/benzphan/Projects)

The algorithm that I made for the simulations:
```
my.bank <- function(my.pictures,
                    my.bet,
                    n.games,
                    bankroll=100,
                    max.profit=200)
{
  # counter keep track of how many rounds that we have played
  counter <- 0
  
  # The amount that you have invested is the money you bet times the number of pictures you've placed it on. For example, placing $2 on 2 pictures means you have invested $4
  amount.invested <- my.bet * length(my.pictures)
  
  # While you still have money and you haven't hit your bankroll goal and you aren't tired
  while (bankroll >= amount.invested & 
         bankroll < max.profit & counter < n.games)
  {
  
    # place bets
    bankroll <- bankroll - amount.invested
    
    # The dice are shaken
    the.dice <- sample(x = 6,
                       size = 3,
                       replace = TRUE)
    
    # This is the money you get back for placing your money on the right picture
    original.amount <- sum(my.pictures %in% the.dice)
    
    # This is the money that the banker pays out to you
    payment <- sum(the.dice %in% my.pictures)
    
    # You receieve both the original amount and the payment
    winnings <- (original.amount + payment) * my.bet
    
    # Add your winnings to your bankroll
    bankroll <- bankroll + winnings
    
    # Onto the next round
    counter <- counter + 1
  }
  
  bankroll
  # This algorithm returns your final bankroll after one night of playing. You stop when
  # (1) you have nothing left, or
  # (2) when you have $200 or more (enough profit for the day...go home), or
  # (3) when you have played 50 times (too tired to play any more).
  
}
```