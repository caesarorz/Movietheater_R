# Assignment
# For today's assignment, you will be simulating the total revenue earned each day for a movie theater over the course of a week. 
# You have some starter code below and comments on what you are supposed to do. If you finish the project early you can try creating 
# the optional functionality detailed in the comments at the top of the starter code.
# 
# Notes:
# Make sure you don't have more people attending your movie than you have seats
# The amount of adult and children in the theater should be randomized
# Make sure to answer the 3 questions at the bottom of the starter code.
# You can download the start code at the bottom.


# Potential Questions to Answer:
# 1. Create snacks that the customers can buy and randomize who buys which snack
# 2. Pretend you own multiple theaters and run two simulations to represent each theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are not allowed to watch

# Cost for adults and children
ticket_cost <- c(4,) 
  ticket_cost_child <- function() #? 
    movies <- c('Matrix', 'Star Wars', 'Lord of The Rings', 'Cinderella Man', '')  # List 5 of your favorite movies
    screens <- 2# How many screens does the theater have? (assume 1 per movie)
      seats <- 10 # How many seats does each theater hold
      week_days <- rep(0, 7)  # Store totals for each day
    
    # iterate through the week
    for () {
      
      # Keep track of total revenue for the day
      
      
      # iterate through the amount of screens on a particular day
      for () {
        
        # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(, 1)
        visitors_children <- sample((),1)
        
        # Calculate the revenue for adults and children
        
        
        # Calculate revenue, and add to running total for the day
        
        
      }
      # Save total to the corresponding day
      
    }
    
    # Make a barchart showing total revenue per day
    
    # Make any other chart
    
    # Which day had the highest revenue? 