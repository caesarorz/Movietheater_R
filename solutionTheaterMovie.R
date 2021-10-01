# Assignment
# For today's assignment, you will be simulating the total revenue earned
# each day for a movie theater over the course of a week. You have some
# starter code below and comments on what you are supposed to do. If you finish
# the project early you can try creating the optional functionality detailed in
# the comments at the top of the starter code.

#
# Notes:
# Make sure you don't have more people attending your movie than you have seats
# The amount of adult and children in the theater should be randomized
# Make sure to answer the 3 questions at the bottom of the starter code.
# You can download the start code at the bottom.


# Potential Questions to Answer:
# 1. Create snacks that the customers can buy and randomize who buys which snack
snacks = c('popcorn'=5, 'chicken'=10)
snacks['popcorn']
# 2. Pretend you own multiple theaters and run two simulations to represent each
# theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are
# not allowed to watch

# Cost for adults and children

ticket_cost_adult <- c(2)
ticket_cost_child <- c(1)
movies <- c('Cinderella Man', 'Lord of the Rings', 'Avatar', 'Star War', 'Ridic')  # List 5 of your favorite movies

screens <- rep(0, length(movies)) # How many screens does the theater have? (assume 1 per movie)
seats <- c(40, 30, 20, 20, 30)  # How many seats does each theater hold
named_days_of_week = c("Sunday", "Monday", "Tuesday", "Wedneday", "Thursday", "Friday", "Saturday")
week_days <- rep(0, 7)  # Store totals for each day
revenue_per_movie = c(screens)


# return total revenue
day_total_revenue <- function(visitors_adults, visitors_children) {
  revenue_adult <- ticket_cost_adult * visitors_adults # 2 * 6 = 12
  revenue_children <- ticket_cost_child * visitors_children # 1 * 10 = 10
  return(revenue_adult + revenue_children)
}


# iterate through the week
for (i in 1:length(week_days)) {
  print(sprintf("Week day #  %d",i))
  
  screen_revenue <- rep(0, 7)
  # iterate through the amount of screens on a particular day
  for (j in 1:length(screens)) {
        print(sprintf('Screens %d ', j))
        # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(1:8, 1)
        visitors_children <- sample(1:8, 1)
        total_people <- visitors_adults + visitors_children
        
        
        # check screen capacity was not passed
        if(total_people > seats[j]){
          print("No more seats ***************************")
          screen_revenue[i] <- 0
          next
        }
        
        # Calculate the revenue for adults and children
        # Calculate revenue, and add to running total for the day
        revenue <- day_total_revenue(visitors_adults, visitors_children)
        screen_revenue[i] <- revenue
      
        print(sprintf("Revenue %d ",revenue))
        
        revenue_per_movie[j] <- revenue + revenue_per_movie[j]
        week_days <- screen_revenue + week_days
        
        print(sprintf("Cumulative revenue"))
        print(week_days)
  }
  # Save total to the corresponding day

  print(sprintf("Per week in day %d is ", i))
  print(week_days)
 
}


# Make a barchart showing total revenue per day

barplot(week_days,
        main = "Revenue in a Week",
        xlab = "Day",
        ylab = "Revenue",
        names.arg = named_days_of_week,
        col = "darkred",
        horiz = FALSE)

# Make any other chart

barplot(revenue_per_movie, names.arg = c(movies), col='red')



# Which day had the highest revenue?

highestRevenue <- which(week_days==max(week_days))

sprintf("The day with the highest reveneu is: %s with $%d", 
        named_days_of_week[highestRevenue], 
        max(week_days))



