#Collaborators:

# - Megan Madrigal Amador. 
# - César Orozco Zamora. 
# - Johan Bastos Zamora. 



# The following code simulates a movie theater with 5 screens.
# We have an asistance per day per movie
# with the cost of the ticket
# also with the possibility to buy snacks

# We add the cost of the staff: c("cleaning"=100, "snack seller"=100, "tickets seller"=100, "movie staff"=250, "manager"=350)
# also fixed cost like rent and insurance

# We proyect everything for 48 weeks (whole year 28 days per month)


snacks_with_prices = c('popcorn'=5, 'chickenwings'=10, 'soda'=5, 'candy '=10)
ticket_cost_adult <- c(15)
ticket_cost_child <- c(10)
movies <- c('Cinderella Man', 'Lord of the Rings', 'Avatar', 'Star War', 'Ridic')  # List 5 of your favorite movies
screens <- rep(0, length(movies)) # How many screens does the theater have? (assume 1 per movie)
seats <- c(40, 30, 20, 20, 30)  # How many seats does each theater hold

week_days <- rep(0, 7)  # Store totals for each day
named_days_of_week = c("Monday", "Tuesday", "Wedneday", "Thursday", "Friday", "Saturday", "Sunday")

revenue_per_movie = c(screens)


# Costs
rent <- 1000 # rent per month
insurance <- 100 # per month
# staff salary and manager 
staff_salary <- c("cleaning"=100, "snack seller"=100, "tickets seller"=100, "movie staff"=250, "manager"=350)


# dataframes containers for weekMovie function calculator
df_snacks_revenue_week <- createDataFrame(7, length(movies), named_days_of_week)
df_movie_revenue_week <- createDataFrame(7, length(movies), named_days_of_week)
df_amount_people_week <- createDataFrame(7, length(movies), named_days_of_week)

weekMovie <- function(weekNum){
  print(weekNum)
  print(randomNumber(weekNum))
  # iterate through the week
  for (i in 1:length(week_days)) {
    
    screen_revenue <- rep(0, 7)
    # iterate through the amount of screens on a particular day
    for (j in 1:length(screens)) {
      
      # Calculate  how many adults and children are watching the movie
      visitors_adults <- sample(randomNumber(weekNum), 1)
      visitors_children <- sample(randomNumber(weekNum), 1)

      total_people <- visitors_adults + visitors_children
      
      # add total people per day/week
      df_amount_people_week[j,i] <<- total_people
      
      # check screen capacity was not passed
      if(total_people > seats[j]){
        screen_revenue[i] <- seats[j] * ticket_cost_adult
        next
      }
      
      # Calculate the revenue for adults and children for movie
      # Calculate revenue, and add to running total for the day
      revenueMovies <- day_total_revenue(visitors_adults, visitors_children)
      screen_revenue[i] <- revenueMovies
      
      revenue_per_movie[j] <- revenueMovies + revenue_per_movie[j]
      week_days <- screen_revenue + week_days
      df_movie_revenue_week[j,i] <<- screen_revenue[i]
      
      # Calculate the revenue for snacks based on
      # pick a random snack and random number for sales for that snack
      
      revenue_snacks <- snacks_total_revenue(total_people)
      # add it. Register the snack sale counting it   <- new feature
      df_snacks_revenue_week[j,i] <<- revenue_snacks 
    }
  }

}

dfMonthMovieSales <- list()
dfMonthSnackSales <- list()
dfMonthAmountPeople <- list()

# project as many weeks as we want as numWeeks
monthMovie <- function(numWeeks){
  for (i in 1:numWeeks){
    weekMovie(i)
    dfMonthMovieSales[[i]] <<- df_movie_revenue_week
    dfMonthSnackSales[[i]] <<- df_snacks_revenue_week
    dfMonthAmountPeople[[i]] <<- df_amount_people_week
  }
}


### generate scenario for weeks
monthMovie(48) # add number of weeks
print(getTotalSales())  # 
print(getTotalSalesSnacks())
print(getTotalSalesMovies())

#################### Questions to answer
################

#**************** 1 month with more sales and month with less sales?
#*

# 
totalSalesEachMonth <- function(){
  months <- rep(0, length(dfMonthMovieSales)/4) # set
  months
  control <- 0
  for(i in 1:length(months)){
    for(week in control+1:length(dfMonthMovieSales)){
      sum <- calculateTotalPerDF(dfMonthMovieSales[[week]]) + calculateTotalPerDF(dfMonthSnackSales[[week]])
      months[i] <- sum + months[i]
      control <- i * 4
      if(week == control){
        break
      }
    }
    sum <- 0
  }
  return(months)
}


## see all months sales (1,2,3,4,5,6,7,8,9,10,11,12)

totalSalesEachMonth()


## Max sales month and min





## bar char for months

counts <- table(mtcars$vs, mtcars$gear)
barplot(totalSalesEachMonth(), main="",
        xlab="Number of Gears", #col=c("darkblue","red"),
        names.arg = c("Jan", "Feb", "Mar", "Apr", "March", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", "Dec"), beside=TRUE)


#******************** 2 movie with more sales?

# For a week

# for month

# Overall


#******************** 3 Which snack has more sales

#******************** 4 create chart for total sales per week

#******************** 5 bar char comparing total sales (snacks and movies) vs snack sales and vs movie sales


totalSalesSnacks <- calculateTotalPerDF(getTotalSalesSnacks())
totalSalesMovies <- calculateTotalPerDF(getTotalSalesMovies())
totalSales <- totalSalesSnacks + totalSalesMovies

sales <- c(totalSales, totalSalesMovies, totalSalesSnacks)

barplot(sales,
        main = "Comparison Total Sales vs Snack and Movie Sales",
        xlab = "Sales in $",
        ylab = "Day",
        names.arg = c("Total", "Snacks", "Movies"),
        col = "darkred",
        horiz = TRUE)

#******************** 6 pie chart showing which makes more sales, snacks or movies?

# metadata to use:
# print(getTotalSales())
# print(getTotalSalesSnacks())
# print(getTotalSalesMovies())


totalSalesSnacks <- calculateTotalPerDF(getTotalSalesSnacks())
totalSalesMovies <- calculateTotalPerDF(getTotalSalesMovies())

salesPerItem <- c(totalSalesSnacks, totalSalesMovies)
labelSales <- c("Snacks", "Movies")

pie(salesPerItem, 
    labels = round(100*salesPerItem/sum(salesPerItem), 1), 
    main = "Sales per Movies and Snacks",
    col = rainbow(length(salesPerItem))
)
legend("topright", labelSales, cex = 0.9,
       fill = rainbow(length(salesPerItem)))

getTotalSalesMovies()



#******************** 7 given total people per movie/day, how many resources should we allocate 
#* (staff) in order to bring the best-profitable service
#*
#* Given the staff salaries, the amount of people and sales, calculate if it is profible 
#* so we can get how much personal we can allocate 
#*

# calculate total people for every month

totalPeopleAllMonth <- function(){
  months <- rep(0, 12) # set
  control <- 1
  for(i in 1:length(months)){
    print(i)
    for(week in 1:control+3){
      sum <- calculateTotalPerDF(dfMonthAmountPeople[[week]])
      
      print("control")
      print(control)
      #months[i] <- sum + months[i]
      #
      if(week == control){
        break
      }
    }
    sum <- 0
    control <- i * 4 + 1
  }
  return(months)
}
print(totalPeopleAllMonth())

# get total people for specific month   ************ not ready
totalPeopleOneMonth <- function(numMonth){
  months <- rep(0, length(dfMonthAmountPeople)/4.33) # set
  control <- 0
  for(i in 1:length(months)){
    for(week in 1:length(dfMonthAmountPeople)){
      
      #sum <- calculateTotalPerDF(dfMonthAmountPeople[[week]])
      #months[i] <- sum + months[i]
      #control <- numMonth * 4
      
      #if(week == control){
      #  break
      #}
    }
    sum <- 0
  }
  return(months)
}
totalPeopleOneMonth(1)



#******************** 9 Costs vs sales: it this profitable?

#* Create costs char
#*

# fixed costs per year = (rent+insurance)*12
fixedCostPerMonth <- function () {
  return(rent + insurance)
}
fixedCostsPerMonth <- fixedCostPerMonth()



##### variable cost
# staff salary and manager 
stt <-  sum(staff_salary)
stt



staffCostPerMonth <- function (monthNumber) {
  staff_cost_month <- 0
  monthAsistance <-  totalPeopleAllMonth()[monthNumber]
  maxAsistance <- sum(seats)*4*7 # calculating max capacity
  
  for(i in staff_cost_month){
    if(monthAsistance >= maxAsistance*.9) { ## 100-90%
      staff_cost_month <- staff_salary[1] * 5 + staff_salary[2]*5 + staff_salary[3]*3 + staff_salary[4] + staff_salary[5]
      print("90")
    } else if (monthAsistance >= maxAsistance*.8) { ## 80%
      staff_cost_month <- staff_salary[1] * 4 + staff_salary[2]*4 + staff_salary[3]*2 + staff_salary[4] + staff_salary[5]
      print("80")
    } else if (monthAsistance >= maxAsistance*.6) { ## 60%
      staff_cost_month <- staff_salary[1] * 3 + staff_salary[2]*3 + staff_salary[3]*2 + staff_salary[4] + staff_salary[5]
      print("60")
    } else if (monthAsistance >= maxAsistance*.4) { ## 40%
      staff_cost_month <- staff_salary[1] * 2 + staff_salary[2]*2 + staff_salary[3]*1 + staff_salary[4] + staff_salary[5]
      print("40")
    } else if (monthAsistance >= maxAsistance*.2) { ## 20%
      staff_cost_month <- staff_salary[1] * 1 + staff_salary[2]*1 + staff_salary[3]*1 + staff_salary[4] + staff_salary[5]
      print("20")
    } else {
      staff_cost_month <- staff_salary[1] * 1 + staff_salary[2]*1 + staff_salary[3]*1 + staff_salary[4] + staff_salary[5]
      print("less 20")
    }   
  }
  return(staff_cost_month)
}


print(staffCostPerMonth(3))

## staff cost per year (12 months)

staffCostPerYear <- function() {
  cost_year <- rep(0, 12)
  for (i in 1:length(cost_year)) {
    cost_year[i] <- staffCostPerMonth(i)
  }
  return(cost_year)
}

staffCostPerYear()
## fixes + variable cost (12 months)

## comparison costs (expenses) vs sales => get revenue
totalSalesEachMonth()


# from amount of people 


#* Create sales char (line)
#* 

## cross both cost and sales to check



#********************  pie chart showing movie with more sales -> optional: 



### optional



# another ideas 


# Total sales per day

# Movie with greatest revenue
# Day of the week with greatest revenue
# Favorite snack

# Analize a specific month -> 
dfMonthMovieSales[[1]][1,]
for(i in dfMonthMovieSales){
  for(j in 1:5){
    print(i[j,]) 
  }
}



########## Get total sales per week (which day sales more overall)

## Get total sales per day ()
# with snacks and movies
getTotalSalesPerDay <- function(){
  
}

# Get total sales for specific day
# with snacks and movies









##########################################
##########  Helper functions

# Total sales movies + snacks for all weeks simulated
getTotalSales <- function(){
  df <- createDataFrame(7, length(movies), named_days_of_week)
  for(i in 1:length(dfMonthMovieSales)){
    df <- dfMonthMovieSales[[i]] + dfMonthSnackSales[[i]] + df
  }
  return(df)
}

# Total sales for snacks for all weeks simulated
getTotalSalesSnacks <- function(){
  df <- createDataFrame(7, length(movies), named_days_of_week)
  for(i in 1:length(dfMonthMovieSales)){
    df <- dfMonthSnackSales[[i]] + df
  }
  return(df)
}

# Total sales for movies for all weeks simulated
getTotalSalesMovies <- function(){
  df <- createDataFrame(7, length(movies), named_days_of_week)
  for(i in 1:length(dfMonthMovieSales)){
    df <- dfMonthMovieSales[[i]] + df
  }
  return(df)
}

# generate total revenue for snacks (inside movieWeek function)
snacks_total_revenue <- function(totalpeople){
  revenue_snack <- 0
  for(snack in 1:length(snacks_with_prices)){
    random_people <- sample(0:totalpeople, 1)
    revenue_snack <- random_people * snacks_with_prices[snack][[1]]
  }
  return(revenue_snack)
}

# return total revenue
day_total_revenue <- function(visitors_adults, visitors_children) {
  revenue_adult <- ticket_cost_adult * visitors_adults # 2 * 6 = 12
  revenue_children <- ticket_cost_child * visitors_children # 1 * 10 = 10
  return(revenue_adult + revenue_children)
}

########### Utility functions

# calculate sum of a dataframe-week (sales or costs or )
calculateTotalPerDF <- function(df){
  total <- 0
  for(i in df){
    total <- sum(i) + total
    #print(sum(i)) 
  }
  return(total)
}

# create empty dataframe
createDataFrame <- function(colN, rowN, colNames=0){
  df_revenue_week <- data.frame(matrix(0, ncol = colN, nrow = rowN))
  colnames(df_revenue_week) <- colNames
  return(df_revenue_week)
}

# given a week number, calculate in which month it is

getMonthByWeek <- function(week) {
  return(ceiling(week / 4))
}


getWeeksMonth <- function(month) {
  
}


# sample function 
randomNumber <- function(weekNum){
  month <- getMonthByWeek(weekNum)
  if(month == 1 | month == 12 | month == 7) {
    return(sample(35:50, 1))
  } else if(month == 8 | month == 9 | month == 10) {
    return(sample(0:35, 1))
  } else {
    return(sample(20:40, 1))
  }
}















