ST558 Project 1: Interacting with APIs
================
Alex Prevatte
10/2/2021

-   [Requirements](#requirements)
-   [API Connection Functions](#api-connection-functions)
    -   [`confirmed`](#confirmed)
    -   [`deaths`](#deaths)
    -   [`daily`](#daily)
    -   [`live`](#live)
    -   [`dateFilter`](#datefilter)
    -   [`worldPercent`](#worldpercent)
-   [Exploratory Data Analysis](#exploratory-data-analysis)

This vignette was created for reading and summarizing data from the
[COVID-19
API](https://documenter.getpostman.com/view/10808728/SzS8rjbc#704f6cc8-780a-4819-8c66-5e03750cb406)

# Requirements

To interact with the API and perform exploratory data analysis I used
the following packages:

-   [`knitr`](https://cran.r-project.org/web/packages/knitr/index.html):
    Report generation
-   [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/):
    JSON parser for R
-   [`tidyverse`](https://www.tidyverse.org/): Data cleaning, filtering,
    graphing
-   [`RCurl`](https://cran.r-project.org/web/packages/RCurl/RCurl.pdf):
    Access API

# API Connection Functions

### `confirmed`

The `confirmed` function is used to return total and new confirmed
COVID-19 case counts for a country of interest. To return the total case
count for all countries, “all” can be used. These total counts are
updated daily.

``` r
confirmed <- function(country = "all"){
  ###
  # This function returns total and new COVID case count
  ###
  
  # Obtain data from summary endpoint
  o <- fromJSON(
    "https://api.covid19api.com/summary"
    )
  
  # View countries
  output <- o$Countries
  
  # If country input is not "all", filter rows accordingly
  if (country != "all"){
    
  # Filter rows based on country selected. 
  # Select Country,TotalConfirmed, and NewConfirmed variables
   if (country %in% output$Country){
    sub <- output %>%
       filter(Country == country) %>%
       select(Country,TotalConfirmed,NewConfirmed)
   }
  
  # Filter rows based on selected Country Code. 
  # Select Country,TotalConfirmed, and NewConfirmed variables
  else if (country %in% output$CountryCode){
    sub <- output %>%
      filter(CountryCode == country) %>%
      select(Country,TotalConfirmed,NewConfirmed)
  }
  
  # Filter rows based on selected Slug 
  # Select Country,TotalConfirmed, and NewConfirmed variables
  else if (country %in% output$Slug){
    sub <- output %>%
      filter(Slug == country) %>%
      select(Country,TotalConfirmed,NewConfirmed)
  }
  
  # If Country, Country Code, or Slug is not found, return error message. 
  else {
    message <- paste("ERROR: Country was not found in Country,
                     CountryCode, or Slug columns. Check spelling.")
    stop(message)
  }
}
  # Return Country, TotalConfirmed, and NewConfirmed variables 
  # from all countries
  else { 
    sub <- output %>%
      select(Country,TotalConfirmed,NewConfirmed)
    }
  return(sub)
}
```

### `deaths`

The `deaths` function is used to return total and new COVID-19 deaths
for a country of interest. To return the total case count for all
countries, `"all"` can be used. These total counts are updated daily.

``` r
deaths <- function(country = "all"){
  ###
  # This function returns total and new COVID deaths
  ###
  
  # Obtain data from summary endpoint
  o <- fromJSON(
    "https://api.covid19api.com/summary"
    )
  
  # View countries
  output <- o$Countries
  
  # If country input is not "all", filter rows accordingly
  if (country != "all"){
    
  # Filter rows based on country selected. 
  # Select Country,TotalDeaths, and NewDeaths variables
  if (country %in% output$Country){
    sub <- output %>%
       filter(Country == country) %>%
       select(Country,TotalDeaths,NewDeaths)
   }
  
  # Filter rows based on Country Code selected. 
  # Select Country,TotalDeaths, and NewDeaths variables
  else if (country %in% output$CountryCode){
    sub <- output %>%
      filter(CountryCode == country) %>%
      select(Country,TotalDeaths,NewDeaths)
  }
  
  # Filter rows based on Slug selected. 
  # Select Country,TotalDeaths, and NewDeaths variables
  else if (country %in% output$Slug){
    sub <- output %>%
      filter(Slug == country) %>%
      select(Country,TotalDeaths,NewDeaths)
  }
  
  # If Country, Country Code, or Slug is not found, return error message. 
  else {
    message <- paste("ERROR: Country was not found in Country,
                     CountryCode, or Slug columns. Check spelling.")
    stop(message)
  }
  }
  # Return Country, TotalDeaths, and NewDeaths variables 
  # from all countries
  else { 
       sub <- output %>%
       select(Country,TotalDeaths,NewDeaths)
    }
  return(sub)
}
```

### `daily`

The `daily` function is used to return all cases by case type for a
country from the first recorded case. These total counts are updated
daily.

``` r
daily <- function(country) {
  ###
  # This function returns total cases by case type for a specified country
  ###
  
  # Construct URL for country flexibility
  baseURL <- "https://api.covid19api.com/dayone/country/"
  value <- country
  # Return only blank provinces
  last <- "?province=+"
  URL <- paste0(baseURL, value, last)
  out <- RCurl::getURL(URL)
  o <- fromJSON(URL)
  
  # Subset dataset to return Country, Province, Confirmed, Deaths,
  # Recovered, Active, and Date variables
  sub <- o %>%
       select(Country, Province, Confirmed, 
              Deaths, Recovered, Active, Date)
  
  return(sub)
}
```

### `live`

The `live` function is used to return all live confirmed cases (10
minute increments) for the country of interest. These live counts are
updated every 10 minutes.

``` r
live <- function(country) {
  ###
  # This function returns live confirmed cases by country
  ###
  
  # Construct URL for country flexibility
  baseURL <- "https://api.covid19api.com/live/country/"
  value <- country
  # Return all provinces
  region <- "?province=+"
  last <- "/status/confirmed"
  URL <- paste0(baseURL, value, region, last)
  out <- RCurl::getURL(URL)
  o <- fromJSON(out)
  
  # Subset dataset to return Country, Province, Confirmed, and Date variables
  sub <- o %>%
       select(Country, Province, Confirmed, Date)
  
  return(sub)
}
```

### `dateFilter`

The `dateFilter` function is used to return cumulative daily confirmed
cases for a given range of dates for the country of interest.

``` r
dateFilter <- function(country, covidStart, covidEnd) {
  ###
  # This function returns live confirmed cases by country
  ###
  
  # Construct URL for country flexibility
  baseURL <- "https://api.covid19api.com/country/"
  value <- country
  
  # Add remaining URL pieces with time stamp
  status <- "/status/confirmed?from="
  start <- covidStart
  from <- "T00:00:00Z&to="
  end <- covidEnd
  to <- "T00:00:00Z"
  URL <- paste0(baseURL, value, status, start, from, end, to)
  out <- RCurl::getURL(URL)
  o <- fromJSON(out)
  
  # Subset dataset to return Country, Province, Confirmed, and Date variables
  sub <- o %>%
       select(Country, Province, Cases, Status, Date)
  
  return(sub)
}
```

### `worldPercent`

The `worldPercent` function uses the world and summary endpoints to
return total confirmed cases, total deaths, and percent of confirmed
cases and deaths from global cases. Here we can directly see what
percent of global cases and deaths are attributed to a given country.

``` r
worldPercent <- function(country) {
  ###
  # This function returns country confirmed and death percentages 
  # in relation to global case numbers 
  ###
  
  # Construct URL from world endpoint
  URL <- "https://api.covid19api.com/world/total"
  out <- RCurl::getURL(URL)
  o <- fromJSON(URL)
  
  # Calculate confirmed case and death percentages
  death <- round((deaths(country)[2] / o[["TotalDeaths"]]) * 100, 2)
  confirm <- round((confirmed(country)[2] / o[["TotalConfirmed"]]) * 100,2)
  
  # Construct dataframe of relevant information
  df <- data.frame(country, deaths(country)[2], o[["TotalDeaths"]], 
                   death, confirmed(country)[2],
                   o[["TotalConfirmed"]], confirm)
  colnames(df) <- c("Country", "Death Count", "Global Death Count", 
                    "% of Global Death", "Case count", 
                    "Global Case Count", "% of Global Case Count")
  return(df)
}
```

# Exploratory Data Analysis

After creating functions to interact with the COVID-19 API, we are going
to create contingency tables, quantitative summaries, and a variety of
qualitative and quantitative visualizations.

First, we are going to compare total confirmed cases and deaths from the
three largest countries by confirmed case number (United States of
America, India, Brazil). We are also going to examine new daily
confirmed cases and deaths from these countries.

``` r
# Determine top 3 countries with highest total confirmed case count
totalConfirmed <- confirmed("all")[c(1,2)]
totalConfirmedOrdered <- totalConfirmed %>% arrange(desc(TotalConfirmed))
order <- totalConfirmedOrdered[1:3,]

#total confirmed cases from top 3 countries
usTotalConfirmed <- confirmed("united-states")[c(1,2)]
indiaTotalConfirmed <- confirmed("india")[c(1,2)]
brazilTotalConfirmed <- confirmed("brazil")[c(1,2)]
confirmedDF <- rbind(usTotalConfirmed, indiaTotalConfirmed, brazilTotalConfirmed)

#total deaths from top 3 countries
usTotalDeaths <- deaths("united-states")[2]
indiaTotalDeaths <- deaths("india")[2]
brazilTotalDeaths <- deaths("brazil")[2]
deathDF <- rbind(usTotalDeaths, indiaTotalDeaths, brazilTotalDeaths)

#combine confirmed and death dataframes and create table
combinedDF <- cbind(confirmedDF,deathDF)
colnames(combinedDF) <- c("Country", "Total Confirmed Cases", "Total Deaths")
knitr::kable(combinedDF)
```

| Country                  | Total Confirmed Cases | Total Deaths |
|:-------------------------|----------------------:|-------------:|
| United States of America |              43852255 |       703278 |
| India                    |              33853048 |       449260 |
| Brazil                   |              21478546 |       598152 |

``` r
#new confirmed cases from top 3 countries
usNewConfirmed <- confirmed("united-states")[c(1,3)]
indiaNewConfirmed <- confirmed("india")[c(1,3)]
brazilNewConfirmed <- confirmed("brazil")[c(1,3)]
confirmedNewDF <- rbind(usNewConfirmed, indiaNewConfirmed, brazilNewConfirmed)

#new deaths from top 3 countries
usNewConfirmed <- deaths("united-states")[3]
indiaNewConfirmed <- deaths("india")[3]
brazilNewConfirmed <- deaths("brazil")[3]
deathNewDF <- rbind(usNewConfirmed, indiaNewConfirmed, brazilNewConfirmed)

#combine confirmed and death dataframes and create table
combinedNewDF <- cbind(confirmedNewDF,deathNewDF)
colnames(combinedNewDF) <- c("Country", "New Daily Confirmed Cases", "New Daily Deaths")
knitr::kable(combinedNewDF)
```

| Country                  | New Daily Confirmed Cases | New Daily Deaths |
|:-------------------------|--------------------------:|-----------------:|
| United States of America |                    169207 |             2109 |
| India                    |                     18346 |              263 |
| Brazil                   |                     10425 |              204 |

Based on the contingency tables, total and new confirmed case numbers
from highest to lowest are from the United States, India, and Brazil
respectively. However, Brazil has more total deaths in comparison to
India.

Next, I’d like to look at numerical summaries for daily confirmed cases
and daily deaths from the United States. The COVID-19 API contains
confirmed cases and deaths cumulatively by day, but I’d like to see how
the daily counts and deaths vary by day. To create quantitative
summaries of this data, I decided to create two new variables,
newDailyConfirmed and newDailyDeaths, that display the daily confirmed
case and death count without summing these variables over time. This
way, we can determine summary statistics for daily confirmed and death
values.

``` r
# Extract data from the daily function
usDF <- daily("united-states")
n <- nrow(usDF)
# Create two new variables for the United States
newDailyConfirmed <- vector(n, mode = "numeric")
newDailyDeaths <- vector(n, mode = "numeric")
# Run a loop to fill the new variables with daily information
for (i in 1:n) {
  newDailyConfirmed[i] <- usDF$Confirmed[i+1] - usDF$Confirmed[i]
  newDailyDeaths[i] <- usDF$Deaths[i+1] - usDF$Deaths[i]
}

# Get summary statistics for new daily confirmed cases in the U.S
newDailyConfirmed <- data.frame(newDailyConfirmed)
newDailyConfirmed <- newDailyConfirmed %>%
summarize("Min." = min(newDailyConfirmed, na.rm = TRUE),
          "1st Quartile" = quantile(newDailyConfirmed, 0.25, na.rm = TRUE),
          "Median" = quantile(newDailyConfirmed, 0.5, na.rm = TRUE),
          "Mean" = mean(newDailyConfirmed, na.rm = TRUE),
          "3rd Quartile" = quantile(newDailyConfirmed, 0.75, na.rm = TRUE),
          "Max" = max(newDailyConfirmed, na.rm = TRUE),
          "Std. Dev." = sd(newDailyConfirmed, na.rm = TRUE)
          )
# Assign category
newDailyConfirmed$Category <- "Daily Confirmed Cases"

# Get summary statistics for new daily deaths in the U.S
newDailyDeaths <- data.frame(newDailyDeaths)
newDailyDeaths <- newDailyDeaths %>%
summarize("Min." = min(newDailyDeaths, na.rm = TRUE),
          "1st Quartile" = quantile(newDailyDeaths, 0.25, na.rm = TRUE),
          "Median" = quantile(newDailyDeaths, 0.5, na.rm = TRUE),
          "Mean" = mean(newDailyDeaths, na.rm = TRUE),
          "3rd Quartile" = quantile(newDailyDeaths, 0.75, na.rm = TRUE),
          "Max" = max(newDailyDeaths, na.rm = TRUE),
          "Std. Dev." = sd(newDailyDeaths, na.rm = TRUE)
          )
#assign category
newDailyDeaths$Category <- "Daily Death"

#Table of summary statistics
combinedStats <- rbind(newDailyConfirmed,newDailyDeaths)
combinedStats <- combinedStats %>% select(Category, Min., `1st Quartile`, Median,
                                          Mean, `3rd Quartile`, Max, `Std. Dev.`)
  
knitr::kable(combinedStats, 
             caption="Summary Statistics for Daily Confirmed Cases
             and Deaths in the United States",
             digits=0)
```

| Category              | Min. | 1st Quartile | Median |  Mean | 3rd Quartile |    Max | Std. Dev. |
|:----------------------|-----:|-------------:|-------:|------:|-------------:|-------:|----------:|
| Daily Confirmed Cases |    0 |        24448 |  47630 | 70616 |        85922 | 303008 |     67810 |
| Daily Death           |    0 |          434 |    891 |  1132 |         1570 |   4460 |       970 |

Summary Statistics for Daily Confirmed Cases and Deaths in the United
States

The numeric summaries for daily confirmed cases and daily death show
quite a bit of variability. Some days there were not many cases and
deaths, other days had much higher values. This makes sense, as the U.S
experienced several waves of increased cases and deaths before
vaccination.

Now let’s look at boxplots of the top three countries by case number to
visualize daily confirmed case count.

``` r
# Create daily confirmed variables for United States 
usDFCount <- daily("united-states")
n <- nrow(usDFCount)
usDailyConfirmed <- vector(n, mode = "numeric")
for (i in 1:n) {
  usDailyConfirmed[i] <- usDFCount$Confirmed[i+1] - usDFCount$Confirmed[i]
}

# Assign category, column name, and convert to dataframe
usDailyConfirmed <- data.frame(usDailyConfirmed)
usDailyConfirmed$Country <- "United States"
colnames(usDailyConfirmed) <- c("Count", "Country")


# Create daily confirmed variables for India 
indiaDF <- daily("india")
n <- nrow(indiaDF)
indiaDailyConfirmed <- vector(n, mode = "numeric")
for (i in 1:n) {
  indiaDailyConfirmed[i] <- indiaDF$Confirmed[i+1] - indiaDF$Confirmed[i]
}

# Assign category, column name, and convert to dataframe
indiaDailyConfirmed <- data.frame(indiaDailyConfirmed)
indiaDailyConfirmed$Country <- "India"
colnames(indiaDailyConfirmed) <- c("Count", "Country")

# Create daily confirmed variables for Brazil 
brazilDF <- daily("brazil")
n <- nrow(brazilDF)
brazilDailyConfirmed <- vector(n, mode = "numeric")
for (i in 1:n) {
  brazilDailyConfirmed[i] <- brazilDF$Confirmed[i+1] - brazilDF$Confirmed[i]
}

# Assign category, column name, and convert to dataframe
brazilDailyConfirmed <- data.frame(brazilDailyConfirmed)
brazilDailyConfirmed$Country <- "Brazil"
colnames(brazilDailyConfirmed) <- c("Count", "Country")

# Combine data from the U.S, India, and Brazil
combinedAll <- rbind(usDailyConfirmed,indiaDailyConfirmed,brazilDailyConfirmed)
combinedAll <- combinedAll %>% filter(!is.na(Count))

# Create factor from categorical variable. 
country <- factor(combinedAll$Country, 
                  levels = c("United States", "India", "Brazil"),
                  ordered = TRUE)

# Make a box plot of daily confirmed cases by region.
g <- ggplot(combinedAll, 
               aes(x = country,
                   y = Count,
                   color = Country
        ))
  # Add the box plot layer
  g + geom_boxplot() +
  # Add labels to the axes
  scale_x_discrete("Country") +
  scale_y_continuous("Daily Case Count") + 
  # Add a title
  ggtitle("Daily Case Count for Top 3 Countries") + 
  # Remove the legend 
  theme(legend.position="none")
```

![](README_files/figure-gfm/boxplot-1.png)<!-- -->

Looking at the boxplots, we see that the U.S has the highest daily
average followed by India and Brazil. We can see that India has many
outliers which represents individual days where the confirmed case count
was extremely high. Brazil didn’t have as many outliers in comparison to
the U.S and India.

To visualize deaths from the United States, Brazil, and India, I created
a simple barplot. Here we can see that deaths from Brazil are much
higher than India, though India had a higher total case count.

``` r
# Determine top 3 countries by total deaths
totalDeaths <- deaths("all")[c(1,2)]
totalDeathsOrdered <- totalDeaths %>% arrange(desc(TotalDeaths))
order <- totalDeathsOrdered[1:3,]

# Parse out total deaths from each country
usDeaths <- deaths("united-states")[c(1,2)]
indiaDeaths <- deaths("india")[c(1,2)]
brazilDeaths <- deaths("brazil")[c(1,2)]

# Convert to dataframe and assign categorical variable for each country
usBar <- data.frame(replicate(n = usDeaths[1,2], "United States"))
colnames(usBar) <- "Country"
indiaBar <- data.frame(replicate(n = indiaDeaths[1,2], "India"))
colnames(indiaBar) <- "Country"
brazilBar <- data.frame(replicate(n = brazilDeaths[1,2], "Brazil"))
colnames(brazilBar) <- "Country"

# Combine all data
totalBar <- rbind(usBar,indiaBar,brazilBar)

# Create positions and barplot of total deaths
positions <- c("United States", "Brazil", "India")
g2 <- ggplot(data = totalBar, aes(x = Country)) + scale_x_discrete(limits = positions)
# Add barplot
g2 + geom_bar(fill = c("red", "green", "blue")) +
  # Add labels and title
  labs(y = "Death", title = "Bar Plot of Deaths from Top 3 Countries") 
```

![](README_files/figure-gfm/barplot-1.png)<!-- -->

I then created a histogram of daily confirmed cases with each region
overlayed. From this histogram, we can see that there was a higher
frequency of lower case numbers from Brazil in comparison to the U.S and
India. The U.S and India had more days of higher confirmed cases.

``` r
# Create histogram of daily case count
g3 <- ggplot(combinedAll, aes(x = Count, fill = Country))
g3 + geom_histogram() +
  #Add labels to axes
  scale_x_continuous("Number of Confirmed Cases") + 
  scale_y_continuous("Daily Case Frequency") +
  # Add a title
  ggtitle("Frequency of Daily Confirmed Cases by Region") 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-gfm/histogram-1.png)<!-- -->

Using the daily endpoint, I created a scatterplot of cumulative total
cases over time by region. We see different trends based on region, with
some case loads increasing at faster rates over others.

``` r
# Create United States dataframe of cumulative cases
day <- rep(1:nrow(usDF))
category <- data.frame(replicate(nrow(usDF), "United States"))
us <- cbind(day, Confirmed = usDF$Confirmed, Cat = category)
colnames(us) <- c("Day","Confirmed","Country")

# Create India dataframe of cumulative cases
day <- rep(1:nrow(indiaDF))
category <- data.frame(replicate(nrow(indiaDF), "India"))
india <- cbind(day, Confirmed = indiaDF$Confirmed, Cat = category)
colnames(india) <- c("Day","Confirmed","Country")

# Create Brazil dataframe of cumulative cases
day <- rep(1:nrow(brazilDF))
category <- data.frame(replicate(nrow(brazilDF), "Brazil"))
brazil <- cbind(day, Confirmed = brazilDF$Confirmed, Cat = category)
colnames(brazil) <- c("Day","Confirmed","Country")

# Combine all data 
data <- rbind(us,india,brazil)

# Create scatterplot of cumulative daily confirmed case numbers by region
g4 <- ggplot(data, aes(Day,Confirmed, color = Country))
# Scatterplot layer
g4 + geom_point() +
    # Add trend line
    geom_smooth(col = "Black") +
    # Add labels
    scale_y_continuous("Total Confirmed Case Count") + 
    # Add title
    ggtitle("Scatterplot of Cumulative Confirmed Cases by Day") +
    # Graph by country
    facet_wrap(~Country) +
    # Remove the legend 
    theme(legend.position="none")
```

![](README_files/figure-gfm/scatterplot-1.png)<!-- -->

Using the daily endpoint, I created a scatterplot of total cases by day
for each region. Instead of the viewing the cumulative trend, we are
looking at daily confirmed cases over time. We see different trends
based on region, with one noticeable spike in case load from India and
two large spikes in the U.S.

``` r
# Ending at 1 less than the number of rows to account for the NA removed in the combinedAll dataset. 
usDay <- data.frame(rep(1:(nrow(usDailyConfirmed)-1)))
colnames(usDay) <- "Day"
indiaDay <- data.frame(rep(1:(nrow(indiaDailyConfirmed)-1)))
colnames(indiaDay) <- "Day"
brazilDay <- data.frame(rep(1:(nrow(brazilDailyConfirmed)-1)))
colnames(brazilDay) <- "Day"

#combine all days
Day <- rbind(usDay,indiaDay,brazilDay)
combinedAll <- cbind(combinedAll, Day)

# Create scatterplot for each country of daily cases
g5 <- ggplot(combinedAll, aes(Day, Count, color = Country))
# Add scatterplot layer
g5 + geom_point() +
    # Add trend line
    geom_smooth(col = "Black") +
    # Add axis label
    scale_y_continuous("Daily Confirmed Case Count") + 
    # Add title
    ggtitle("Scatterplot of Daily Confirmed Cases Over Time") +
    # Graph by country
    facet_wrap(~Country) +
    # Remove the legend 
    theme(legend.position="none")
```

![](README_files/figure-gfm/scatterplot2-1.png)<!-- -->
