workout01-jacqueline-wood
================

Workout 01
----------

The final narrative is included below under the header 5.2) Narrative.

Loading in some necessary data.
===============================

``` r
shots_data <- read.csv("../data/shots-data.csv",header=TRUE,stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", header=TRUE, stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", header=TRUE, stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", header=TRUE, stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", header=TRUE, stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", header=TRUE, stringsAsFactors = FALSE)

library(jpeg)
library(grid)

# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"

# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))
```

5.1) Effective Shooting Percentage
==================================

``` r
head(shots_data)
```

    ##               team_name game_date season period minutes_remaining
    ## 1 Golden State Warriors   3/24/17   2016      3                 2
    ## 2 Golden State Warriors   11/3/16   2016      2                10
    ## 3 Golden State Warriors  10/25/16   2016      2                 0
    ## 4 Golden State Warriors   11/3/16   2016      2                11
    ## 5 Golden State Warriors    1/8/17   2016      4                 0
    ## 6 Golden State Warriors   3/16/17   2016      3                 5
    ##   seconds_remaining shot_made_flag          action_type      shot_type
    ## 1                35        shot_no  Alley Oop Dunk Shot 2PT Field Goal
    ## 2                51        shot_no  Alley Oop Dunk Shot 2PT Field Goal
    ## 3                 6       shot_yes  Alley Oop Dunk Shot 2PT Field Goal
    ## 4                10        shot_no Alley Oop Layup shot 2PT Field Goal
    ## 5                32       shot_yes    Cutting Dunk Shot 2PT Field Goal
    ## 6                30       shot_yes    Cutting Dunk Shot 2PT Field Goal
    ##   shot_distance              opponent   x  y           name minute
    ## 1             0      Sacramento Kings   0  1 Andre Iguodala     34
    ## 2             1 Oklahoma City Thunder -12 13 Andre Iguodala     14
    ## 3             0     San Antonio Spurs   0  1 Andre Iguodala     24
    ## 4             1 Oklahoma City Thunder  -1 11 Andre Iguodala     13
    ## 5             0      Sacramento Kings   0  1 Andre Iguodala     48
    ## 6             0         Orlando Magic   0  1 Andre Iguodala     31

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)

effective_shooting <- function(points) {
  if(points == 2) {
    filtered <- filter(shots_data,shot_type=="2PT Field Goal")
  } else if(points == 3) {
    filtered <- filter(shots_data,shot_type=="3PT Field Goal")
  } else {
    filtered <- shots_data
  }
  
  shooting <- summarise(group_by(filtered,name),total = n())
  
  shooting$made <- summarise(group_by(filter(filtered,shot_made_flag=="shot_yes"),name),made=n())$made
  
  shooting$perc_made <- shooting$made / shooting$total
  
  shooting <- arrange(shooting,desc(perc_made))
  
  return(shooting)
}

# 2PT Effective Shooting % by Player
effective_shooting(2)
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Andre Iguodala   210   134     0.638
    ## 2 Kevin Durant     643   390     0.607
    ## 3 Stephen Curry    563   304     0.540
    ## 4 Klay Thompson    640   329     0.514
    ## 5 Draymond Green   346   171     0.494

``` r
# 3PT Effective Shooting % by Player
effective_shooting(3)
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Klay Thompson    580   246     0.424
    ## 2 Stephen Curry    687   280     0.408
    ## 3 Kevin Durant     272   105     0.386
    ## 4 Andre Iguodala   161    58     0.360
    ## 5 Draymond Green   232    74     0.319

``` r
# Effective Shooting % by Player
effective_shooting("all")
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Kevin Durant     915   495     0.541
    ## 2 Andre Iguodala   371   192     0.518
    ## 3 Klay Thompson   1220   575     0.471
    ## 4 Stephen Curry   1250   584     0.467
    ## 5 Draymond Green   578   245     0.424

5.2) Narrative
==============

Imagine that you are hired by an Oakland-based sports media company to write an article about the shooting statistics of the five GSW players: Iguodala, Green, Durant, Thompson, and Curry.

Golden State Warriors - Shooting their Shot
===========================================

The Warriors are well-known as one of the most dominating forces on the basketball court, especially when it comes to shooting.

Motivation
----------

I wanted to take an in-depth look at the ways in which the Warriors' shooting is so successful and consider how this contributes to their overall excellence. Specifically, I wanted to compare the shooting for their top five players.

Data
----

To begin, shooting data was gathered from the team's top five players: Andre Iguodala, Draymond Green, Kevin Durant, Klay Thompson, and Steph Curry, and combined into one data table. This included data on the shooter's name, the moment these shots were taken (in minutes and seconds remaining in specific period), the type of shot, the number of feet to the basket, the coordinates on the court, and whether the shot was made or not. The first five lines of the data is shown below.

``` r
head(shots_data)
```

    ##               team_name game_date season period minutes_remaining
    ## 1 Golden State Warriors   3/24/17   2016      3                 2
    ## 2 Golden State Warriors   11/3/16   2016      2                10
    ## 3 Golden State Warriors  10/25/16   2016      2                 0
    ## 4 Golden State Warriors   11/3/16   2016      2                11
    ## 5 Golden State Warriors    1/8/17   2016      4                 0
    ## 6 Golden State Warriors   3/16/17   2016      3                 5
    ##   seconds_remaining shot_made_flag          action_type      shot_type
    ## 1                35        shot_no  Alley Oop Dunk Shot 2PT Field Goal
    ## 2                51        shot_no  Alley Oop Dunk Shot 2PT Field Goal
    ## 3                 6       shot_yes  Alley Oop Dunk Shot 2PT Field Goal
    ## 4                10        shot_no Alley Oop Layup shot 2PT Field Goal
    ## 5                32       shot_yes    Cutting Dunk Shot 2PT Field Goal
    ## 6                30       shot_yes    Cutting Dunk Shot 2PT Field Goal
    ##   shot_distance              opponent   x  y           name minute
    ## 1             0      Sacramento Kings   0  1 Andre Iguodala     34
    ## 2             1 Oklahoma City Thunder -12 13 Andre Iguodala     14
    ## 3             0     San Antonio Spurs   0  1 Andre Iguodala     24
    ## 4             1 Oklahoma City Thunder  -1 11 Andre Iguodala     13
    ## 5             0      Sacramento Kings   0  1 Andre Iguodala     48
    ## 6             0         Orlando Magic   0  1 Andre Iguodala     31

Facetted Shot Charts
====================

After conducting some exploratory analysis on the data, charts were made of each of the five player's shots in order to compare their performances. The facetted shot chart is shown below.

``` r
knitr::include_graphics('../images/gsw-shot-chart.png')
```

![](../images/gsw-shot-chart.png)

Effective Shooting % by Player
------------------------------

Originally, I notice that it appears as if Kevin Durant, Klay Thompson, and Stephen Curry are taking overall more shots than Draymond Green and Andre Iguodala, but it is hard to tell which shooters are actually making a greater percentage of their shots from these shots. So I constructed a table (shown below) that includes the effective shooting percentage by player for both 2 and 3 point shots, arranged in descending order by percentage.

``` r
effective_shooting("all")
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Kevin Durant     915   495     0.541
    ## 2 Andre Iguodala   371   192     0.518
    ## 3 Klay Thompson   1220   575     0.471
    ## 4 Stephen Curry   1250   584     0.467
    ## 5 Draymond Green   578   245     0.424

As seen above, my hypothesis that Kevin Durant, Klay Thompson, and Steph Curry were shooting the most is correct. Steph Curry took the most total shots with 1250, then Klay Thompson with 1220, and Kevin Durant with 915. These three players are also the top three highest scorers.

However, that does not necessarily mean these shooters have the highest shooting percentages. Kevin Durant, the number three scorer has the highest percent made with about 54%. Andre Iguodala is next with a percent made of about 54%. Although, he is not taking nearly as many shots, when he does shoot he makes the shot over 50% of the time.

The fact that the top five players on the Warriors all have above a 40% shooting percentage is very impressive, especially considering the large number of shots they are actually taking, which translates to a large number of made shots, and then of course victorious games.

3PT Effective Shooting % by Player
----------------------------------

Now that I've established these players are excellent shooters when considering both 2 and 3 point field goals, I decided to dive a little deeper to focus solely on their 3-pointer performance and to see if the Warriors actually live up to their 3-point hype.

I hypothesized that Steph Curry would have made the most three point shots and have the highest 3 pointer percentage made because I always hear that he is one of the best three point shooters of all time.

I constructed a table, similar to the one above, that includes the effective shooting percentage by player for solely 3 point shots, arranged in descending order by percentage. This table is shown below.

``` r
effective_shooting(3)
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Klay Thompson    580   246     0.424
    ## 2 Stephen Curry    687   280     0.408
    ## 3 Kevin Durant     272   105     0.386
    ## 4 Andre Iguodala   161    58     0.360
    ## 5 Draymond Green   232    74     0.319

Although, my hypothesis that Steph Curry would have made both the most three point shots and have the highest 3 pointer percentage was incorrect, he did score the most three point shots with 280 out of hi 687 shot, giving him a 40% 3 point effective shooting percentage. However, Klay Thompson, who follows closely behind with 246 3 point shots made, has a slightly higher percentage made of about 42%. I can definitely see why these two men are called the "Splash Brothers" in regards to their incredible three point shooting. These two are considered to be the most explosive back-court duo in NBA history and the numbers back it up.

Splash Brothers' 3PT Shot Locations
-----------------------------------

Considering this, I wanted to take a closer look at where exactly they are taking these 3 point shots from. I wanted to see if Klay Thompson and Steph Curry had sweet spots where they took and made most of their three point shots from or if they were dispersed throughout the court. In order to do this, I made shot charts of their positions on the court where they shot their 3 point shots and included whether the shot was made or not. The results are shown below.

``` r
ggplot(data = thompson[which(thompson$shot_type == "3PT Field Goal"),]) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('3 Point Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
```

![](workout01-jacqueline-wood_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggplot(data = curry[which(curry$shot_type == "3PT Field Goal"),]) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('3 Point Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
```

    ## Warning: Removed 17 rows containing missing values (geom_point).

![](workout01-jacqueline-wood_files/figure-markdown_github/unnamed-chunk-8-2.png)

Interestingly enough, it does not appear as if neither of the Splash Brothers have one specific spot on the court they either take or make most of their 3 point shots from. Both of their distribution of three point shots seem pretty spread out along the perimeter of the 3 point line. However, it does appear that Steph Curry takes more 3 point shots from farther away from the basket then Klay Thompson does so I decided to investigate this a little further.

I decided to make side-by-side boxplots of their shot distances for their 3 point shots. The results are shown below.

``` r
boxplot(shot_distance~name,
data=shots_data[(shots_data$shot_type  == "3PT Field Goal") & (shots_data$name %in% c("Klay Thompson","Stephen Curry")), ],
main="Different boxplots for each Splash Bro",
xlab="Name",
ylab="Shot Distance for 3 Point Shots",
col="orange",
border="brown"
)
```

![](workout01-jacqueline-wood_files/figure-markdown_github/unnamed-chunk-9-1.png)

As you can see, the 3 point shots Curry is taking do have a much wider range of distance in feet from the basket than Klay Thompson's do.

However, if you further restrict these taken 3 point shots to the shots each of the Splash Brothers actually made and make side-by-side boxpots of their shot distances (shown below) you can see that the distributions are far more similar.

``` r
boxplot(shot_distance~name,
data=shots_data[(shots_data$shot_type  == "3PT Field Goal") & (shots_data$name %in% c("Klay Thompson","Stephen Curry")) & (shots_data$shot_made_flag  == "shot_yes"), ],
main="Different boxplots for each Splash Bro",
xlab="Name",
ylab="Shot Distance for 3 Point Shots",
col="orange",
border="brown"
)
```

![](workout01-jacqueline-wood_files/figure-markdown_github/unnamed-chunk-10-1.png)

2PT Effective Shooting % by Player
----------------------------------

Lastly, I decided to focus solely on 2-point shots because those make up the majority of the game and are obviously incredibly important. I began with making a similar table like the ones above which contains the effective shooting percentage by player for both only 2 point shots, arranged in descending order by percentage (shown below).

``` r
effective_shooting(2)
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <chr>          <int> <int>     <dbl>
    ## 1 Andre Iguodala   210   134     0.638
    ## 2 Kevin Durant     643   390     0.607
    ## 3 Stephen Curry    563   304     0.540
    ## 4 Klay Thompson    640   329     0.514
    ## 5 Draymond Green   346   171     0.494

I found the results fairly surprising. Although Andre Iguodala is taking and making the least amount of 2 point field goals his percentage made is very high at about 64%. Essentially, it appears that perhaps Andre Iguodala is pickier about the shots he takes.

To examine this, I made a shot chart of his 2 point field goals (below). He appears to be taking many of his shots within the restricted area (the inner arc closest to the basket) and performing well there.

``` r
ggplot(data = iguodala[which(iguodala$shot_type == "2PT Field Goal"),]) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('2 Point Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
```

![](workout01-jacqueline-wood_files/figure-markdown_github/unnamed-chunk-12-1.png)

However, Kevin Durant has the most made 2 point field goals with 390 out of 643. Considering the sheer amount of 2 point field goals he is taking and making his percentage made of about 61% is incredibly impressive.

As shown below, Kevin Durant is taking 2 point shots from all over and is obviously a very dynamic 2 point shooter and an excellent addition to the Warriors lineup.

``` r
ggplot(data = durant[which(durant$shot_type == "2PT Field Goal"),]) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('2 Point Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
```

![](workout01-jacqueline-wood_files/figure-markdown_github/unnamed-chunk-13-1.png)

Conclusion
----------

All in all, I have highlighted many reasons why the Warriors are such a strong shooting team. All of their top five players, Andre Iguodala, Draymond Green, Kevin Durant, Klay Thompson, and Steph Curry, have an overall (both 2 point and 3 point field goals) percentage made of over 40% and each brings something unique to the team. The Splash Brothers, Steph Curry and Klay Thompson, dominate the back-court with record high three point shooting, Andre Iguodala is great inside the restricted area, and Kevin Durant dominates the entire 2 point field goal range. Of course, Draymond Green brings a lot to the team too. Although he is not as dominant of a shooter of some of the other players (which was focused on here) he is probably the strongest defender on the Warriors court and as you know, the best offense is a strong defense.

Overall, the Warriors excellence comes from their diversification of talent, as each player is the cream of the crop in a specific area.

The Warriors should most definitely continue to "shoot their shot".
