Setup
-----

### Load packages

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.1

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

### Load data

Make sure your data and R Markdown files are in the same directory. When loaded your data file will be called `brfss2013`. Delete this note when before you submit your work.

``` r
load("brfss2013.RData")
```

------------------------------------------------------------------------

Part 1: Data
------------

The observations in the sample are collected using random digit dialing among both landline and cellular phone numbers among non-institutionalized adults 18 years of age or older. Using random digit dialing will allow for generalizations to the population of non-institutionalized adults 18 years of age or older to be made. However causality will not be determined, as these are observational data and not a true experiment.

------------------------------------------------------------------------

Part 2: Research questions
--------------------------

**Research question 1:**

Is the incidence of tobacco use (both smoking and smokeless products) higher among veterans than non veterans?

from the CDC page

<https://www.cdc.gov/tobacco/data_statistics/fact_sheets/adult_data/cig_smoking/index.htm>

in 2016, 15.5% percent of adults aged 18 or older smoked cigarretes

I want to see if the people who are veterans are more prone to smoke cigarretes

**Research question 2:**

Is there a correlation between the numbers of minutes/hours of walking, running, jogging, or swimming and the time a subject sleep?

Could it be that people that are more active sleep longer to recover? or could it be that people that are not that active need less sleep?

**Research question 3:**

Do people without health insurance tend to smoke more than people with it?

I would assume that people that have health insurance would tend to smoke less than people without is since doctors would recommned it.

------------------------------------------------------------------------

Part 3: Exploratory data analysis
---------------------------------

**Research quesion 1:**

first let us filter our data set so we have adults older than 18 years old and have responded smoking at least 100 cigarettes during their lifetime and who, at the time they participated in a survey about this topic, reported smoking every day or some days

Also lets look at the the kind of data there is in the column "smoke100"

``` r
summary(brfss2013$smoke100)
```

    ##    Yes     No   NA's 
    ## 215201 261654  14920

from here we can see the type of data is a string "Yes", "No" and "N/A"

Next step we will subset the Data Frame to select only the smokers population (veterans and non veterans)

``` r
smokers_population <- brfss2013 %>%
  filter(smoke100 == "Yes" & (smokday2 == "Every day" |smokday2 == "Some days"))
```

``` r
smokers_percentage = nrow(smokers_population)/nrow(brfss2013)
smokers_percentage
```

    ## [1] 0.1558721

from here we can confirm that the smokers porcentage for the population is 15.5%

Now we extract the veteran total population

``` r
veteran_population <- brfss2013 %>%
  filter(veteran3 =="Yes")
```

and lets find the smokers from the veteran population

``` r
veteran_smokers_population <- veteran_population %>%
  filter(smoke100 == "Yes" & (smokday2 == "Every day" |smokday2 == "Some days"))
```

now lets see the porcentage of veterans smokers from the population

``` r
veterans_smokers_percentage = nrow(veteran_smokers_population)/nrow(veteran_population)
veterans_smokers_percentage
```

    ## [1] 0.1576018

now we do the same for the non-veteran poulation

``` r
non_veteran_population <- brfss2013 %>%
  filter(veteran3 =="No")
non_veteran_smokers_population <- non_veteran_population %>%
  filter(smoke100 == "Yes" & (smokday2 == "Every day" |smokday2 == "Some days"))
non_veterans_smokers_percentage = nrow(non_veteran_smokers_population)/nrow(non_veteran_population)
non_veterans_smokers_percentage
```

    ## [1] 0.1557208

From here we can see that there is no mayor difference (15.7 vs 15.5) in the smoking porcentage beetween veterans and non veterans.

We can graph this using stack columns

``` r
tidydata_veteran <- brfss2013 %>% select(veteran3,smoke100,smokday2)
tidydata_veteran <-filter(tidydata_veteran, veteran3 =="Yes" )
tidydata_veteran <- tidydata_veteran %>%mutate(smokes = case_when(smoke100 == "Yes" & (smokday2 == "Every day" |smokday2 == "Some days") ~"Smokes", TRUE ~"Dont Smoke"))
tidydata_veteran <- tidydata_veteran %>%mutate(veteran_status = case_when(veteran3 == "Yes" ~"Veteran", TRUE ~"Non Veteran"))
tidydata_veteran <- tidydata_veteran %>% group_by(smokes,veteran_status) %>% summarise(count=n()) %>% mutate(perc=count/nrow(tidydata_veteran))

tidydata_non_veteran <- brfss2013 %>% select(veteran3,smoke100,smokday2)
tidydata_non_veteran <-filter(tidydata_non_veteran, veteran3 =="No" )
tidydata_non_veteran <- tidydata_non_veteran %>%mutate(smokes = case_when(smoke100 == "Yes" & (smokday2 == "Every day" |smokday2 == "Some days") ~"Smokes", TRUE ~"Dont Smoke"))
tidydata_non_veteran <- tidydata_non_veteran %>%mutate(veteran_status = case_when(veteran3 == "Yes" ~"Veteran", TRUE ~"Non Veteran"))
tidydata_non_veteran <- tidydata_non_veteran %>% group_by(smokes,veteran_status) %>% summarise(count=n()) %>% mutate(perc=count/nrow(tidydata_non_veteran))
tidydata_all_veteran <- rbind(tidydata_non_veteran, tidydata_veteran)

ggplot(tidydata_all_veteran, aes(x = factor(veteran_status), y = perc*100, fill = factor(smokes))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Veteran Status", y = "percent", fill = "Smokes") +
  theme_minimal(base_size = 14) + geom_text(aes(label=floor(perc*100)),position="stack",vjust=1)
```

![](Final_Project_Coursera_probability_files/figure-markdown_github/unnamed-chunk-8-1.png)

**Research question 2:**

First lets create a tidy data set where the sleep variable and the exercise are not "NA"

``` r
tidydata_sleepvsexer <- brfss2013 %>% select(exerhmm2,sleptim1)
tidydata_sleepvsexer <-filter(tidydata_sleepvsexer, !is.na(exerhmm2)& !is.na(sleptim1) )
```

now lets plot the graph between sleep and exercise

``` r
plot(tidydata_sleepvsexer$sleptim1, tidydata_sleepvsexer$exerhmm2, main="Hours of sleep vs. minutes/hours of exercise",
   xlab="Sleep Time", ylab="minutes/hours of exercise", pch=19)
```

![](Final_Project_Coursera_probability_files/figure-markdown_github/unnamed-chunk-10-1.png) there is not a strong linear relationship between the numbers of days asleep and the amount of exercise a person reports per the scater plot graph.

**Research question 3:**

Using the same approach in the research question 1 we can see that:

``` r
tidydata_insured <- brfss2013 %>% select(hlthpln1,smoke100,smokday2)
tidydata_insured <-filter(tidydata_insured, hlthpln1 =="Yes")
tidydata_insured <- tidydata_insured %>%mutate(smokes = case_when(smoke100 == "Yes" & (smokday2 == "Every day" |smokday2 == "Some days") ~"Smokes", TRUE ~"Dont Smoke"))
tidydata_insured <- tidydata_insured %>%mutate(insured_status = case_when(hlthpln1 == "Yes" ~"Insured", TRUE ~"Non Insured"))
tidydata_insured <- tidydata_insured %>% group_by(smokes,insured_status) %>% summarise(count=n()) %>% mutate(perc=count/nrow(tidydata_insured))

tidydata_non_insured <- brfss2013 %>% select(hlthpln1,smoke100,smokday2)
tidydata_non_insured <-filter(tidydata_non_insured, hlthpln1 =="No" )
tidydata_non_insured <- tidydata_non_insured %>%mutate(smokes = case_when(smoke100 == "Yes" & (smokday2 == "Every day" |smokday2 == "Some days") ~"Smokes", TRUE ~"Dont Smoke"))
tidydata_non_insured <- tidydata_non_insured %>%mutate(insured_status = case_when(hlthpln1 == "Yes" ~"Insured", TRUE ~"Non Insured"))
tidydata_non_insured <- tidydata_non_insured %>% group_by(smokes,insured_status) %>% summarise(count=n()) %>% mutate(perc=count/nrow(tidydata_non_insured))
tidydata_all_insured <- rbind(tidydata_insured, tidydata_non_insured)

ggplot(tidydata_all_insured, aes(x = factor(insured_status), y = perc*100, fill = factor(smokes))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Veteran Status", y = "percent", fill = "Smokes") +
  theme_minimal(base_size = 14) + geom_text(aes(label=floor(perc*100)),position="stack",vjust=1)
```

![](Final_Project_Coursera_probability_files/figure-markdown_github/unnamed-chunk-11-1.png)

From the chart we can see there is difference between non insured smokers vs insured smokers (13% vs 30%). There seems to be a correlation but that does not mean causation.
