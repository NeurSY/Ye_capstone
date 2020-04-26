---
title: "538Capstone"
author: "Sherry Ye"
date: "4/26/2020"
output: html_document
---

## Information concerning the experiment

Hyperphosphorylation of tau, a microtubule-associated protein, is a hallmark of many neurodegenerative disorders such as Alzheimer’s disease. However, the mechanisms behind this tau-mediated degeneration remains unknown. Fruit flies are a useful model to study neurodegeneration, due to their naturally short lifespan, genetic tractability, and relatively simple brain. A genetic model of tauopathy has been created by pan-neuronally expressing human tau in fruit flies. Here, I am particularly interested in seeing whether tauopathy in these fruit flies mimics the progressive degeneration pattern seen in humans. 

To do so, I will collect data from three different time points during the flies’ life span (1 week, 3 weeks, and 5 weeks). To further quantify gross degeneration in fly brains, I will employ a histological approach in which I stain the neuronal tissue in the brain, image, and analyze images using ImageJ. Similar to that in humans brains, roughly circular regions in fly brains that resemble holes are accepted as markers of neurodegeneration. Here, we refer to those regions as vacuoles. For each brain sample, the number of vacuoles with areas larger than 9 μm² was recorded using ImageJ. 

Hypothesis: If flies have the same progressive pattern of neurodegeneration as seen in humans, then the number of vacuoles will be highest at 5 weeks, lower at 3 weeks, and lowest at 1 week. 

Note: the dependent variable here will be the number of vacuoles (measured variable). The independent variable will be age (measured variable), with three levels (1 week, 3 weeks, and 5 weeks). 

H0: the mean number of vacuoles is the same for all three age groups. 

H1: the mean number of vacuoles is not the same for all three age groups.

For the statistical analysis, I will use a completely randomized one-way ANOVA test. There are three levels in the independent variable, one level in the dependent variable, and the tau-expressing flies will be selected randomly from our fly colony. Here, I believe each fly is an independent replicate. These flies, though from the same colony, are not genetic replicates of one another. Male and female fly data will be combined for this experiment. For decision making, I will use an alpha level of 0.05 and a power of 80% when conducting the Monte Carlo analysis. If the ANOVA test shows a significant difference between the mean number of vacuoles for the three age groups, I will also perform post hoc tests to see where the variance lies. 


## Graph of simulated results

```{r echo=FALSE, message=FALSE, warning=FALSE}
#Load Packages
library(tidyverse)
library(ez)
```

```{r}
a = 2 #expected number of vacuoles at 1 week
b = 6 #expected number of vacuoles at 3 weeks
c = 11 #expected number of vacuoles at 5 weeks
sd = 3 #expected standard deviation of Outcome variable
n = 4 # number of independent replicates per group
sims = 100 #number of Monte Carlo simulations to run

# Use this script to simulate your custom sample.

dataMaker <- function(n, a, b, c, sd) { 
  
  a1 <- as.integer(abs(rnorm(n, a, sd))) #1wk
  a2 <- as.integer(abs(rnorm(n, b, sd))) #3wk
  a3 <- as.integer(abs(rnorm(n, c, sd))) #5wk
    
    Vacuoles <- c(a1, a2, a3)
    Age <- c(rep(c("1wk", "3wk", "5wk"), each = n))
    ID <- as.factor(c(1:length(Age)))
    df <-data.frame(ID, Vacuoles, Age)
    }

data <- dataMaker(n,a,b,c,sd)

ggplot(data, aes(Age, Vacuoles))+
  geom_jitter(width=0.1,size = 4, alpha=1,color="turquoise")+
  labs(y = "Number of Vacuoles")
```

## Monte Carlo Analysis

```{r message=FALSE}
pval <- replicate(
  sims, {
 
    sample.df <- dataMaker(n, a, b, c, sd)
    
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = ID,
            dv = Vacuoles,
            between = Age,
            type = 2
            )
  
  pval <- sim.ezaov$ANOVA[1,5]
    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power. Change 'n' in your initializer for higher or lower power.")
```

It looks like (with my simulated data) I will only need 4 flies in each age group. 

