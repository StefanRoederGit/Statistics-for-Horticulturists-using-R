LICOR 6400XT - Leaf Gas Exchange
================
Stefan Roeder
7/1/2021

### Background

We will use a fictional dataset.

Research question:<br/> Do scion and rootstock affect leaf gas exchange
in apple?

Explanatory variables:<br/> - Rootstock (2 levels)<br/> - Scion (5
levels)

Response variables: <br/> - Carbon dioxide assimilation rate (Photo)
<br/> - Transpiration rate (Trmmol) <br/> - Conductance (Cond)

Experimental design:<br/> - subsampling (two readings per tree)

Note:<br/> - I will be using the emmeans package. Thus, the reported
means are estimated marginal means.

#### Install and Load Required Packages

``` r
if (!require('nlme')) install.packages('nlme')
library('nlme')

if (!require('emmeans')) install.packages('emmeans')
library('emmeans')

if (!require('multcomp')) install.packages('multcomp')
library('multcomp')

if (!require('multcompView')) install.packages('multcompView')
library('multcompView')

if (!require('ggplot2')) install.packages('ggplot2')
library('ggplot2')

if (!require('ggpubr')) install.packages('ggpubr')
library('ggpubr')
```

#### Import Dataset

``` r
mydata <- read.delim(url("https://raw.githubusercontent.com/StefanRoederGit/Statistics-for-Horticulturists-using-R/main/LICOR6400XT/Dataset/LICOR-6400XT.txt"),stringsAsFactors=TRUE)
```

#### Convert to Factor Variables

``` r
mydata$Rootstock <- as.factor(mydata$Rootstock)
mydata$Scion <- as.factor(mydata$Scion)
mydata$Tree <- as.factor(mydata$Tree)
```

#### Fit Linear Mixed Model, Run ANOVA, and Perform Multiple Comparision Procedure

#### CO2 Assimilation Rate (Photo)

``` r
fit <- lme(Photo ~ Rootstock * Scion, data = mydata, random = ~1|Tree)
anova(fit)
```

    ##                 numDF denDF   F-value p-value
    ## (Intercept)         1    27 1953.4370  <.0001
    ## Rootstock           1    27   14.9000  0.0006
    ## Scion               4    27    5.3701  0.0026
    ## Rootstock:Scion     4    27    1.7046  0.1781

There were significant differences in carbon dioxide assimilation rate
between the two rootstocks (F(1,27) = 14.9, P = 0.0006) and five scion
cultivars (F(1,27) = 5.4, P = 0.0026).

``` r
lm.emmGrid <- emmeans(fit, pairwise ~ Rootstock)
cld(lm.emmGrid[[1]], Letters = letters)
```

    ##  Rootstock emmean    SE df lower.CL upper.CL .group
    ##  G.11        15.3 0.538 23     14.2     16.5  a    
    ##  G.41        18.3 0.538 22     17.2     19.4   b   
    ## 
    ## Results are averaged over the levels of: Scion 
    ## Degrees-of-freedom method: containment 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05

The average assimilation rate of G.11 (mean = 15.3 μmol m−2 s−1) was
significant lower when compared to G.41 (mean = 18.3 μmol m−2 s−1).

``` r
lm.emmGrid <- emmeans(fit, pairwise ~ Scion)
cld(lm.emmGrid[[1]], Letters = letters)
```

    ##  Scion            emmean    SE df lower.CL upper.CL .group
    ##  Red Delicious      14.6 0.851 23     12.8     16.3  a    
    ##  Fuji               15.8 0.851 23     14.1     17.6  a    
    ##  Golden Delicious   16.3 0.851 22     14.5     18.0  ab   
    ##  Gala               17.7 0.851 23     16.0     19.5  ab   
    ##  Granny Smith       19.7 0.851 23     17.9     21.5   b   
    ## 
    ## Results are averaged over the levels of: Rootstock 
    ## Degrees-of-freedom method: containment 
    ## Confidence level used: 0.95 
    ## P value adjustment: tukey method for comparing a family of 5 estimates 
    ## significance level used: alpha = 0.05

Furthermore, ‘Granny Smith’ showed with 19.7 μmol m−2 s−1 significantly
higher assimilation rates than ‘Red Delicious’ (mean = 14.6 μmol m−2
s−1) and ‘Fuji’ (mean = 15.8 μmol m−2 s−1). The assimilation rate of
‘Golden Delicious’ (mean = 16.3 μmol m−2 s−1) and ‘Gala’ (17.7 μmol m−2
s−1) did not differ from ‘Red Delicious’, ‘Fuji’, and ‘Granny Smith’.

#### Plot Data (emmeans +/- 95% confidence intervals)

``` r
lm.emmGrid <- emmeans(fit, pairwise ~ Rootstock)
clddata <- cld(lm.emmGrid[[1]], Letters = letters)

ggplot(clddata, aes(x = Rootstock, y = emmean, fill = Rootstock)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, position=position_dodge(.9)) +
  theme_bw(base_size = 14) +
  ggtitle("Main Effect: Rootstock") +
  xlab("Rootstock") +
  ylab(~paste("Assimilation rate (", mu, "mol CO"[2], " m"^-2,"s"^-1,")"))+
  ylim(0,25) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  geom_text(data = clddata, aes(y= upper.CL + 1, label = .group),  size = 6) +
  theme(legend.position = "none")  
```

![](https://github.com/StefanRoederGit/Statistics-for-Horticulturists-using-R/blob/108b303923725b18cbfb901e84d8c708c9657791/LICOR6400XT/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
lm.emmGrid <- emmeans(fit, pairwise ~ Scion)
clddata <- cld(lm.emmGrid[[1]], Letters = letters)

ggplot(clddata, aes(x = Scion, y = emmean, fill = Scion)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1, position=position_dodge(.9)) +
  theme_bw(base_size = 14) +
  ggtitle("Main Effect: Scion") +
  xlab("Cultivar") +
  ylab(~paste("Assimilation rate (", mu, "mol CO"[2], " m"^-2,"s"^-1,")")) +
  ylim(0,25) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  geom_text(data = clddata, aes(y= upper.CL + 1, label = .group),  size = 6) +
  theme(legend.position = "none")  
```

![](LICOR-6400XT_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
