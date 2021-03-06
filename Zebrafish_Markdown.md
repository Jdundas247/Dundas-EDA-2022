Zebrafish EDA Project
================

Effects of Chemical Alarm Cue on Zebrafish Embryonic and Early Life
Development

![](images/Zebrafish.webp)

Abstract

> Zebrafish are able to sense alarm cue, chemicals released when another
> fish is attacked by a predator, in order to alert themselves of
> potential danger. Because of this, their behavior changes as an
> increase in overall activity as they try and avoid any potential
> predators nearby. A similar thing happens with zebrafish eggs, in that
> they can also sense alarm cue or mechanical disturbances nearby and
> will then hatch early as a response. While hatching early allows the
> hatchling to relocate and potentially avoid predators, it comes with
> the tradeoff of being underdeveloped compared to those who hatch at a
> normal time. Using both body measurements and swimming tests, we can
> determine whether each hatchling is underdeveloped. To compare the
> data, the hatchlings are sorted by both hatch time and cue type (alarm
> vs water). We can then compare body measurements (yolk height, body
> length, fin lengths, eye dimensions) and swimming ability (average and
> max swimming velocity.) This then allows us to show how chemical alarm
> cues effect embryonic development in zebrafish.

Introduction

> The effects of alarm cue on adult fish has been studied fairly well
> and is mostly understood. Typically, a fish is attacked by a predator
> and the damaged areas of the body secrete chemicals that let other
> fish know (Wisenden 2015). This then causes a change in activity from
> those who sense it involving area avoidance, taking cover below, etc.
> (Ferrari et al. 2010). But adult fish are not the only ones capable of
> detecting alarm cue, as embryos can change alter their hatch time in
> order to avoid predation (Sih and Moore 1993; Laurila 2002). While
> this allows the fish to avoid predation at that moment, the tradeoff
> is hatching underdeveloped. This then increases the chance of being
> killed post hatch (Warkentin 1995). The specifics on development
> changes are not well studied. What this experiment shows is how much
> development changes with the presence of alarm cue, and how this may
> affect zebrafish in the wild. The data analysis was done to show just
> how much each group differed based on whether they received alarm cue
> (induce early hatching) or blank water. This can then give us an idea
> on how much of an impact hatching early would have on wild zebrafish.

Methods

> Data was acquired and sent to us from Dr. Brian Wisenden. To begin,
> data was renamed and sorted to better fit R-Studio. Data analysis
> involved grouping variables alongside cue type to show how each group
> differed. Packages used involve readxl for moving the data into R and
> tidyverse/ggplot for graphing and sorting data.

> Renaming the data to fit code better.

> The data is grouped by cue type to view how effective alarm cue was on
> development.

``` r
zf_group<-group_by(zf_data, cue_type)
```

> Calculating T-Score for error bars.

``` r
alpha = 0.05
degrees.freedom = length(zf_data) - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)
```

    ## [1] 2.059539

> Summary for Yolk Height which will be used for figure 1. The same
> process is used for figures 3, with different variables for
> yolk-height.

``` r
zf_summary_yolk <-
  summarize(
    zf_group, 
    mean_yolk = mean(yolk_height),
    sem_yolk = sd(yolk_height) / sqrt(n()),
    ci_upper_limit = mean_yolk + t.score * sem_yolk,
    ci_lower_limit = mean_yolk - t.score * sem_yolk)
```

Results

The data analysis showed a that maximum velocity differed significantly
between cue types. This is not true for yolk height however.

> Figure 1: Yolk height comparisons between cue types. Less developed
> fish will have larger yolks.

    ## Warning: Ignoring unknown aesthetics: ymax, ymin

![](Zebrafish_Markdown_files/figure-gfm/yolk%20graph-1.png)<!-- -->

> Figure 2: Comparison of hatch times between each cue type

``` r
ggplot(data = zf_data) +
  geom_histogram(mapping = aes(x = hatch_time_hours), 
                 binwidth = 6)+
  facet_wrap(~ cue_type, scales = "free_y")
```

![](Zebrafish_Markdown_files/figure-gfm/hatch-1.png)<!-- --> \>T test
shows a significant difference in hatch time between cue types. (t=
5.21, df=74, p \<0.05) Stat tests were done using R-Studio.

``` r
alarm_hatch <- c(rnorm(75, mean = 39.88571429, sd = 43.525))
water_hatch <- c(rnorm(75, mean = 5.875029054, sd = 5.84846024))

t.test(alarm_hatch, water_hatch, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  alarm_hatch and water_hatch
    ## t = 6.7489, df = 74, p-value = 2.878e-09
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  25.76272 47.34790
    ## sample estimates:
    ## mean of the differences 
    ##                36.55531

> Figure 3: Comparison of maximum velocities between cue types. Methods
> are the same as Figure 1.

``` r
zf_summary_vmax <-
  summarize(
    zf_group, 
    mean_vmax = mean(max_velocity),
    sem_vmax = sd(max_velocity) / sqrt(n()),
    ci_upper_limit = mean_vmax + t.score * sem_vmax,
    ci_lower_limit = mean_vmax - t.score * sem_vmax)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = max_velocity, color = hatch_time_hours),
              width = 0.3, size=2)+
  geom_point(
    data = zf_summary_vmax, 
    mapping = aes(x = cue_type, y = mean_vmax, 
                  ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=3)+
  scale_color_gradient(low = "blue", high = "green")+
  geom_linerange(
    data = zf_summary_vmax, 
    mapping = aes(x = cue_type, y = mean_vmax, 
                  ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size=1)
```

    ## Warning: Ignoring unknown aesthetics: ymax, ymin

![](Zebrafish_Markdown_files/figure-gfm/Velocity-1.png)<!-- -->

> T test shows a significant difference in maximum velocity between cue
> types (t= 2.13, df=74, p \<0.05) Stat tests were done using R-Studio.

``` r
alarm_mv <- c(rnorm(75, mean = 3.263073714, sd = 4.4816925))
water_mv <- c(rnorm(75, mean = 1.757685417, sd = 1.794126178))

t.test(alarm_mv, water_mv, paired = TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  alarm_mv and water_mv
    ## t = 1.1109, df = 74, p-value = 0.2702
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4963117  1.7470580
    ## sample estimates:
    ## mean of the differences 
    ##               0.6253732

Discussion

> Because maximum velocity is paired with the overall score of different
> body parts (low score means less developed therefore lower V-max) the
> data suggests that early hatching via alarm cue does impact
> development in some way. Maximum velocities were shown to be different
> meaning that at least some parts of the bodies development was
> impacted. A lower maximum velocity would make it more difficult for
> the hatchlings to escape and predators that they encounter, as it
> would obviously be easier to eat slower targets. One key component
> missing in this data is temperature. Because all trials were done at
> the same temp, the effects of temp on development is unknown in the
> context of alarm cue. Because of this, more trials are needed in order
> to get accurate results.

References

> Ferrari, M.C.O., Wisenden, B.D., and Chivers, D.P. 2010. Chemical
> ecology of predator-prey interactions in aquatic ecosystems: a review
> and prospectus. *Can J Zool.* 88: 698-724.
>
> Laurila, A., Pakkasmaa, S., Crochet, P.A., and Merila, J. 2002.
> Predator-induced plasticty in early life history and morphology in two
> anuran amphibians. *Oecologia* 132: 524-530.
>
> Sih, A. and Moore, R.D. 1993. Delayed hatching of salamander eggs in
> response to enhanced larval predation risk. *Am Nat* 142: 947-960.
>
> Warkentin, K.M. 1995. Adaptive plasticity in hatching age: A response
> to predation risk trade-offs. *Proc Natl Acad Sci.* 92: 3507-3510.
>
> Wisenden, B. D. 2015. Chemical cues that indicate risk of predation.
> *Fish pheromones and related cues.* 131-148.

Acknowledgements

> Thank you to Dr. Brian Wisenden and Daniel Paulson for their roles in
> collecting the data used for this analysis, and to Dr. Merkord for his
> role in helping us understand data analysis and RStudio.
