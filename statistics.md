Nutrient Requirements of Conventionally and Organically Grown Cranberry
(*Vaccinium macrocarpon* Ait.)
================

**Computations by Serge-Étienne Parent and S.M.Reza Jamaly**

When managing cranberry nutrition, growers are most interested in
determining the factor(s) that limit growth and yield. In this notebook,
we correlate the performance of cranberry production with fertilizers.

The performance of cranberry production can be assessed by the following
metrics.

  - number of flowers per reporductive uprights (count)
  - fruit set (ratio of berry counts to flower counts)
  - berry counts per fruiting upright (count)
  - average berry weight (\(g \cdot berry^{-1}\))
  - marketable yield (\(Mg \cdot ha^{-1}\))
  - berry quality
  - total anthocyanins (TAcy)
  - total soluble solids (Brix)
  - firmness

We tested fertilizers provides the following nutrients: N, P, K, Mg, Cu,
B, and S.

## Initiate session

To initiate the session, we load the following libraries.

``` r
library("tidyverse") # generic data handling and plotting
library("nlme") # mixed models
library("grid")
library("gridExtra")
library("ggthemr") # prettify ggplot
ggthemr("greyscale")
```

All data are placed in a single csv file.

``` r
pr <- read_csv(file="data/performance-fertilisation.csv")
```

### Arrange data

A couple of data handling is needed for the analysis. First, some
variable types must be changed.

1.  the Bloc column refers to an experimental unit, not a continuous
    variable.
2.  the Fertilizer column is a fertilizer type, i.e. a factor

Then, yield was markedly reduced by 75% in 2017 due to adverse
meteorological conditions in winter and spring. Year 2017 was thus
discarded, I’m considering only data before 2017.

``` r
pr <- pr %>%
  mutate(Bloc = factor(Bloc),
         Fertilizer = factor(Fertilizer)) %>%
  filter(Year < 2017) %>% 
  filter(Fertilizer != "S") %>% 
  droplevels()
```

We create a vector containing the column names of performance indicator.

``` r
performance_index <- colnames(pr)[10:21]
```

Another object contains the names of the fertilizer treatment.

``` r
treatments <- unique(pr$Fertilizer)
```

## Mixed modeling

Combination of performance-treatment will be subjected to a mixed model,
but some with a linear model, and other with a quadratic model.
Moreover, some combinaisons of performance-treatment were not tested. We
aim to loop each possible combinaison to fit a linear model, a quadratic
model or no model at all. The first step of this process is to create a
grid of these combinaisons.

``` r
model_conditions <- expand.grid(performance_index = performance_index, 
                                Fertilizer = treatments)
model_conditions$index <- 1:nrow(model_conditions)
```

We are adding a column to the grid containing the information on which
option to choose. By default, the option is linear.

``` r
model_conditions$model_type = 'linear'
```

Where the modeling threw erors due to lack of data, I imposed `'none'`
as
`model_type`.

``` r
model_conditions$model_type[model_conditions$Fertilizer == 'B' & model_conditions$performance_index == 'Brix'] <- 'none'
```

The following instructions impose a quadratic model to some
combinaisons. We selected quadratic where it visually seemed
appropriate.

``` r
model_conditions$model_type[model_conditions$Fertilizer == 'N' & model_conditions$performance_index == "Yield"] <- 'quadratic'
model_conditions$model_type[model_conditions$Fertilizer == 'N' & model_conditions$performance_index == "Berry_weight"] <- 'quadratic'
model_conditions$model_type[model_conditions$Fertilizer == 'K' & model_conditions$performance_index == "Yield"] <- 'quadratic'
model_conditions$model_type[model_conditions$Fertilizer == 'K' & model_conditions$performance_index == "Berry_weight"] <- 'quadratic'
```

### Effect of nitrogen source

We hypothesized (\(H0\)) that cranberry quality, berry yield and yield
components don’t respond to N and K fertilization and N sources. We test
the linear and quadratic effects of Rate and the linear effect of
fertilizer type. We extract the effect of the fertilizer type from the
model.

The following cell runs mixed models on nitrogen trials and save them in
a list.

``` r
alpha <- 0.05
lmm_N <- list()
interval_lmmN <- list()

mc_N <- model_conditions %>% 
  filter(Fertilizer == "N")

for (i in 1:nrow(mc_N)) {
  performance_i <- as.character(mc_N[i, 1])
  model_type_i <- mc_N[i, 4]
  
  table_mm_i <- pr %>%
    filter(Fertilizer == "N") %>%
    select(one_of(c('Year','Site','Bloc','Rate','Cropping_system', 'Fertilizer_type', performance_i))) %>%
    drop_na()
  
  if (model_type_i == 'linear') {
    lmm_N[[i]] <- lme(as.formula(paste0(performance_i, " ~ Rate + Fertilizer_type ")),
                      random = ~ 1 + 1|Year/Site/Bloc,
                      data = table_mm_i)      
  } else if (model_type_i == 'quadratic') {
    lmm_N[[i]] <- lme(as.formula(paste0(performance_i, " ~ Rate + I(Rate^2) + Fertilizer_type")),
                      random = ~ 1 + 1|Year/Site/Bloc,
                      data = table_mm_i)
  } else if (model_type_i == 'none') {
    lmm_N[[i]] <- NA
  } else {
    print("Model type not recognized. Choose linear or quadratic.")
  }
  
  tTable <- summary(lmm_N[[i]])$tTable%>%
    data.frame() %>%
    rownames_to_column() %>%
    filter(str_detect(rowname, 'Fert'))
  
  intervals <- intervals(lmm_N[[i]], which = "fixed")[[1]] %>%
    data.frame() %>%
    rownames_to_column() %>%
    filter(str_detect(rowname, 'Fert')) %>%
    bind_cols(tTable["p.value"])
  
  intervals$performance <- mc_N$performance_index[i]
  
  interval_lmmN[[i]] <- intervals
}
interval_lmmN <- do.call(rbind.data.frame, interval_lmmN)
interval_lmmN$pvalue_alpha <- ifelse(interval_lmmN$p.value <= alpha, paste("≤", alpha), paste(">", alpha))
```

Prettier performance indexes and fertilizer type.

``` r
interval_lmmN <- interval_lmmN %>%
  mutate(performance = factor(performance,
                              labels = c("TAcy~plain('(')~mg~100^{-1}~plain(')')",
                                         "{}^o~Brix",
                                         "Firmness~plain('(')~N~sec^{-1}~plain(')')",
                                         "Berry~weight~plain('(')~g~plain(')')",
                                         "Berry~yield~plain('(')~Mg~ha^{-1}~plain(')')",
                                         "Reproductive~upright~plain('(')~m^{-2}~plain(')')",
                                         "Flower~count~plain('(')~m^{-2}~plain(')')",
                                         "Flower~per~repr.~upright~plain('(')~m^{-2}~plain(')')",
                                         "Fruiting~upright~plain('(')~m^{-2}~plain(')')",
                                         "Berry~count~plain('(')~m^{-2}~plain(')')",
                                         "Berry~per~fruiting~upright~plain('(')~m^{-2}~plain(')')",
                                         "Fruit~set~plain('(%)')")),
         rowname = factor(rowname,
                          labels = c('Fertilizer: 24-5-11',
                                     'Fertilizer:  6-1-1',
                                     'Fertilizer:  8-0-0')))
```

Slopes and their 95% confidence intervals are plotted for each
performance index.

``` r
plot_cols <- 4
plot_rows <- 3

interval_lmmN %>% ggplot(aes(x = est., y = rowname)) +
  facet_wrap(. ~ performance, scales = "free", labeller = label_parsed, switch = "x") +
  geom_vline(xintercept = 0, lty = 1) +
  geom_segment(mapping = aes(x = lower, xend = upper, yend = rowname), size = 1) + # , colour = pvalue_alpha
  geom_point(size = 3) + # aes(colour = pvalue_alpha), 
  geom_text(aes(x = lower, label = signif(lower, 3)), hjust = 1.2) +
  geom_label(aes(label = signif(est., 3))) +
  geom_text(aes(x = upper, label = signif(upper, 3)), hjust = -0.2) +
  labs(y = "", x = "") +
  scale_x_continuous(expand = expand_scale(mult = c(0.5, 0.5))) +
  #theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        strip.placement = "outside")
```

![](statistics_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggsave("images/sources-nitrogen.pdf", width = plot_cols * 5, height = plot_rows * 2, dpi=300)
ggsave("images/sources-nitrogen.png", width = plot_cols * 5, height = plot_rows * 2, dpi=300)
```

### Effect of fertlizers and doses

For all fertlizers and perfomance indexes, we run a linear or quadratic
model (disregarding the effect of fertilizer types for nitrogen).

``` r
alpha <- 0.05
lmm <- list()
pred <- list()

for (i in 1:nrow(model_conditions)) {
  performance_i <- as.character(model_conditions[i, 1])
  treatment_i <- model_conditions[i, 2]
  model_type_i <- model_conditions[i, 4]
  
  table_mm_i <- pr %>%
    filter(Fertilizer == treatment_i) %>%
    select(one_of(c('Year','Site','Bloc','Rate','Cropping_system', performance_i))) %>%
    drop_na()
  
  if (model_type_i == 'linear') {
    lmm[[i]] <- lme(as.formula(paste0(performance_i, " ~ Rate")),
                    random = ~ 1 + 1|Year/Site/Bloc,
                    data = table_mm_i)
    p_value <- summary(lmm[[i]])$tTable[2, 5]
    slope <- coef(lmm[[i]])[1, 2]
  } else if (model_type_i == 'quadratic') {
    lmm[[i]] <- lme(as.formula(paste0(performance_i, " ~ Rate + I(Rate^2)")),
                    random = ~ 1 + 1|Year/Site/Bloc,
                    data = table_mm_i)
    p_value <- summary(lmm[[i]])$tTable[3, 5]
    slope <- coef(lmm[[i]])[1, 3]
  } else if (model_type_i == 'none') {
    lmm[[i]] <- NA
    p_value <- NA
    slope <- NA
  } else {
    print("Model type not recognized. Choose linear or quadratic.")
  }
  
  if (any(is.na(lmm[[i]]))) {
    pred[[i]] <- NA
  } else {
    x_seq <- expand.grid(Rate = seq(0, max(table_mm_i$Rate), length=20),
                         Year = c(2016))
    y_seq <- predict(lmm[[i]], newdata = x_seq, level = 0)
    pred[[i]] <- data.frame(Rate = x_seq$Rate,
                            performance = y_seq,
                            Fertilizer = treatment_i,
                            performance_index = performance_i,
                            p_value = p_value,
                            slope = slope)
  }
}
pred <- do.call(rbind.data.frame, pred) # list to data frame
pred$pvalue_alpha <- factor(ifelse(pred$p_value <= alpha, paste("≤", alpha), paste(">", alpha)))
```

Before plotting, the original data are gathered by performance indexes.
We also order and prettify performance categories and nutrients.

``` r
pred <- pred %>%
  mutate(performance_index_pretty = factor(performance_index,
                                           labels = c("TAcy~plain('(')~mg~100^{-1}~plain(')')",
                                                      "{}^o~Brix",
                                                      "Firmness~plain('(')~N~sec^{-1}~plain(')')",
                                                      "Berry~weight~plain('(')~g~plain(')')",
                                                      "Berry~yield~plain('(')~Mg~ha^{-1}~plain(')')",
                                                      "Reproductive~upright~plain('(')~m^{-2}~plain(')')",
                                                      "Flower~count~plain('(')~m^{-2}~plain(')')",
                                                      "Flower~per~repr.~upright~plain('(')~m^{-2}~plain(')')",
                                                      "Fruiting~upright~plain('(')~m^{-2}~plain(')')",
                                                      "Berry~count~plain('(')~m^{-2}~plain(')')",
                                                      "Berry~per~fruiting~upright~plain('(')~m^{-2}~plain(')')",
                                                      "Fruit~set~plain('(%)')")))
```

``` r
pr_tidy <- pr %>%
  gather(key = performance_index, value = performance, performance_index) %>%
  mutate(performance_index = factor(performance_index),
         performance_index_pretty = factor(performance_index,
                                           labels = c("Berry~weight~plain('(')~g~plain(')')",
                                                      "{}^o~Brix",
                                                      "Firmness~plain('(')~N~sec^{-1}~plain(')')",
                                                      "Flower~count~plain('(')~m^{-2}~plain(')')",
                                                      "Flower~per~repr.~upright~plain('(')~m^{-2}~plain(')')",
                                                      "Reproductive~upright~plain('(')~m^{-2}~plain(')')",
                                                      "Berry~count~plain('(')~m^{-2}~plain(')')",
                                                      "Berry~per~fruiting~upright~plain('(')~m^{-2}~plain(')')",
                                                      "Fruit~set~plain('(%)')",
                                                      "Fruiting~upright~plain('(')~m^{-2}~plain(')')",
                                                      "TAcy~plain('(')~mg~100^{-1}~plain(')')",
                                                      "Berry~yield~plain('(')~Mg~ha^{-1}~plain(')')")),
         Fertilizer = factor(Fertilizer, levels = c("N", "P", "K", "Mg", "S", "Cu", "B")))
```

Classifying performance indexes.

``` r
quality_index <- c("TAcy", "Brix", "Firmness", "Berry_weight", "Yield")
physiology_index <- c('Flower_stem', 'Flower', 'Flower_per_stem', 'Fruit_stem', 
                      'Fruit', 'Fruit_per_stem', 'Fruit_set')
```

Results will be plotted by nutrient and performance types.

#### Macro-elements

##### Quality

``` r
plot_cols <- 5
plot_rows <- 3
pred_gg <- pred %>% filter(Fertilizer %in% c("N", "P", "K") & performance_index %in% quality_index) %>% drop_na()

pr_tidy %>%
  filter(Fertilizer %in% c("N", "P", "K") & performance_index %in% quality_index) %>%
  ggplot(aes(Rate, performance)) +
  facet_grid(performance_index_pretty ~ Fertilizer, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_gg, size = 1) +
  labs(x = expression("Rate (kg ha"^"-1"~")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  theme(axis.title.y = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=270),
        strip.text.x = element_text(size = 18))
```

![](statistics_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggsave("images/grid_macro&quality.pdf", width = plot_cols * 3, height = plot_rows * 2.5, dpi=300)
ggsave("images/grid_macro&quality.png", width = plot_cols * 3, height = plot_rows * 2.5, dpi=300)
```

##### Physiology

``` r
n_perf <- 7
n_elem <- 3
pred_gg <- pred %>% filter(Fertilizer %in% c("N", "P", "K") & performance_index %in% physiology_index) %>% drop_na()

pr_tidy %>%
  filter(Fertilizer %in% c("N", "P", "K") & performance_index %in% physiology_index) %>%
  drop_na() %>%
  ggplot(aes(Rate, performance)) +
  facet_grid(performance_index_pretty ~ Fertilizer, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_gg, size = 1) +
  labs(x = expression("Rate (kg ha"^"-1"~")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  #theme_bw() +
  theme(axis.title.y = element_blank(),
        strip.background = element_rect(fill = "transparent", colour = "transparent"),
        strip.placement = "outside",
        strip.text.y = element_text(angle=270),
        strip.text.x = element_text(size = 18))
```

![](statistics_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggsave("images/grid_macro&physiology.pdf", width = n_elem * 3, height = n_perf * 2.5, dpi=300)
ggsave("images/grid_macro&physiology.png", width = n_elem * 3, height = n_perf * 2.5, dpi=300)
```

#### Secondary and micro-elements

##### Quality

``` r
n_perf <- 5
n_elem <- 3
pred_gg <- pred %>% filter(Fertilizer %in% c("Mg", "B", "Cu") & performance_index %in% quality_index) %>% drop_na()

pr_tidy %>%
  filter(Fertilizer %in% c("Mg", "B", "Cu") & performance_index %in% quality_index) %>%
  drop_na() %>%
  ggplot(aes(Rate, performance)) +
  facet_grid(performance_index_pretty ~ Fertilizer, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_gg, size = 1) +
  labs(x = expression("Rate (kg ha"^"-1"~")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  theme(axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.placement = 'outside',
        strip.text.y = element_text(angle=270),
        strip.text.x = element_text(size = 18))
```

![](statistics_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
ggsave("images/grid_secondary-micro&quality.pdf", width = n_elem * 3, height = n_perf * 2.5, dpi=300)
ggsave("images/grid_secondary-micro&quality.png", width = n_elem * 3, height = n_perf * 2.5, dpi=300)
```

##### Physiology

``` r
n_perf <- 7
n_elem <- 3
pred_gg <- pred %>% filter(Fertilizer %in% c("Mg", "B", "Cu") & performance_index %in% physiology_index) %>% drop_na()

pr_tidy %>%
  filter(Fertilizer %in% c("Mg", "B", "Cu") & performance_index %in% physiology_index) %>%
  drop_na() %>%
  ggplot(aes(Rate, performance)) +
  facet_grid(performance_index_pretty ~ Fertilizer, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.3) +
  geom_line(data = pred_gg, size = 1) +
  labs(x = expression("Rate (kg ha"^"-1"~")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  #theme_bw() +
  theme(axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'transparent'),
        strip.placement = 'outside',
        strip.text.y = element_text(angle=270),
        strip.text.x = element_text(size = 18))
```

![](statistics_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
ggsave("images/grid_secondary-micro&physiology.pdf", width = n_elem * 3, height = n_perf * 2.5, dpi=300)
ggsave("images/grid_secondary-micro&physiology.png", width = n_elem * 3, height = n_perf * 2.5, dpi=300)
```