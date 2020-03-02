Nutrient Requirements of Conventionally and Organically Grown Cranberry
(*Vaccinium macrocarpon* Ait.) - Statistics
================
Serge-Étienne Parent

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

# Initiate session

To initiate the session, we load the following libraries.

``` r
library("tidyverse") # generic data handling and plotting
library("GGally")
library("nlme") # mixed models
# library("grid")
# library("gridExtra")
```

All data are placed in a single csv file.

``` r
data_init <- read_csv(file = "data/data_init.csv", guess_max = 1729)
```

## Arrange data

A couple of data handling is needed for the analysis. First, some
variable types must be changed. First, TAcy is a performance indicator.
It is stored in two different columns, depending on the methodology. It
merge in a single column depending on the presence/absence of data.

``` r
data_init <- data_init %>%
  mutate(TAcy = case_when(
    !is.na(Fruit_Tacy_labo_mg_Anth_100g) ~ Fruit_Tacy_labo_mg_Anth_100g,
    TRUE ~ Fruit_Tacy_image_mg_Anth_100g_515nm
  ))
```

To go on with subsequent operations, I need reference tables. The first
one links nutrients to the name of the column where the dosage value is
stored.

``` r
dose_df <- tibble(
  dose_colname = c(
    "Fertilizer-tot-dose_N_kg_ha", "Fertilizer-tot-dose_P_kg_ha",
    "Fertilizer-tot-dose_K_kg_ha", "Fertilizer-tot-dose_Mg_kg_ha",
    "Fertilizer-tot-dose_B_kg_ha", "Fertilizer-tot-dose_Cu_kg_ha"
  ),
  dose_name = c("N", "P", "K", "Mg", "B", "Cu")
)
```

The second reference table links crop performance to the columns where
the value is stored, and label which will be used for plotting later on.

``` r
performance_df <- tibble(
  performance_name = c(
    "TAcy", "Brix", "Firmness", "Berry_weight",
    "Berry_moisture", "Yield",
    "Fruit_stem", "Fruit"
  ),
  performance_colname = c(
    "TAcy", "Fruit_Brix_degreeBrix", "Fruit_Firmness_N_s", "Fruit_weight_g",
    "Fruit_moisture_berry_perc", "Fruit_yield_Mg_ha",
    "Fruiting_upright_Nb_m2", "Nb_Fruit_m2"
  ),
  performance_labels = c(
    "TAcy~plain('(')~mg~100^{plain('-1')}~plain(')')",
    "{}^o~Brix",
    "Firmness~plain('(')~N~sec^{plain('-1')}~plain(')')",
    "Berry~weight~plain('(')~g~plain(')')",
    "Berry~moisture~plain('(')~plain('%')~plain(')')",
    "Berry~yield~plain('(')~Mg~ha^{plain('-1')}~plain(')')",
    # "Reproductive~upright~plain('(')~m^{plain('-2')}~plain(')')",
    # "Flower~count~plain('(')~m^{plain('-2')}~plain(')')",
    # "Flower~per~repr.~upright~plain('(')~m^{plain('-2')}~plain(')')",
    # "Fruiting~upright~plain('(')~m^{plain('-2')}~plain(')')",
    "Berry~count~plain('(')~m^{plain('-2')}~plain(')')",
    "Berry~per~fruiting~upright~plain('(')~m^{plain('-2')}~plain(')')" # "Fruit~set~plain('(%)')",
  ),
  performance_type = c(
    "Quality", "Quality", "Quality", "Quality",
    "Quality", "Quality",
    "Physiology", "Physiology"
  )
)
```

The next chunk creates a column where I store the dose of the
corresponding trial.

``` r
data_init$Dose_trial <- NA
for (i in seq_along(dose_df$dose_name)) {
  filter_i <- data_init$Nutrient_tested == dose_df$dose_name[i]
  filter_i[is.na(filter_i)] <- FALSE
  data_init$Dose_trial[filter_i] <- data_init[filter_i, dose_df$dose_colname[i]] %>% pull()
}
```

# Yield componenents

This section is a correlation analysis between performance
components.

<div class="figure" style="text-align: center">

<img src="images/correlation-physiology.png" alt="Correlations between physiological measeurements" width="2400" />

<p class="caption">

Correlations between physiological
measeurements

</p>

</div>

<div class="figure" style="text-align: center">

<img src="images/lm-physiology.png" alt="Linear relationships between physiological measurements and berry yield." width="1800" />

<p class="caption">

Linear relationships between physiological measurements and berry yield.

</p>

</div>

# Berry yield, weight and quality

Berry concentrations summaries.

``` r
nutrient_exportation <- data_init %>%
  select(Fruit_yield_Mg_ha, Fruit_moisture_berry_perc, starts_with("Fruit_") & ends_with("g_kg"), -Fruit_S_g_kg_ICP) %>%
  mutate(dry_weight_kg_ha = Fruit_yield_Mg_ha * 1000 * (100 - Fruit_moisture_berry_perc) / 100) %>%
  select(-Fruit_yield_Mg_ha, -Fruit_moisture_berry_perc) %>%
  mutate_all(~ . * dry_weight_kg_ha / 1000) %>%
  # g_nutrient/kg_fruit * kg_fruit/ha -> g_nutrient/ha / 1000 -> kg_nutrient / ha
  select(-dry_weight_kg_ha) %>%
  summarise_all(funs(mean), na.rm = TRUE) %>%
  t()
```

    ## Warning: funs() is soft deprecated as of dplyr 0.8.0
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once per session.

``` r
nutrient_concentration <- data_init %>%
  select(starts_with("Fruit_") & ends_with("g_kg"), -Fruit_S_g_kg_ICP) %>%
  summarise_all(funs(mean), na.rm = TRUE) %>%
  t()

nutrient_name <- data_init %>%
  select(starts_with("Fruit_") & ends_with("g_kg"), -Fruit_S_g_kg_ICP) %>%
  names() %>%
  sub(".*Fruit_ *(.*?) *_g_kg.*", "\\1", .)

tibble(
  nutrient_name,
  nutrient_concentration,
  nutrient_exportation
) %>%
  print(n = 14)
```

    ## # A tibble: 14 x 3
    ##    nutrient_name nutrient_concentration[,1] nutrient_exportation[,1]
    ##    <chr>                              <dbl>                    <dbl>
    ##  1 C                              438.                     1907.    
    ##  2 S                                0.569                     2.83  
    ##  3 N                                2.86                     15.3   
    ##  4 P                                0.647                     3.25  
    ##  5 K                                5.97                     32.7   
    ##  6 Ca                               0.447                     2.47  
    ##  7 Mg                               0.372                     2.10  
    ##  8 B                                0.00602                   0.0207
    ##  9 Cu                               0.00343                   0.0136
    ## 10 Zn                               0.0177                    0.0987
    ## 11 Mn                               0.0146                    0.0616
    ## 12 Fe                               0.0283                    0.140 
    ## 13 Al                               0.0738                    0.345 
    ## 14 Si                               0.105                     0.430

# Mixed modeling

I create table where I retain only the fertilizers columns, performance
columns, fertilizer type and columns describing experimental design.
Then, yield was markedly reduced by 75% in 2017 due to adverse
meteorological conditions in winter and spring. Year 2017 was thus
discarded, I’m considering only data before 2017.

``` r
data_stats <- data_init %>%
  select(
    Year_of_experiment, Field_no, Treatment_no, Block_no, Cropping_system, Nutrient_tested, Fertilizer_formula, Dose_trial,
    dose_df$dose_colname, performance_df$performance_colname
  ) %>%
  filter(Year_of_experiment != 2017) %>%
  rename(
    Year = Year_of_experiment,
    Field = Field_no,
    Treatment = Treatment_no,
    Block = Block_no,
    Fertilizer_trial = Nutrient_tested
  ) %>%
  droplevels()

data_stats %>%
  sample_n(10)
```

    ## # A tibble: 10 x 22
    ##     Year Field Treatment Block Cropping_system Fertilizer_trial Fertilizer_form…
    ##    <dbl> <chr> <chr>     <dbl> <chr>           <chr>            <chr>           
    ##  1  2016 K13   <NA>         NA Conventional    <NA>             <NA>            
    ##  2  2018 45    Mg12          2 Conventional    Mg               <NA>            
    ##  3  2002 CA02  P3            3 Conventional    P                11-52-0         
    ##  4  2000 CA02  P07           2 Conventional    P                11-52-0         
    ##  5  2014 K09   <NA>         NA Conventional    <NA>             <NA>            
    ##  6  2010 7     <NA>         NA Organic         <NA>             <NA>            
    ##  7  2016 P06   <NA>         NA Conventional    <NA>             <NA>            
    ##  8  1990 <NA>  <NA>         NA <NA>            <NA>             <NA>            
    ##  9  2018 72H   <NA>         NA <NA>            <NA>             10-10-20        
    ## 10  2016 45    Cu2           1 Conventional    Cu               0-0-0-25        
    ## # … with 15 more variables: Dose_trial <dbl>,
    ## #   `Fertilizer-tot-dose_N_kg_ha` <dbl>, `Fertilizer-tot-dose_P_kg_ha` <dbl>,
    ## #   `Fertilizer-tot-dose_K_kg_ha` <dbl>, `Fertilizer-tot-dose_Mg_kg_ha` <dbl>,
    ## #   `Fertilizer-tot-dose_B_kg_ha` <dbl>, `Fertilizer-tot-dose_Cu_kg_ha` <dbl>,
    ## #   TAcy <dbl>, Fruit_Brix_degreeBrix <dbl>, Fruit_Firmness_N_s <dbl>,
    ## #   Fruit_weight_g <dbl>, Fruit_moisture_berry_perc <dbl>,
    ## #   Fruit_yield_Mg_ha <dbl>, Fruiting_upright_Nb_m2 <dbl>, Nb_Fruit_m2 <dbl>

Combination of performance-treatment will be subjected to a mixed model,
but some with a linear model, and other with a quadratic model.
Moreover, some combinaisons of performance-treatment were not tested. We
aim to loop each possible combinaison to fit a linear model, a quadratic
model or no model at all. The first step of this process is to create a
grid of these combinaisons.

``` r
model_conditions <- expand.grid(
  performance_name = performance_df$performance_name,
  Fertilizer_trial = c("N", "P", "K", "Mg", "B", "Cu")
)
model_conditions$index <- 1:nrow(model_conditions)
model_conditions %>%
  sample_n(5)
```

    ##   performance_name Fertilizer_trial index
    ## 1            Yield               Mg    30
    ## 2   Berry_moisture                K    21
    ## 3            Yield                N     6
    ## 4   Berry_moisture               Cu    45
    ## 5             Brix                P    10

We are adding a column to the grid containing the information on which
option to choose. By default, the option is linear.

``` r
model_conditions$model_type <- "linear"
```

Where the modeling threw erors due to lack of data, I imposed `"none"`
as
`model_type`.

``` r
model_conditions$model_type[model_conditions$Fertilizer == "B" & model_conditions$performance_index == "Brix"] <- "none"
```

The following instructions impose a quadratic model to some
combinaisons. We selected quadratic where it visually seemed
appropriate.

``` r
model_conditions$model_type[model_conditions$Fertilizer == "N" & model_conditions$performance_index == "Yield"] <- "quadratic"
model_conditions$model_type[model_conditions$Fertilizer == "N" & model_conditions$performance_index == "Berry_weight"] <- "quadratic"
model_conditions$model_type[model_conditions$Fertilizer == "K" & model_conditions$performance_index == "Yield"] <- "quadratic"
model_conditions$model_type[model_conditions$Fertilizer == "K" & model_conditions$performance_index == "Berry_weight"] <- "quadratic"
```

### Effect of nitrogen source

We hypothesized (\(H_0\)) that cranberry quality and physiology don’t
respond N sources after dicarding the dose effect. We test the linear
and quadratic effects of dose and the linear effect of fertilizer type.
We extract the effect of the fertilizer type from the model. The
following cell runs mixed models on nitrogen trials and save them in a
list.

``` r
alpha <- 0.05
lmm_N <- list()
interval_lmmN <- list()

mc_N <- model_conditions %>%
  filter(Fertilizer_trial == "N")

for (i in 1:nrow(mc_N)) {
  performance_i <- performance_df$performance_colname[performance_df$performance_name == as.character(mc_N[i, 1])]
  model_type_i <- mc_N[i, 4]
  
  table_mm_i <- data_stats %>%
    filter(Fertilizer_trial == "N") %>%
    select(one_of(c("Year", "Field", "Treatment", "Block", "Fertilizer-tot-dose_N_kg_ha", "Cropping_system", "Fertilizer_formula", performance_i))) %>%
    rename(Dose = "Fertilizer-tot-dose_N_kg_ha") %>%
    drop_na()
  
  table_mm_i$Fertilizer_formula <- relevel(factor(table_mm_i$Fertilizer_formula), ref = "21-0-0")
  
  if (model_type_i == "linear") {
    lmm_N[[i]] <- lme(as.formula(paste0(performance_i, " ~ Dose + Fertilizer_formula ")),
                      random = ~ 1 + 1 | Year / Field / Treatment,
                      data = table_mm_i
    )
  } else if (model_type_i == "quadratic") {
    lmm_N[[i]] <- lme(as.formula(paste0(performance_i, " ~ Dose + I(Dose^2) + Fertilizer_formula")),
                      random = ~ 1 + 1 | Year / Field / Treatment,
                      data = table_mm_i
    )
  } else if (model_type_i == "none") {
    lmm_N[[i]] <- NA
  } else {
    print("Model type not recognized. Choose linear or quadratic.")
  }
  
  tTable <- summary(lmm_N[[i]])$tTable %>%
    data.frame() %>%
    rownames_to_column() %>%
    filter(str_detect(rowname, "Fert"))
  
  intervals <- intervals(lmm_N[[i]], which = "fixed")[[1]] %>%
    data.frame() %>%
    rownames_to_column() %>%
    filter(str_detect(rowname, "Fert")) %>%
    bind_cols(tTable["p.value"])
  
  intervals$rowname <- tTable$rowname <- gsub("Fertilizer_formula*", "", tTable$rowname)
  
  intervals$performance_name <- mc_N$performance_name[i]
  intervals$performance_labels <- performance_df$performance_labels[performance_df$performance_colname == performance_i]
  
  interval_lmmN[[i]] <- intervals
}
```

Transform the `interval_lmmN` list to a table.

``` r
interval_lmmN <- do.call(rbind.data.frame, interval_lmmN)
interval_lmmN$pvalue_alpha <- ifelse(interval_lmmN$p.value <= alpha, paste("≤", alpha), paste(">", alpha))
```

Slopes and their 95% confidence intervals are plotted for each
performance index.

``` r
plot_cols <- 4
plot_rows <- 3

interval_lmmN %>% ggplot(aes(x = est., y = rowname)) +
  facet_wrap(. ~ performance_labels, scales = "free", labeller = label_parsed, switch = "x") +
  geom_vline(xintercept = 0, lty = 1) +
  geom_segment(mapping = aes(x = lower, xend = upper, yend = rowname), size = 1) + # , colour = pvalue_alpha
  geom_point(size = 3) +
  geom_text(aes(x = lower, label = signif(lower, 3)), hjust = 1.2) +
  geom_label(aes(label = signif(est., 3))) +
  geom_text(aes(x = upper, label = signif(upper, 3)), hjust = -0.2) +
  labs(y = "", x = "") +
  scale_x_continuous(expand = expand_scale(mult = c(0.5, 0.5))) +
  theme_bw() +
  theme(
    strip.text.y = element_text(angle = 0),
    axis.title.x = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.placement = "outside"
  ) +
  ggsave("images/sources-nitrogen.pdf", width = plot_cols * 5, height = plot_rows * 2, dpi = 300) +
  ggsave("images/sources-nitrogen.png", width = plot_cols * 5, height = plot_rows * 2, dpi = 300)
```

![](01_statistics_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Effect of fertlizers and doses

For all fertlizers and perfomance indexes, we run a linear or quadratic
model (disregarding the effect of fertilizer types).

``` r
alpha <- 0.05
lmm <- list()
pred <- list()

for (i in 1:nrow(model_conditions)) {
  performance_i <- performance_df$performance_colname[performance_df$performance_name == as.character(model_conditions[i, 1])]
  treatment_i <- model_conditions[i, 2]
  # dose_i <- dose_df$dose_colname[dose_df$dose_name == treatment_i]
  model_type_i <- model_conditions[i, 4]
  
  table_mm_i <- data_stats %>%
    filter(Fertilizer_trial == treatment_i) %>%
    select(one_of(c("Year", "Field", "Treatment", "Block", "Cropping_system", "Dose_trial", performance_i))) %>%
    rename(Dose = Dose_trial) %>%
    drop_na()
  
  if (model_type_i == "linear") {
    lmm[[i]] <- lme(as.formula(paste0(performance_i, " ~ Dose")),
                    random = ~ 1 + 1 | Year / Field / Treatment,
                    data = table_mm_i
    )
    p_value <- summary(lmm[[i]])$tTable[2, 5]
    slope <- coef(lmm[[i]])[1, 2]
  } else if (model_type_i == "quadratic") {
    lmm[[i]] <- lme(as.formula(paste0(performance_i, " ~ Dose + I(Dose^2)")),
                    random = ~ 1 + 1 | Year / Field / Treatment,
                    data = table_mm_i
    )
    p_value <- summary(lmm[[i]])$tTable[3, 5]
    slope <- coef(lmm[[i]])[1, 3]
  } else if (model_type_i == "none") {
    lmm[[i]] <- NA
    p_value <- NA
    slope <- NA
  } else {
    print("Model type not recognized. Choose linear or quadratic.")
  }
  
  if (any(is.na(lmm[[i]]))) {
    pred[[i]] <- NA
    print(paste("Model", i, "is NA"))
  } else {
    x_seq <- expand.grid(Dose = seq(0, max(table_mm_i$Dose), length = 20))
    y_seq <- predict(lmm[[i]], newdata = x_seq, level = 0)
    pred[[i]] <- data.frame(
      Dose_trial = x_seq$Dose,
      performance_value = y_seq,
      # label = performance_df$labels[performance_df$colname == performance_i],
      Fertilizer_trial = treatment_i,
      performance_colname = performance_i,
      p_value = p_value,
      slope = slope
    )
  }
}
```

    ## Warning in pt(-abs(tVal), fDF): production de NaN
    
    ## Warning in pt(-abs(tVal), fDF): production de NaN
    
    ## Warning in pt(-abs(tVal), fDF): production de NaN
    
    ## Warning in pt(-abs(tVal), fDF): production de NaN

Tidying the predicted models.

``` r
pred_tidy <- do.call(rbind.data.frame, pred) # list to data frame
pred_tidy$pvalue_alpha <- factor(ifelse(pred_tidy$p_value <= alpha, paste("≤", alpha), paste(">", alpha)))
pred_tidy <- pred_tidy %>%
  left_join(performance_df, by = "performance_colname") %>%
  select(-performance_colname)
```

    ## Warning: Column `performance_colname` joining factor and character vector,
    ## coercing into character vector

``` r
pred_tidy %>% sample_n(10)
```

    ##    Dose_trial performance_value Fertilizer_trial      p_value       slope
    ## 1    79.98256          7.177035                N 7.799831e-09 -0.01201912
    ## 2    93.78947         35.030878                P 5.341179e-01 -0.01164211
    ## 3    26.05263         35.819478                P 5.341179e-01 -0.01164211
    ## 4    16.83843         28.231034                N 1.352905e-12  0.32417712
    ## 5    26.05263         32.460454                P 5.752483e-01 -0.01076260
    ## 6    27.50000          7.333333                P          NaN -0.02493333
    ## 7    46.89474         35.576832                P 5.341179e-01 -0.01164211
    ## 8     2.00000          5.926703               Cu 4.072653e-01  0.18589848
    ## 9    79.98256          5.689038                N 1.643050e-02 -0.00514452
    ## 10   58.19284         28.010241                K 2.378419e-01  0.06136229
    ##    pvalue_alpha performance_name
    ## 1        ≤ 0.05             Brix
    ## 2        > 0.05            Yield
    ## 3        > 0.05            Yield
    ## 4        ≤ 0.05            Yield
    ## 5        > 0.05             TAcy
    ## 6          <NA>             Brix
    ## 7        > 0.05            Yield
    ## 8        > 0.05         Firmness
    ## 9        ≤ 0.05         Firmness
    ## 10       > 0.05             TAcy
    ##                                       performance_labels performance_type
    ## 1                                              {}^o~Brix          Quality
    ## 2  Berry~yield~plain('(')~Mg~ha^{plain('-1')}~plain(')')          Quality
    ## 3  Berry~yield~plain('(')~Mg~ha^{plain('-1')}~plain(')')          Quality
    ## 4  Berry~yield~plain('(')~Mg~ha^{plain('-1')}~plain(')')          Quality
    ## 5        TAcy~plain('(')~mg~100^{plain('-1')}~plain(')')          Quality
    ## 6                                              {}^o~Brix          Quality
    ## 7  Berry~yield~plain('(')~Mg~ha^{plain('-1')}~plain(')')          Quality
    ## 8     Firmness~plain('(')~N~sec^{plain('-1')}~plain(')')          Quality
    ## 9     Firmness~plain('(')~N~sec^{plain('-1')}~plain(')')          Quality
    ## 10       TAcy~plain('(')~mg~100^{plain('-1')}~plain(')')          Quality

Tidying the `data_stats` table before going further.

``` r
data_tidy <- data_stats %>%
  gather(key = "performance_colname", value = "performance_value", performance_df$performance_colname) %>%
  left_join(performance_df, by = "performance_colname") %>%
  select(-performance_colname)

quality_index <- performance_df$performance_name[performance_df$performance_type == "Quality"]
physiology_index <- performance_df$performance_name[performance_df$performance_type == "Physiology"]
```

Results will be plotted by nutrient and performance types.

#### Macro-elements

##### Quality

``` r
n_perf <- length(quality_index)
n_elem <- 3

data_gg <- data_tidy %>%
  filter(Fertilizer_trial %in% c("N", "P", "K") & performance_name %in% quality_index) %>%
  mutate(Fertilizer_trial = factor(Fertilizer_trial, levels = c("N", "P", "K"))) %>%
  select(Dose_trial, performance_value, performance_labels, Fertilizer_trial) %>%
  drop_na()
pred_gg <- pred_tidy %>%
  filter(Fertilizer_trial %in% c("N", "P", "K") & performance_name %in% quality_index) %>%
  drop_na()

ggplot(data = data_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  facet_grid(performance_labels ~ Fertilizer_trial, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.1) +
  geom_line(pred_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  labs(x = expression("Dose (kg ha"^"-1" ~ ")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, size = 12),
    strip.text.x = element_text(size = 12)
  ) +
  ggsave("images/grid_macro&quality.pdf", width = n_elem * 3, height = n_perf * 2.5, dpi = 300) +
  ggsave("images/grid_macro&quality.png", width = n_elem * 3, height = n_perf * 2.5, dpi = 300)
```

![](01_statistics_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

##### Physiology

``` r
n_perf <- length(physiology_index)
n_elem <- 3

data_gg <- data_tidy %>%
  filter(Fertilizer_trial %in% c("N", "P", "K") & performance_name %in% physiology_index) %>%
  mutate(Fertilizer_trial = factor(Fertilizer_trial, levels = c("N", "P", "K"))) %>%
  select(Dose_trial, performance_value, performance_labels, Fertilizer_trial) %>%
  drop_na()
pred_gg <- pred_tidy %>%
  filter(Fertilizer_trial %in% c("N", "P", "K") & performance_name %in% physiology_index) %>%
  drop_na()

ggplot(data = data_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  facet_grid(performance_labels ~ Fertilizer_trial, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.1) +
  geom_line(pred_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  labs(x = expression("Dose (kg ha"^"-1" ~ ")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, size = 12),
    strip.text.x = element_text(size = 12)
  ) +
  ggsave("images/grid_macro&physiology.pdf", width = n_elem * 3, height = n_perf * 2.8, dpi = 300) +
  ggsave("images/grid_macro&physiology.png", width = n_elem * 3, height = n_perf * 2.8, dpi = 300)
```

![](01_statistics_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

#### Secondary and micro-elements

##### Quality

``` r
n_perf <- length(quality_index)
n_elem <- 3

data_gg <- data_tidy %>%
  filter(Fertilizer_trial %in% c("Mg", "B", "Cu") & performance_name %in% quality_index) %>%
  select(Dose_trial, performance_value, performance_labels, Fertilizer_trial) %>%
  drop_na()
pred_gg <- pred_tidy %>%
  filter(Fertilizer_trial %in% c("Mg", "B", "Cu") & performance_name %in% quality_index) %>%
  drop_na()

ggplot(data = data_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  facet_grid(performance_labels ~ Fertilizer_trial, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.1) +
  geom_line(pred_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  labs(x = expression("Dose (kg ha"^"-1" ~ ")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, size = 12),
    strip.text.x = element_text(size = 12)
  ) +
  ggsave("images/grid_secondary-micro&quality.pdf", width = n_elem * 3, height = n_perf * 2.5, dpi = 300) +
  ggsave("images/grid_secondary-micro&quality.png", width = n_elem * 3, height = n_perf * 2.5, dpi = 300)
```

![](01_statistics_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

##### Physiology

``` r
n_perf <- length(physiology_index)
n_elem <- 3

data_gg <- data_tidy %>%
  filter(Fertilizer_trial %in% c("Mg", "B", "Cu") & performance_name %in% physiology_index) %>%
  select(Dose_trial, performance_value, performance_labels, Fertilizer_trial) %>%
  drop_na()
pred_gg <- pred_tidy %>%
  filter(Fertilizer_trial %in% c("Mg", "B", "Cu") & performance_name %in% physiology_index) %>%
  drop_na()

ggplot(data = data_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  facet_grid(performance_labels ~ Fertilizer_trial, scales = "free", labeller = label_parsed, switch = "y") +
  geom_point(alpha = 0.1) +
  geom_line(pred_gg, mapping = aes(x = Dose_trial, y = performance_value)) +
  labs(x = expression("Dose (kg ha"^"-1" ~ ")"), y = "Performance") +
  geom_label(data = pred_gg, aes(label = paste("Slope =", signif(slope, 3))), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
  geom_label(data = pred_gg, aes(label = paste("p =", signif(p_value, 3))), x = -Inf, y = Inf, hjust = -0.18, vjust = 2.5) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, size = 12),
    strip.text.x = element_text(size = 12)
  ) +
  ggsave("images/grid_secondary-micro&physiology.pdf", width = n_elem * 3, height = n_perf * 2.8, dpi = 300) +
  ggsave("images/grid_secondary-micro&physiology.png", width = n_elem * 3, height = n_perf * 2.8, dpi = 300)
```

![](01_statistics_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
