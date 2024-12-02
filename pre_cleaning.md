Pre Cleaning
================

# rename main variables

``` r
ukb_data = ukb_data |>
  rename(
    age = n_21022_0_0,
    sex = n_31_0_0,
    ethnicity = n_21000_0_0,
    townsend = n_22189_0_0,
    medication = n_6177_0_0,
    income = n_738_0_0,
    father_ill = n_20107_0_0,
    mother_ill = n_20110_0_0,
    mental_health = n_29000_0_0,
    marriage = n_29088_0_0,
    baseline_date = s_53_0_0,
    lost_date = s_191_0_0,
    death_date = s_40000_0_0,
    record_origin = s_40022_0_0,
    death_origin = s_40020_0_0
  )
```

# Processing with covariates

The potential variables we use comprise demographic data (age, sex,
income, marriage, Townsend index), lifestyle (diet quality\[assessed
using DASH diet score\], sleep hours, smoking status, weekly alcohol
intake, \[by calculating total alcohol in ml for the participants drink
in a normal week\]) biochemical laboratory tests (blood lipid, blood
glucose, blood pressure, blood immune cell concentration) and disease
outcome (NAFLD, Cirrhosis)

## Deal with income

``` r
ukb_data = ukb_data |>
  filter(income >= 0)
```

## Deal with ethnicity

``` r
ukb_data = ukb_data |>
  mutate(
    ethnicity_cat = case_when(
      ethnicity %in% c(1001, 1002, 1003) ~ 1,
      TRUE ~ 0
    )
  )
```

## Deal with physical activity, generate MET score

``` r
ukb_data = ukb_data |>
  rename(
    MET_walk = n_22037_0_0,
    MET_moderate = n_22038_0_0,
    MET_vigorous = n_22039_0_0
  ) |>
  mutate(total_MET = MET_moderate + MET_vigorous)

ukb_data |>
  filter(!is.na(total_MET)) |>
  ggplot(aes(x = total_MET)) +
  geom_density(fill = "skyblue") +
  theme_minimal() +
  labs(
    title = "Total MET Score Density Plot",
    x = "Total MET Score",
    y = "Density"
  )
```

![](pre_cleaning_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Deal with diet variables

diet quality was calculated using DASH score, which is the sum of 7
categories. We only include participants having clear memory of portion
they took, we treated “less than one” portion as 0.5. In each 7
categories, we mark the score as 1 if participants meet the required
portion of food and score as 0 if they do not.

### Rename food variables

``` r
ukb_data = ukb_data |>
  rename(
    cooked_vegetable_0 = n_1289_0_0,
    salad_raw_vegetable_0 = n_1299_0_0,
    fresh_fruit_0 = n_1309_0_0,
    dried_fruit_0 = n_1319_0_0,
    oily_fish_0 = n_1329_0_0,
    non_oily_fish_0 = n_1339_0_0,
    processed_meat_0 = n_1349_0_0,
    beef_0 = n_1369_0_0,
    lamb_mutton_0 = n_1379_0_0,
    pork_0 = n_1389_0_0,
    bread_0 = n_1438_0_0,
    bread_type_0 = n_1448_0_0,
    cereal_0 = n_1458_0_0,
    cereal_type_0 = n_1468_0_0
  )
```

### Cooked vegetables

``` r
ukb_data = ukb_data |>
  mutate(
    cooked_vegetable = case_when(
      cooked_vegetable_0 %in% c(-3, -1, -10) ~ NA_real_,
      cooked_vegetable_0 == -10 ~ 0.5,
      TRUE ~ cooked_vegetable_0
    )
  )
```

### Salad/raw vegetable

``` r
ukb_data = ukb_data |>
  mutate(
    salad_raw_vegetable = case_when(
      salad_raw_vegetable_0 %in% c(-3, -1, -10) ~ NA_real_,
      salad_raw_vegetable_0 == -10 ~ 0.5,
      TRUE ~ salad_raw_vegetable_0
    )
  )
```

### Vegetable servings

``` r
ukb_data = ukb_data |>
  mutate(
    healthy_vegetable = case_when(
      ((cooked_vegetable + salad_raw_vegetable) / 3 >= 3 & !is.na(cooked_vegetable) & !is.na(salad_raw_vegetable)) |
        (cooked_vegetable / 3 >= 3 & !is.na(cooked_vegetable)) |
        (salad_raw_vegetable / 3 >= 3 & !is.na(salad_raw_vegetable)) ~ 1,
      TRUE ~ 0
    )
  )
```

**vegetables: ≥ 3 servings/day** was marked as ideal portion with 1
score.

### Fruit variables

``` r
ukb_data = ukb_data |>
  mutate(
    fresh_fruit = case_when(
      fresh_fruit_0 %in% c(-3, -1, -10) ~ NA_real_,
      fresh_fruit_0 == -10 ~ 0.5,
      TRUE ~ fresh_fruit_0
    ),
    dried_fruit = case_when(
      dried_fruit_0 %in% c(-3, -1, -10) ~ NA_real_,
      dried_fruit_0 == -10 ~ 0.5,
      TRUE ~ dried_fruit_0
    )
  ) |>
  mutate(
    healthy_fruit = case_when(
      (fresh_fruit + dried_fruit / 5 >= 3 & !is.na(fresh_fruit) & !is.na(dried_fruit)) |
        (fresh_fruit >= 3 & !is.na(fresh_fruit)) |
        (dried_fruit / 5 >= 3 & !is.na(dried_fruit)) ~ 1,
      TRUE ~ 0
    )
  )
```

**Fruits: ≥ 3 servings/day** was marked as ideal portion with 1 score.

### Fish variables

``` r
ukb_data = ukb_data |>
  mutate(
    oily_fish = case_when(
      oily_fish_0 %in% c(-3, -1) ~ NA_real_,
      TRUE ~ oily_fish_0
    ),
    non_oily_fish = case_when(
      non_oily_fish_0 %in% c(-3, -1) ~ NA_real_,
      TRUE ~ non_oily_fish_0
    ),
    healthy_fish = case_when(
      ((oily_fish + non_oily_fish) >= 4 & !is.na(oily_fish) & !is.na(non_oily_fish)) |
        (oily_fish >= 3 & !is.na(oily_fish)) |
        (non_oily_fish >= 3 & !is.na(non_oily_fish)) ~ 1,
      TRUE ~ 0
    )
  )
```

**fish: \>=2 servings/week** was marked as ideal portion with 1 score.

### Processed meat

``` r
ukb_data = ukb_data |>
  mutate(
    processed_meat = case_when(
      processed_meat_0 %in% c(-3, -1) ~ NA_real_,
      TRUE ~ processed_meat_0
    ),
    healthy_processed_meat = case_when(
      processed_meat <= 2 & !is.na(processed_meat) ~ 1,
      TRUE ~ 0
    )
  )
```

**processed meat: ≤1 serving/week** was marked as ideal portion with 1
score.

### Red meat

``` r
ukb_data = ukb_data |>
  mutate(
    beef = case_when(beef_0 %in% c(-3, -1) ~ NA_real_, TRUE ~ beef_0),
    lamb_mutton = case_when(lamb_mutton_0 %in% c(-3, -1) ~ NA_real_, TRUE ~ lamb_mutton_0),
    pork = case_when(pork_0 %in% c(-3, -1) ~ NA_real_, TRUE ~ pork_0)
  ) |>
  mutate(
    healthy_red_meat = case_when(
      beef + lamb_mutton + pork <= 3 & beef < 3 & lamb_mutton < 3 & pork < 3 &
        !is.na(beef) & !is.na(lamb_mutton) & !is.na(pork) ~ 1,
      TRUE ~ 0
    )
  )
```

**red meat: ≤1.5 serving/week** was marked as ideal portion with 1
score.

### Whole and refined grain

``` r
ukb_data = ukb_data |>
  mutate(
    bread = case_when(
      bread_0 %in% c(-3, -1) ~ NA_real_,
      bread_0 == -10 ~ 0.5,
      TRUE ~ bread_0
    ),
    bread_type = case_when(
      bread_type_0 %in% c(-3, -1) ~ NA_real_,
      TRUE ~ bread_type_0
    ),
    cereal = case_when(
      cereal_0 %in% c(-3, -1) ~ NA_real_,
      cereal_0 == -10 ~ 0.5,
      TRUE ~ cereal_0
    ),
    cereal_type = case_when(
      cereal_type_0 %in% c(-3, -1) ~ NA_real_,
      TRUE ~ cereal_type_0
    )
  ) |>
  mutate(
    healthy_refined_grain = case_when(
      (bread > 1.5 & bread_type %in% c(1, 2, 4) & !is.na(bread) & !is.na(bread_type)) |
        (cereal > 1.5 & cereal_type %in% c(2, 5) & !is.na(cereal) & !is.na(cereal_type)) |
        (bread + cereal > 1.5 & bread_type %in% c(1, 2, 4) & cereal_type %in% c(2, 5) &
           !is.na(bread) & !is.na(cereal)) ~ 0,
      bread_type == 3 & cereal_type %in% c(1, 3, 4) ~ 1,
      bread + cereal <= 1.5 & !is.na(bread) & !is.na(cereal) ~ 1,
      TRUE ~ 0
    ),
    healthy_whole_grain = case_when(
      (bread >= 3 & bread_type == 3 & !is.na(bread) & !is.na(bread_type)) |
        (cereal >= 3 & cereal_type %in% c(1, 3, 4) & !is.na(cereal) & !is.na(cereal_type)) |
        (bread + cereal >= 3 & bread_type == 3 & cereal_type %in% c(1, 3, 4) &
           !is.na(bread) & !is.na(cereal)) ~ 1,
      TRUE ~ 0
    )
  )
```

**refined grain: ≤1.5 serving/week** was marked as ideal portion with 1
score. **whole grain: \>=3 serving/week** was marked as ideal portion
with 1 score.

### Drop intermediate variables

``` r
ukb_data = ukb_data |>
  select(
    -c(cooked_vegetable_0, salad_raw_vegetable_0, fresh_fruit_0, dried_fruit_0,
       oily_fish_0, non_oily_fish_0)
  )
```

### Compute DASH diet quality

``` r
ukb_data = ukb_data |>
  mutate(
    diet_quality = rowSums(select(ukb_data, healthy_vegetable, healthy_fruit,
                                  healthy_fish, healthy_processed_meat,
                                  healthy_red_meat, healthy_refined_grain,
                                  healthy_refined_grain), na.rm = TRUE)
  )

ukb_data |>
  filter(!is.na(diet_quality)) |>
  ggplot(aes(x = diet_quality)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    title = "Diet Quality Score",
    x = "Score",
    y = "Counts"
  )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](pre_cleaning_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Sleep hours

``` r
ukb_data = ukb_data |>
  rename(sleep_hour = n_1160_0_0) |>
  filter(!sleep_hour %in% c(-1, -3))
```

## Smoking

We calculate a smoking score as a portrait of participants history of
smoking and amount of tobacco product usage. From never smoking as
lowest 0, to current smoking as highest 5.

``` r
ukb_data = ukb_data |>
  rename(
    smoke_current = n_1239_0_0,
    smoke_past = n_1249_0_0
  ) |>
  mutate(
    smoke_current = if_else(smoke_current %in% c(-3), NA_real_, smoke_current),
    smoke_past = if_else(smoke_past %in% c(-3), NA_real_, smoke_past),
    smoke_cat = case_when(
      smoke_current == 0 & smoke_past == 4 ~ 0,
      smoke_current == 0 & smoke_past == 3 ~ 1,
      smoke_current == 0 & smoke_past == 2 ~ 2,
      smoke_current == 0 & smoke_past == 1 ~ 3,
      smoke_current == 2 ~ 4,
      smoke_current == 1 ~ 5
    )
  ) |>
  filter(!is.na(smoke_cat))

ukb_data |>
  filter(!is.na(smoke_cat)) |>
  ggplot(aes(x = smoke_cat)) +
  geom_histogram(fill = "pink3") +
  theme_minimal() +
  labs(
    title = "Smoking Status Score",
    x = "Score",
    y = "Counts"
  )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](pre_cleaning_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Alcohol consumption

We calculate alcohol consumption in a normal week. Our assumption is
that beer plus cider have 2 ml per bottle or pints, red wine, white
wine, champagne and fortified wine have 1.5 per glass, and spirits have
1 ml per shot.

``` r
ukb_data = ukb_data |>
  mutate(
    n_1568_0_0 = if_else(n_1568_0_0 %in% c(-1, -3), NA_real_, as.double(n_1568_0_0)),
    redwine_week = n_1568_0_0 * 1.5,
    n_4407_0_0 = if_else(n_4407_0_0 %in% c(-1, -3), NA_real_, as.double(n_4407_0_0)),
    redwine_month = n_4407_0_0 * 1.5 / 4,
    n_1578_0_0 = if_else(n_1578_0_0 %in% c(-1, -3), NA_real_, as.double(n_1578_0_0)),
    champagne_week = n_1578_0_0 * 1.5,
    n_4418_0_0 = if_else(n_4418_0_0 %in% c(-1, -3), NA_real_, as.double(n_4418_0_0)),
    champagne_month = n_4418_0_0 * 1.5 / 4,
    n_1588_0_0 = if_else(n_1588_0_0 %in% c(-1, -3), NA_real_, as.double(n_1588_0_0)),
    beer_week = n_1588_0_0 * 2,
    n_4429_0_0 = if_else(n_4429_0_0 %in% c(-1, -3), NA_real_, as.double(n_4429_0_0)),
    beer_month = n_4429_0_0 * 2 / 4,
    n_1598_0_0 = if_else(n_1598_0_0 %in% c(-1, -3), NA_real_, as.double(n_1598_0_0)),
    spirits_week = n_1598_0_0 * 1,
    n_4440_0_0 = if_else(n_4440_0_0 %in% c(-1, -3), NA_real_, as.double(n_4440_0_0)),
    spirits_month = n_4440_0_0 * 1 / 4,
    n_1608_0_0 = if_else(n_1608_0_0 %in% c(-1, -3), NA_real_, as.double(n_1608_0_0)),
    wine_week = n_1608_0_0 * 1.5,
    n_4451_0_0 = if_else(n_4451_0_0 %in% c(-1, -3), NA_real_, as.double(n_4451_0_0)),
    wine_month = n_4451_0_0 * 1.5 / 4
  )


ukb_data$total_alcohol = rowSums(
  ukb_data[, c("redwine_week", "redwine_month", "champagne_week", "champagne_month", 
               "beer_week", "beer_month", "spirits_week", "spirits_month", 
               "wine_week", "wine_month")],
  na.rm = TRUE
)
# use this method to avoid ambiguity in calculation

ukb_data |>
  filter(!is.na(total_alcohol) & total_alcohol < 200) |>
  ggplot(aes(x = total_alcohol)) +
  geom_density(fill = "darkred") +
  theme_minimal() +
  labs(
    title = "Total Weekly Alcohol Intake Plot",
    x = "Total Alcohol Intake",
    y = "Density"
  )
```

![](pre_cleaning_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Blood lipids

``` r
ukb_data = ukb_data |>
  rename(
    tc = n_23400_0_0,
    hdl = n_23406_0_0
  ) |>
  mutate(non_hdl = tc * 88.6 - hdl * 38.6)
```

## Blood glucose

``` r
ukb_data = ukb_data |>
  rename(
    tg_0 = n_30740_0_0,
    hba1c = n_30750_0_0
  ) |>
  mutate(tg = tg_0 * 18)
```

## Blood pressure

We calculate a blood pressure score based on Systolic Blood Pressure and
Diastolic Blood Pressure as below:

``` r
ukb_data = ukb_data |>
  rename(
    dbp = n_4079_0_0,
    sbp = n_4080_0_0
  ) |>
  mutate(
    bp_cat = case_when(
      sbp < 120 & dbp < 80 ~ 0,
      (sbp >= 120 & sbp < 130) & dbp < 80 ~ 1,
      (sbp >= 130 & sbp < 140) | (dbp >= 80 & dbp < 90) ~ 2,
      (sbp >= 140 & sbp < 160) | (dbp >= 90 & dbp < 100) ~ 3,
      (sbp >= 160 & sbp < 180) | (dbp >= 100 & dbp < 110) ~ 4,
      sbp >= 180 | dbp >= 110 ~ 5,
      TRUE ~ NA_real_
    )
  )


ukb_data |>
  filter(!is.na(bp_cat)) |>
  ggplot(aes(x = bp_cat)) +
  geom_histogram(fill = "yellow3") +
  theme_minimal() +
  labs(
    title = "Blood Pressure Score",
    x = "Score",
    y = "Counts"
  )
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](pre_cleaning_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

# Save the dataset in .dta form for quicker I/O

``` r
haven::write_dta(ukb_data, "./data/inf_0.dta")
```
