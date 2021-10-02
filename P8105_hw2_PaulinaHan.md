P8105\_hw2\_PaulinaHan
================
Paulina Han
2021/10/1

# Problem 1

``` r
trash_df = read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = "Mr. Trash Wheel",range = cell_cols("A:N")) #rows?

#tidying trash data set
trash_tidy = 
  trash_df %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>% 
  rename(weight = weight_tons ,volume = volume_cubic_yards) %>% 
  mutate(sports_balls = round(sports_balls,0))

#2019 precipitation
precipitation19_df = read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",sheet = "2019 Precipitation", range = "A2:B14")

#tidy 2019 precipitation data
tidy_2019_df =
  precipitation19_df %>% 
  janitor::clean_names() %>% 
  drop_na(total) %>% 
  mutate(year = 2019)
  
#2018 precipitation
precipitation18_df = read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",sheet = "2018 Precipitation", range = "A2:B14")

#tidy 2018 precipitation data
tidy_2018_df =
  precipitation19_df %>% 
  janitor::clean_names() %>% 
  drop_na(total) %>% 
  mutate(year = 2018)  

#combine precipitation data set
preciptation = 
  bind_rows(tidy_2018_df,tidy_2019_df) %>% 
  mutate(month=month.name)

#description 
#data set 1
ncol_trash_tidy = ncol(trash_tidy)
nrow_trash_tidy = nrow(trash_tidy)

mean_weight = mean(pull(trash_tidy,weight))
max_weight = max(pull(trash_tidy,weight))
min_weight = min(pull(trash_tidy,weight))

mean_volume = mean(pull(trash_tidy,volume))
max_volume = max(pull(trash_tidy,volume))
min_volume = min(pull(trash_tidy,volume))
#data set 2
ncol_preciptation = ncol(preciptation)
nrow_preciptation = nrow(preciptation)

max_rain = max(pull(preciptation,total))

#median of sports ball in 2017
ball =
  trash_tidy %>% 
  filter(year==2017) %>% 
  pull(sports_balls) %>% 
  median()
```

The tidied Mr.Wheel Trash data set has 14 columns and 344 rows. The mean
weight of trash the dumpster took is 3.262936 and the maximum weight of
trash the dumpster took is 5.62, the minimum weight of trash the
dumpster took is 0.96. The mean volume of trash the dumpster took is
15.5436047, the maximum volume of trash the dumpster took is 20 and the
minimum volume of trash the dumpster took is 7.

The precipitation data set including 2018 and 2019 has 3 columns and 12
rows. 2018 March had the most preciptation in 2018 and 2019 which is
4.47.

the median number of sports balls in a dumpster in 2017 is 8.
