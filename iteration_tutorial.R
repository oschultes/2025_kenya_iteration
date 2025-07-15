
######################
# EFGH Phase C Data Core
# Kenya In-Person Orientation
# August 19, 2025
# Author: Olivia Schultes
######################


# setup
pacman::p_load(tidyverse, lubridate, epiR)





#################
# FOR-LOOPS


##### Structure & Basics

# basic for-loop structure 
# for(value in sequence) {
#   statement
# }


# printing numbers in a sequence
for(x in 1:10) {
  print(x)
}


# creating an object that contains numbers in a sequence, then printing that object
vec <- NULL
seq <- 1:10

for(i in seq) {
  vec[i] <- i
}
print(vec)


# repeating above exercise, except now we don't specify vec before running the for loop
# why does this not work?
rm(vec)
seq <- 1:10

for(i in seq) {
  vec[i] <- i
}
print(vec)


# what if we try creating the object vec within the for loop?
# why does this still not work?
seq <- 1:10

for(i in seq) {
  vec <- 0
  vec[i] <- i
}
print(vec)



##### Example 1: reading files into R

# create a list of files in your working directory 
# (this assumes your working directory is 2025_kenya_iteration)
rds_list <- list.files(path = , pattern = "*.rds", full.names = TRUE)

# create a list of names you would like the imported files to have
name_list <- sub("./", "", rds_list)
name_list <- sub(".rds", "", name_list)

# use a for loop to read in the files and assign them the appropriate names
for (i in 1:length(rds_list)) {  
  assign(name_list[i], readRDS(rds_list[i]))
}






###############
# FUNCTIONS


##### Structure & Basics

# basic function structure 
# function_name <- function(argument1, …) {
#   function
# }


# function with no arguments
func_noarg <- function() {
  print("Hello world")
}

func_noarg()


# function with one argument
func_onearg <- function(x) {
  print(paste0("Hello ", x))
}

func_onearg("Olivia")


# function with many arguments
func_manyarg <- function(x, y) {
 x^2 + y 
}

func_manyarg(x=3, y=4)
func_manyarg(3, 4)
func_manyarg(4, 3)
func_manyarg(y=4, x=3)


# function with default arguments
func_defaultarg <- function(x=3, y=4) {
  x^2 + y 
}

func_defaultarg()
func_defaultarg(2)
func_defaultarg(y=5)
func_defaultarg(2, 5)




##### Example 1: producing plots iteratively

# in this example, we will create a function that can produce a simple plot 
# using data from all sites OR from only one site at a time

produce_figure = function(site = "all") {
  
  if(site == "all") {
    df = scrambled_data
  } else {
    df = scrambled_data |> filter(enroll_site=={{site}})
  }
  
  ggplot(data = df, aes(x=enr_age_months, y=episode_duration_length)) +    
    geom_point(alpha = 0.2, shape = 19)     
}

produce_figure()
produce_figure("Bangladesh")
produce_figure("Mali")




##### Example 2: running a function through nested for-loops

# in this example, we are interested in calculating measures of diagnostic accuracy such as sensitivity and specificity
# we are interested in how well different severity definitions predict a child's risk of hospitalization after the diarrhea episode

# furthermore, we would like to look at how well the severity definitions perform overall, by pathogen (shigella vs all pathogens), 
# and by treatment (any antibiotics vs none)


# prepare the data

scrambled_data = scrambled_data |>
  mutate(mvs_factor = factor(mvs_classification),
         mvs_or_dysentery_factor = factor(mvs_or_dysentery),
         gems_msd_factor = factor(gems_msd),
         clark_factor = factor(clark_classification), 
         hosp_factor = factor(hosp_after_episode))

# first, we'll create a function to calculate measures of diagnostic accuracy
# this includes sensitivity, specificity, diagnostic accuracy, positive predictive value, and negative predictive value
calculate_diagnostic_accuracy <- function(d, severity_definition, denominator, antibiotics) {
  
  # calculate measures of diagnostic accuracy
  two_by_two = d %>% 
    group_by(!!!syms(severity_definition), hosp_factor) %>% 
    summarise(n = n(), .groups = "keep")
  test = epi.tests(two_by_two, method = "exact", digits = 2, conf.level = 0.95)
  
  # format results
  result = test$detail |>
    filter(statistic=="se" | statistic=="sp" | statistic=="pv.pos" | 
             statistic=="pv.neg" | statistic=="diag.ac") %>%
    select(statistic, est) |>
    rename(value = est, measure = statistic) |>
    mutate(value = value*100) |>
    mutate(severity_definition = {{severity_definition}},
           denom = {{denominator}},
           antibiotics = {{antibiotics}})
  
  return(result)
}

# test to make sure this function works
calculate_diagnostic_accuracy(d = scrambled_data, 
                              severity_definition = "mvs_factor", 
                              denominator = "all diarrhea",
                              antibiotics = "overall")

# next, we will specify the sub-groups we are interested in
definitions_sensitivity = c("mvs_factor", "mvs_or_dysentery_factor", 
                            "clark_factor", "gems_msd_factor")
denom = c("all diarrhea", "shigella")
antibiotics = c("overall", "antibiotics", "no antibiotics")

# and create an empty data frame to store the results
diagnostic_accuracy = data.frame()

# finally, we will run the function through three nested for-loops, 
# calculating results for all combinations of sub-groups
for(i in definitions_sensitivity) {
  for(j in denom) {
    for(k in antibiotics) {
      
      # Denominator: all diarrhea or Shigella
      if (j=="all diarrhea") {
        d = scrambled_data
      } else if (j=="shigella") {
        d = scrambled_data %>% filter(positive_tac_or_culture==1)
      } else 
        ("Error: incorrect denominator argument")
      
      # Antibiotic strata: overall, antibiotics, or no antibiotics
      if (k=="overall") {d = d
      } else if (k=="antibiotics") {
        d = d %>% filter(any_abx==1)
      } else if (k=="no antibiotics") {
        d = d %>% filter(any_abx==0)
      } else
        ("Error: incorrect antibiotics argument")
      
      diagnostic_accuracy = rbind(diagnostic_accuracy, calculate_diagnostic_accuracy(d, i, j, k))
      
    }
  }
}





##### Example 3: nested functions

####
###
##
# ask sean for scrambled incidence data





#################
# MAP FROM PURRR PACKAGE


# Structure & basics
# map(.x, .f)


# applying a paste function over each item in a character vector
map_chr(c("Red", "Green", "Yellow"), ~ paste0(.x, " apple"))

# replacing words in a phrase
map_chr(c("Dogs are excellent", "Cats are excellent"), \(value) str_replace(value, "excellent", "great"))

# adding a value to a numeric vector
c(4, 19, 8) |>
  map_dbl(~ . + 5)


# adding a value to each value in a dataframe
data.frame(a = c(1, 2, 3), b = c(2, 5, 4), c = c(3, 2, 5)) |>
  map_df( ~ .x + 5)

# replacing all values in a dataframe with NA
data.frame(a = c(1, 99, 3), b = c(2, 5, 4), c = c(99, 2, 5)) |>
  map_df( ~ ifelse(.x==99, NA, .x))




##### Example 1: filter to particular date range across all datasets

# in this example, we would like to generate a report using data collected through a certain date 
# this is to avoid reporting on data that hasn't been queried yet
# therefore, we need to reproduce each dataset so that it only contains data from the desired date range

# set date for report data cut-off (using Jan 1, 2024 as an example)
data_date = as.Date("2024-01-01")

# make list of pids within report date range
included_pids = scrambled_data$pid[scrambled_data$enroll_date<data_date]

# make a list of all dataframes
dataframes = list(scrambled_data = scrambled_data,
                  scrambled_preenrollment = scrambled_preenrollment)

# for each dataframe in list, filter by pids in specified date range
report_date_data = dataframes |>
  map(\(df) df |> filter(pid %in% included_pids))

# rewrite dataframes in global environment
list2env(report_date_data, envir = .GlobalEnv)




##### Example 2: fix inconsistencies across datasets for merging


# in this example, we would like to combine the rows of three datasets so they are stacked on top of each other
# scrambled_prescreening, scrambled_screening, scrambled_preenrollment

# if we try to combine the first two dataframes, we get an error because there are a different number of columns present
rbind(scrambled_prescreening, scrambled_screening)

# when we remove the extra column in the scrambled_screening dataset, two of the columns are 
# not in the format we would like (serial_id and eligible are character instead of numeric)
rbind(scrambled_prescreening, scrambled_screening |> select(-scr_diar_when))

# it will take some processing to get these dataframes to align in the way we want them to
# to make this process more efficient, we can use map()

# first let's list the dataframes of interest
screening_list = list(scrambled_prescreening = scrambled_prescreening,
                      scrambled_screening = scrambled_screening,
                      scrambled_preenrollment = scrambled_preenrollment)

# next, we need to manipulate the datasets so that rbind works properly 
# editing columns across all the datasets is more efficient using the map function
# then we use the function list_rbind to rbind the objects of the list together
# and label each observation by the original form it came from
combined_screening_forms = screening_list |>
  map(\(df) df |> mutate(across(everything(), ~ ifelse(.x=="", NA, .x)))) |>
  map(\(df) df |> mutate(across(c(serial_id, eligible), ~ as.numeric(.x)))) |>
  map(\(df) df |> mutate(across(c(date), ~ as.Date(.x)))) |>
  map(\(df) df |> select(any_of(c("serial_id", "date", "eligible")))) |>
  list_rbind(names_to = "form") |>
  mutate(form = str_replace(form, "scrambled_", ""))




#################
# PIVOTING INSTEAD OF USING ITERATION


scrambled_medication |>
  pivot_longer(!pid, names_to = c("med", ".value"), names_sep = "_") %>%
  mutate(dosknown_disc = ifelse(dosknown==1 & !is.na(dosknown) & is.na(dos), 1,
                                ifelse(dosknown==1 & !is.na(dosknown) & dos==99 & !is.na(dos), 1, 0))) %>%
  mutate(query = ifelse(dosknown_disc==1, "Medication dose is marked as known but dose amount is blank or marked as 99", NA))
  
  






#################
# APPLY FAMILY


##### Structure & Basics

# basic apply structure
# apply(X, MARGIN, FUN)


# sum rows of a dataframe
apply(scrambled_data[c("water_drink_level", "sanitation_level")], 1, sum)


# column means in a dataframe
apply(scrambled_data[c(5, 7)], 2, mean)



##### Example 1: matching values across a row

# checking to see if child sex matches across multiple enrollments
# in this dataset, each row is one unique child and each column is their recorded sex at each enrollment

# as part of data cleaning, we would like to verify that the sex of each child matches across enrollments
scrambled_repeat_sex$sex_match = ifelse(apply(scrambled_repeat_sex[c(2:5)], 
                                              1, 
                                              function(x) length(unique(na.omit(x)))) == 1, 1, 0)

# we can see that there was one unresolved sex query that is still present in the dataset
scrambled_repeat_sex$sex_match



##### Example 2: series of univariate regression models

# here we are running a series of univariate regression models on the outcome of 
# hospitalization after an episode of diarrhea

# first we set up a list of predictor variables and the measures we would like to pull out of the model results
pred = c("enr_age_months", "sex", "positive_tac_or_culture", "mvs_or_dysentery", "aav")
param_est = c("coef", "se(coef)", "z", "p", "cilow", "cihigh")


# next we use sapply and specify the univariate model within our custom function
hosp_model_results <- sapply(pred, function(x) {  
  
  form <- as.formula(paste0("hosp_after_episode~", x))
  model <- glm(form, data = scrambled_data, family = "binomial", na.action = na.omit)  
  hosp_out = data.frame(matrix(vector(), 6, 5, dimnames = list(param_est, pred)))  
  hosp_out[,x] <- c(summary(model)$coefficients[1,], exp(confint(model))[2,])
})


rownames(hosp_model_results) = param_est
hosp_model_results = as.data.frame(hosp_model_results)



