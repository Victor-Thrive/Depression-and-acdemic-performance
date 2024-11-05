# load library
library(tidyverse)
library(googlesheets4)
# import data
sheet_url <- "https://docs.google.com/spreadsheets/d/1UMLpOJZ738F8Rs1oYgfH_Uvg4zflAv8BazCx-zGUQvE/edit?gid=828760894#gid=828760894"
data <- read_sheet(sheet_url)
#remove 2 rows
df <- data[-c(1,2),]

#renaming columns
df <- df |> rename(
  "email" = `Enter your email address if you would like to get the web link for this research work`,
  "age" =  `Pleas select your age`,
  "gender" = `Please select your gender` ,
  "college" = `Please select your college`,
  "level" = `Please select your level` ,
  "G.P.A" = `Please select the range of your current G.P.A` ,
  "C.G.P" = `Please select the range of your current C.G.P.A` ,
  "department" = `Please select your department`,
  "sponsor" = `Please select your sponsor` ,
  "religion" = `Please select your religion` ,
  "status" = `Please select your marital status`,
  "S1" = `Have you lost of a close family member in the past twelve month?`,
  "E1" = `The death of a close family member is likely to affect your academic result`,
  "S2" = `Transportation problem...103`,
  "E2" = `Having a transportation problem is likely to affect your academic result`,
  "S3" = `Have you lost of a close friend in the past twelve month?` ,
  "E3" = `The death of a close friend is likely to affect your academic result` ,
  "S4" = `Is your parent divorced?...107` ,
  "E4" = `Divorce between one's parent is likely to affect your academic result...50`,
  "S5" = `Have you been held in police custody?...51`,
  "E5" = `Staying in police custody is likely to affect your academic result...108`,
  "S6" = `Do you have any personal illness or injury?...53`,
  "E6" = `Having personal injury or illness is likely to affect academic result` ,
  "S7" = `Are you married?`,
  "E7" = `Getting married is likely to affect your academic performance`,
  "S8" = `Did you fail any Carry (carry over)`,
  "E8" = `Having carry over is likely to affect your academic result`,
  "S9" = `Major change in the health of a family member?` ,
  "E9" = `Deteriorating in the health of a family member is likely to affect your academic result` ,
  "S10" = `Sex problems (abortion, pregnancy, miscarriage e.t.c)`,
  "E10" = `Experiencing sex problem is likely to affect academic result`,
  "S11" = `Fight with close friend` ,
  "E11" = `Fight with close friend is likely to affect your academic result`,
  "S12" = `Low financial status`,
  "E12" = `Inadequate finance is likely to affect your academic result`,
  "S13" = `Change of course` ,
  "E13" = `Not studying your first-choice course is likely to affect your academic result`,
  "S14" = `Trouble with parent...69`,
  "E14" = `Having trouble with parent is likely to affect your academic result`,
  "S15" = `Fight with boyfriend/girlfriend...71` ,
  "E15" = `Fight with boyfriend/girlfriend is likely to affect your academic result`,
  "S16" = `Compulsory holiday (ASUU/NASU strike)` ,
  "E16" = `Strike is likely to affect your academic result` ,
  "S17" = `Messy living condition ( bad hostel )...77` ,
  "E17" = `Living in a bad hostel is likely to affect your academic result` ,
  "S18" = `Trouble working with new people`,
  "E18" = `Having trouble working with new people is likely to affect your academic result`,
  "S19" = `Trouble with lecturer...81`,
  "E19" = `Having trouble with lecturer is likely to affect your academic result` ,
  "S20" = `Water shortage...83`,
  "E20" = `Having water shortage is likely to affect your academic performance`,
  "S21" = `Change in religious believe` ,
  "E21" = `Having a change in religious believe is likely to affect your academic result`,
  "S22" = `Major change in religious activities (a lot more or a lot less)`,
  "E22" = `Having a major change in religious activities is likely to affect your academic result`,
  "S23" = `Major change in sleeping habit ( a lot more or a lot less)` ,
  "E23" = `Having a major change in sleeping habit is likely to affect your academic performance`,
  "S24" = `Having a major change in eating habit ( a lot more or a lot less)` ,
  "E24" = `Having a major change in eating habit is likely to affect your academic performance` ,
  "S25" = `Major change in social activities (party, club, visitation e.t.c)` ,
  "E25" = `Having a major change in social activities is likely to affect your academic result` ,
  "S26" = `Waited in long queues`,
  "E26" =  `Waiting in a long queue is likely to affect your academic result`,
  "S27" = `Room mate conflict`,
  "E27" = `Having conflicts with room mate is likely to affect academic performance`,
  "S28" = `Missed too many classes`,
  "E28" = `Missing too many classes is likely to affect your academic result`,
  "S29" = `Computer problems (mobile phone, laptops e.t.c)` ,
  "E29" = `Having a computer problem is likely to affect your academic result` ,
  "S30" = `Electricity Shortage (blackout)`,
  "E30" = `Electricity shortage is likely to affect your academic result`
)
# subset main data
survey_data <- df[,c("age","gender","college","department","level","religion",
                     "G.P.A","C.G.P","status","sponsor","email",
                     "S1","S2","S3","S4","S5","S6","S7","S8","S9",
                     "S10","S11","S12","S13","S14","S15","S16","S17",
                     "S18","S19","S20","S21","S22","S23","S24","S25",
                     "S26","S27","S28","S29","S30","E1","E2","E3","E4",
                     "E5","E6","E7","E8","E9","E10","E11","E12","E13","E14",
                     "E15","E16","E17","E18","E19","E20","E21","E22","E23",
                     "E24","E25","E26","E27","E28","E29","E30")]

# reverse code likert item
survey_data[, c(42:71)] = 6 - survey_data[,c(42:71)]

# converting to factors
survey_data$age <- factor(survey_data$age, 
                          levels = unique(c("under 18",
                                            "18 - 23",
                                            "24 - 29",
                                            "30 - 35",
                                            "36 - 41",
                                            "42 - 46",
                                            "above 46")))

survey_data$level <-factor(survey_data$level, 
                           levels = unique(c(100,200,300,400,500,600)))

survey_data$gender <-factor(survey_data$gender, 
                            levels = unique(c("Male","Female")))

survey_data$religion <-factor(survey_data$religion, 
                              levels = unique(c("Christianity","Islamic","Other")))

survey_data$G.P.A <-factor(survey_data$G.P.A, 
                           levels = unique(c("4.5 - 5.0",
                                             "3.5 - 4.4",
                                             "2.5 - 3.4",
                                             "1.5- 2.4",
                                             "1.4 - below")))

survey_data$C.G.P <-factor(survey_data$C.G.P, 
                           levels = unique(c("4.5- 5.0",
                                             "3.5 - 4.4",
                                             "2.5 - 3.4",
                                             "1.5- 2.4",
                                             "1.4 - below")))


survey_data$status <-factor(survey_data$status, 
                            levels = unique(c("Single",
                                              "Married",
                                              "Divorced",
                                              "Seperated",
                                              "Widowed",
                                              "Cohabiting")))

survey_data$sponsor <-factor(survey_data$sponsor, 
                             levels = unique(c("Parent","Self","Guardian","Other")))

survey_data$college <-factor(survey_data$college, 
                             levels = unique(c("COLPHYS","COLBIOS","COLERM",
                                               "COLPHEC","COLPLANT","COLENG",
                                               "COLANIM","COLMAS","COLAMRUD","COLVET")))

survey_data$department <-factor(survey_data$department, 
                                levels = unique(c("AGAD","AEFM","AERD","ABG",
                                                  "ANN","APH","PRM","ANP","BCH",
                                                  "MCB","PAB","PAZ","AGE","CVE",
                                                  "ELE","MCE","MTE","AQFM","EMT",
                                                  "FWM","WMA","FST","HTM","HSM",
                                                  "NTD","ETS","CHM","PHS","CSC",
                                                  "MTS","STS","CPT","HRT","PBST",
                                                  "SSLM","VET")))

# working on gender column
table(survey_data$gender)
survey_data$gender[which(is.na(survey_data$gender))] <- "Female"
table(survey_data$gender)
sum(is.na(survey_data$college)) # check if there is missing value

# working on college column
table(survey_data$college)
survey_data$college[which(is.na(survey_data$college))] <- "COLPHEC"
table(survey_data$college)
sum(is.na(survey_data$college)) # check if there is missing value

# Create a named vector for the scoring system
score_mapping <- setNames(c(
  100, 25, 73, 65, 63, 53, 50, 47, 45, 44,
  40, 39, 39, 39, 38, 35, 31, 30, 30, 29,
  29, 29, 28, 28, 28, 25, 25, 25, 25, 35
), paste0("S", 1:30))

# convert E1:E30 to ordered factors
 new_data <- survey_data %>%
    mutate(
      across(
        .cols = matches("^E([1-9]|[12][0-9]|30)$"),  # Match columns E1 to E30
        .fns = ~ factor(., levels = c(1, 2, 3, 4, 5), ordered = TRUE)  # Convert to ordered factors
      )
    ) |>
    mutate(
      across(
        .cols = matches("^S([1-9]|[12][0-9]|30)$"),
        .fns = function(x) factor(x, levels = c("YES","NO"))
      )
    ) |>
    mutate(across(all_of(names(score_mapping)), 
                  ~ if_else(. == "YES", score_mapping[names(score_mapping) == cur_column()], 0), 
                  .names = "{.col}")) |>
    mutate(stress_score = rowSums(across(matches("^S([1-9]|[12][0-9]|30)$")), na.rm = TRUE)) |>
    mutate(stress_level = case_when(
      stress_score < 150 ~ "Very little",
      stress_score >= 150 & stress_score < 200 ~ "Mild",
      stress_score >= 200 & stress_score < 250 ~ "Moderate",
      stress_score >= 250 & stress_score < 300 ~ "Serious",
      TRUE ~ "Major"  # Catch-all for stress_score >= 300
    ))
  

# write new data to file
#write_rds(survey,"new_my_survey.rds")
write.csv(new_data,"data/survey.csv")
