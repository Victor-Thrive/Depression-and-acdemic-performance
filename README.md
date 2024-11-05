## Depression and Academic performance Among Nigerian University Students in the South West Region

### Introduction

This project focuses on predicting student's academic performance from **Interpersonal, Intrapersonal, and Environmental stress factors.** Additionally, demographic information such as **age, academic level, department,** and **college** were collected to provide further context and insights into the mental health of students.

### Objectives

-   Identify key stress factors influencing mental health among undergradauate students.

-   Asses the effect of these factors on academic performance.

-   Predict student academic performance.

### Data Collection

Data were collected via an online survey [here](https://forms.gle/hyTxqo38yTKB7YpE7), designed to capture :

-   Students demographic information such as age, level, department, college, sex, religion etc

-   30 potentially stressful situations that may occur in the life of a student which can trigger depression

-   Other questions relating to interpersonal, Interpersonal and enviromental factors.

### Data Cleaning 

Imported survey data, was carefully cleaned to ensure accuracy and consistency this involve renaming columns, handling missing data and standardizing formats across demographic details like age, level and department. You can view the detailed data cleaning steps in the [here](https://github.com/Victor-Thrive/Depression-and-acdemic-performance/blob/main/source/01-data-cleaning.R)

### Data Pre-processing

Exploratory factor analysis was used to identify key patterns in the data, which allowed me to group related survey items under three main factors: Interpersonal Stress (stress from relationship and social interactions), Intrapersonal Stress (internal stressors like self-esteem and anxiety), and environmental Stress ( electricity blackout, queues, academic workload, transportation etc) EFA helped verify the structure of these factors and ensured that each item aligned with the correct category. You can find the detailed EFA steps [here](https://github.com/Victor-Thrive/Depression-and-acdemic-performance/blob/main/source/02-EFA.R)

### Data Analysis
