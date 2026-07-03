# quantium-data-analytics
This project contains my work for the Quantium Data Analytics Virtual Experience Program on Forage.

The program simulates real-world data analytics tasks performed by Quantium's analytics team.

## Task 1: Data Preparation and Customer Analytics

Conducted exploratory data analysis on retail transaction and customer data to identify purchasing behaviours and key customer segments driving chip sales.

1. Cleaned and prepared transaction data including date formatting, outlier removal and product name analysis.
2. Engineered new features including brand name and pack size from product descriptions.
3. Merged transaction and customer datasets for segment-level analysis.
4. Analysed total sales, customer counts, average units and average price per unit across customer segments.
5. Performed an independent t-test to determine statistical significance of price differences between mainstream and non-mainstream customers.
6. Conducted affinity analysis to identify brand and pack size preferences for the target customer segment.

## Task 2: Store Trials and Performance Testing

Ran a testing framework to see if a new store layout improved sales across three test locations (stores 77, 86 and 88) during a three month trial.

1. Calculated monthly totals for sales, unique customer counts, transaction frequencies and average prices for every store in the dataset.
2. Built a matching script that paired each test store with a highly similar test store. The selection looked at trend similarity (correlation) and sales scale (distance) during the pre-trial period.
3. Created a scaling factor based on past data so the control stores' sales matched the test stores' baseline before the trial started.
4. Used t-tests to evaluate percentage differences between the test and control stores. This proved whether the changes during the trial months were actual growth or just random variation.
5. Created trend graphs using ggplot that mapped out performance of the stores alongside a 95% confidence interval block to highlight when and where the trial layout created a significant lift.

## Task 3: In progress

## Tools & Technologies:
- R
- data.table
- ggplot
- readxl
