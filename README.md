# NEY-Inequalities-Methodologies
This is a repository which outlines methodologies used by the NHSE North East and Yorkshire Analytics Team for analysing healthcare metrics by deprivation. All of the underlying R code used for calculations and visualisations is available in the 'Functions.R' file.

The work builds on recommendations from the NHSE Healthcare Inequalities Improvement Programme and their 'How to Guide' for constructing a health inequalities metric, which is available on their [FutureNHS page](https://future.nhs.uk/InequalitiesImprovement/view?objectId=164231013).

# Folder Structure
The DataWrangling.R file reads in data and performs any necessary data wrangling. Certain data are fed in from the Excel files in the 'DataFiles' folder. These are all from data publicly available as part of the CVDPREVENT Audit. 

The Functions.R file contains all of the functions used to produce visualisations in the final output or perform 'new' Slope Index of Inequality calculations. These are set up to be applied to any dataset, with comments providing guidance on the different column types required and the format the data needs to be in. Note that the visualisations are specific to North East and Yorkshire ICBs, but could be modified to produce outputs for other regions.

The QMD file is then used to create the final output. To reproduce the output, it will be necessary to also read in the shape files and images available as part of this repo. The output from the QMD file is also saved in this folder.

# 'New' Slope Index of Inequality Function
The 'new' Slope Index of Inequality (SII) function can be used to calculate 'new' SII values with accompanying simulated confidence intervals for any dataset broken down by deprivation. It is set up to calculate a value for a single geography/metric/time period, but can be used to calculate values for multiple combinations via a 'loop'. Worked examples of how to do this can be found in the DataWrangling file.

# Contributing
To contribute to this repo, please create a new branch and make a pull request with any changes in to the 'Dev' branch. 

# Contact
For any questions about the contents of the repo, please contact will.manners@nhs.net.
