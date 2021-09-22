Title: Replication File for “Conceptualizing and Measuring Autocratization Episodes”
Journal: Swiss Political Science Review
Author: Lars Pelke, Aurel Croissant 
Correspondence: Lars Pelke, Heidelberg University (lars.pelke@ipw.uni-heidelberg.de)
Date: January 10th, 2021


Required Software: R and RStudio (3.6.1 or Above). 
Additional required software packages and libraries are specified in the replication code.

Overview: These files replicate all analyses in Lars Pelke and Aurel Croissant, “Conceptualizing and Measuring Autocratization Episodes” Swiss Political Science Review
To replicate all analyses and figures, download all data files to a folder entitled “~/data”, create a second folder entitled “~/outputs” in the same directory, and run all code files in numerical order. In addition, download styles.css to the main folder to replicate the Web Application

%%%%%%%%%%%%%% Replication Code %%%%%%%%%%%%%%

01_data_wrangling
Replicates the data wrangling process and figures in the research note. In addition, it compiles data need for subsequent analysis and you can store the autocraitzation periods dataset. 

02_data_prepartion_shiny_web_app
Replicates the data prepartion for the Shiny Web Application (Online Interactive Supplementary Appendix)

03_WebApp
Replicates the Shiny Web Applicatio. The Shiny Web Application runs will with Google Chrome Browser

%%%%%%%%%%%%%% Data Files (relative path "~/data") %%%%%%%%%%%%%%

Replication data for research note:
-vdem_10 folder: Varieties of Democracy Version 10, Coppegde et al. (includes Polity IV and Freedom House data that is necessary for replication)
-subdatasets are created when using 01_data_wrangling 

%%%%%%%%%%%%%% Output Files (relative path "~/outputs") %%%%%%%%%%%%%%

Store outputs, such as Figures
