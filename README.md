# File Structure for Teacher Reports 

### 1. Scripts 
Located in 1920_ia

Run first to load data

* **ia_script_18_19_primary.R** - includes both Primary and Middle school data

* **ia_dl_script_19_20.R**

Files the scripts produce are saved to 1920_ia/data folder and are called *1920_IA_1.Rda* and *1920_dl_IA_1.Rda*

### 2. Helpers
Located in 1920_ia/lib

Do not need to be run on own

* **17_18_ia_dl_helpers.R**

* **18_19_primary_helpers.R**

### 3. Markdowns
Located in 1920_ia

Contains markdowns that create teacher reports. Makes use of helper files

* **SY 18-19 IA 03-8 ELA Report.Rmd**
- ELA separate because contains a Writing breakdown table

* **SY 18-19 IA 03-8 Math Report.Rmd**
- includes both Math and Science

* **SY 19-20 DL IA Report.Rmd**

### 4. Render Reports
Located in 1920_ia/src

* **Render Reports**

- functions to render one Markdown per school, subject, and grade level

- includes code to create google folders and drop reports into them

# File Structure for Dashboards

### 1. Scripts
Located in 1920_ia/src

* **IA_Dashboard_Script_1920.r**

File the script produces is saved to 1920_ia/data and is called *IA_1_db_cont.Rda*

### 2. Dashboard
Located in 1920_ia/reports/IA Dashboard

* **1920 IA 1 Dashboard.Rmd**

### 3. Anaysis
Located in 1920_ia/reports/IA Analysis

* **IA 1 Analysis Middle 1920**
* **IA 1 Analysis Primary 1920**



