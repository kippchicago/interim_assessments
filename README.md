File Structure for Teacher Reports
==================================

### 1. Scripts

Located in 1920\_ia

Run first to load data

-   **ia\_script\_18\_19\_primary.R** - includes both Primary and Middle
    school data

-   **ia\_dl\_script\_19\_20.R**

Files the scripts produce are saved to 1920\_ia/data folder and are
called *1920\_IA\_1.Rda* and *1920\_dl\_IA\_1.Rda*

### 2. Helpers

Located in 1920\_ia/lib

Do not need to be run on own

-   **17\_18\_ia\_dl\_helpers.R**

-   **18\_19\_primary\_helpers.R**

### 3. Markdowns

Located in 1920\_ia

Contains markdowns that create teacher reports. Makes use of helper
files

-   **SY 18-19 IA 03-8 ELA Report.Rmd**
-   ELA separate because contains a Writing breakdown table

-   **SY 18-19 IA 03-8 Math Report.Rmd**
-   includes both Math and Science

-   **SY 19-20 DL IA Report.Rmd**

### 4. Render Reports

Located in 1920\_ia/src

-   **Render Reports**

-   functions to render one Markdown per school, subject, and grade
    level

-   includes code to create google folders and drop reports into them

File Structure for Dashboards
=============================

### 1. Scripts

Located in 1920\_ia/src

-   **IA\_Dashboard\_Script\_1920.r**

File the script produces is saved to 1920\_ia/data and is called
*IA\_1\_db\_cont.Rda*

### 2. Dashboard

Located in 1920\_ia/reports/IA Dashboard

-   **1920 IA 1 Dashboard.Rmd**

### 3. Anaysis

Located in 1920\_ia/reports/IA Analysis

-   **IA 1 Analysis Middle 1920**
-   **IA 1 Analysis Primary 1920**
