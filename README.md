# Guatemala pilot phase mothers' indirect exposure data assessment by Beacon and direct exposure
- You need to use R/RStudio to open data file and scripts.
- You need to install R packages loaded in the scripts, see each script for required packages.

# Description of data file in beacon_JESEE_paper_data.RData
- Beacon_data3: contains the Beacon instruments' Received Signal Strength Indicator (RSSI), indicating the location of mothers in each microenvironment.
- Beacon_inventory: contains the Beacon emitter inventories
- ECM_all: contains 5-minutes interval ECM measured PM2.5 dath
- Mother_exposure_all: contains 5-minute interval 5-minute interval exposure data for mothers, both indirect exposure, direct exposure and mother's location
- PEO_daily3: contains aggregated 24-hour interval exposure data for mothers and time spent in each microenvironment.
- Children_PM3: contains aggregated 24-hour interval exposure data for children and time spent in each microenvironment.
- PM_summary2: contains statistical summary for microenvironmental ECM monitors, summarized from ECM_all dataset.

# Description of scripts:
- mother_indirect.Rmd: runs the mother's microenvironmental data visualizations.  Alternatively, you can assess online Shinyapp https://hapin-trial.shinyapps.io/mother_microenvironmental_data/, which runs the script online showing the plots

- Scripts_for_figures_tables.R: generates the tables and figures for JESSE paper https://doi.org/10.1038/s41370-019-0172-z .

