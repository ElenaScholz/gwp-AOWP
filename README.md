# DLR-GWP-PCA

# Folder Structure

project_root/  
├── scripts/  
├──── 00_preprocessing  
├──── 01_prepareClusterAnalysis  
├──── 02_Clusteranalysis  
├──── 03_Visualizatitons  
├──── 04_InformationForPaper
├── utils/  
├── data/  
├──── HERE WE CAN STORE A SUBSET MAYBE ? OR WE UPLOAD A LINK TO DOWNLOAD THE DATA  
└── docs/  

# Environment setup (renv)

This project uses [renv](https://rstudio.github.io/renv/) to manage R package dependencies and ensure a reproducible environment across all collaborators.

## First-time setup (after cloning)

Open the project in RStudio (or start R in the project root), then run:

```r
renv::restore()
```

This will automatically install all required packages at the correct versions as specified in `renv.lock`. You only need to do this once per machine.

> **Note:** If renv is not yet installed on your system, install it first:
> ```r
> install.packages("renv")
> ```

---

## Adding or updating packages

If you install a new package or update an existing one, record the changes to the lockfile before committing:

```r
renv::snapshot()
```

Then commit the updated lockfile:

```bash
git add renv.lock
git commit -m "chore: update renv lockfile"
```

---

## Checking your environment

To verify that your local library matches the lockfile:

```r
renv::status()
```

A healthy environment will report **no issues**. If there are discrepancies, run `renv::restore()` to fix missing packages or `renv::snapshot()` to update the lockfile to your current state.

---

## Troubleshooting

| Problem | Solution |
|---|---|
| Packages missing after cloning | `renv::restore()` |
| Lockfile out of sync with library | `renv::snapshot()` |
| renv version mismatch warning | `renv::record("renv@<your_version>")` |
| Project not activated | Select option 1 when prompted, or run `renv::activate()` |

For further help, see the [renv documentation](https://rstudio.github.io/renv/articles/renv.html).


# Preprocessing
Preprocessing transforms the raw timeseries data into ready to use time series for further analyses.
## Information for all three Scripts
1. To start the preprocessing use the three scripts given within the 00_prerpocessing folder. 
2. Use the scripts in the right order (1-3)
3. check the paths given in the configuration section at the top of the code
4. make sure to name the folder with the GWP timeseries like this: 01_timeseries_8247 - or change it consistently through out the entire analyses
5. change the absolute paths for output folders to match your directoy

## 00-1_RemoveDuplicates.R
This Scripts checks if a lake is located on two MODIS tiles - If yes it removes the duplicated time series and recalculates the areas for each date. 

The results are stored in the folder *03_timeseries_8247_cor*

## 00-2_CalculateMissingDate.R
This Script checks if there are dates missing within the time series and fills them with data. As there are only single days missing (31-12) of some years linear interpolation is used. 
Inputfolder: 03_timeseries_8247_cor
Outputfolder: 04_timeseries_8247_allDates

## 00-3_rm2902.R
This Script removes the 29.02 from all leap years. 
Inputfolder: 04_timeseries_8247_allDates
Outputfolder: 05_timeseries_8247_rm2902
