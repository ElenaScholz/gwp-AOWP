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

# Data Folder Structure
gwp-aowp-data
│   └── Input/  
│   │   ├── 00_coordinates_8247/
│   │   ├── *Folders for preprocessed gwp timeseries*
│   │   ├── 05_timeseries_8247_rm2902/
│   │   ├── gwp_HylaksWithClimateZone.gpkg  
│   └── CluDat/
│   │   ├── Points_Cont.txt
│   │   ├── Points_Climate.txt
│   │   ├── coordinates_longitude_only.txt
│   │   ├── coordinates_latitude_only.txt
│   │   ├── complete_name_vec.txt
│   │   ├── area_combination_withPrefix.txt
│   │   ├── area_combination_month_withPrefix.txt
│   │   ├── area_combination_month.txt
│   │   ├── area_combination.txt
│   │   ├── all_coordinates_complete.txt
│   │   └── cla_runs
│   │   │   ├── kmn_2.cla
│   │   │   ├── kmn_2.cnt
│   │   │   ├── kmn_3.cla
│   │   │   ├── kmn_3.cnt
│   │   │   ├── kmn_x.cla
│   │   │   └── kmn_x.cnt
│   └── CluDatOutput/  
│   │   ├── kmns_cla
│   │   │   ├── kmns_cla\kmns_cla_2.txt
│   │   │   ├── kmns_cla\kmns_cla_3.txt
│   │   │   ├── kmns_cla\kmns_cla_...txt
│   │   │   └── kmns_cla\kmns_cla_15.txt
│   │   └── plots
│   │   └── CLA_DAT_full.dat
│   │   └── CLA_DAT_full_withMetaData.dat
│   │   ├── data
│   │   │   ├── 03_AnnualClassResults.txt
│   │   │   ├── AnnualClassResults.txt
│   │   │   ├── KMN_LakeClass_results.txt
└   └   └   └── KMN_LakeClass_results_new.txt


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

# Preparation of Cluster Analysis
## 01-1_RestructureTimeSeries.R

Reads all SGV timeseries files from the input directory and produces four
aggregated output tables — a daily and a monthly version, each available
with and without metadata identifiers.

### What it does

| Step | Description |
|---|---|
| Load | Reads all `*_SGV-timeseries_allDates_rm2902.txt` files from the input folder |
| Validate | Warns if files differ in row count (= mismatched date ranges) |
| Daily | Reshapes each timeseries into a **year × day-of-year** matrix |
| Monthly | Averages each timeseries within months → **year × 12** matrix |
| Export | Writes four space-separated `.txt` files to `ClaDat/` |

### Input

- **Location:** `input/05_timeseries_8247_rm2902/`
- **Format:** Semicolon-separated `.txt` files with columns `Date` (YYYY-MM-DD) and `Area`
- **Naming convention:** `<prefix_id>_SGV-timeseries_allDates_rm2902.txt`

### Output

All files are written to `CluDat/`, space-separated, decimal point `.`.

| File | Contents |
|---|---|
| `area_combination.txt` | Daily matrix — values only, no headers |
| `area_combination_withPrefix.txt` | Daily matrix — with `prefix_id` and `Year` columns |
| `area_combination_month.txt` | Monthly average matrix — values only, no headers |
| `area_combination_month_withPrefix.txt` | Monthly average matrix — with `prefix_id` and `Year` columns |

## 01-4_add_ClimateZonesAndContinents.R

Prerequesite: gwp_HylaksWithClimateZone.gpkg - spatial join of Köppen-Geiger climate zones with all GWP coordinates

### Configuration

All paths and filename patterns are set at the top of the script in the `config` list.
Adjust `ROOT` and subfolder paths before running.

```r
config <- list(
  ROOT              = "your/folder/PCA-Analysis/",
  ts_subfolder      = "input/05_timeseries_8247_rm2902/",
  ...
)
```

### Dependencies

- `renv` (environment managed via `renv.lock` — run `renv::restore()` on first use)
- `scripts/utils/helperfunctions.R` — provides `read_data()`

### Notes

- The monthly aggregation uses the **arithmetic mean** across all days within each month (`na.rm = TRUE`)
- The `prefix_id` is extracted from each filename by stripping the suffix pattern — ensure filenames follow the expected convention
- If files have unequal row counts, a warning is printed but processing continues; check for date range mismatches in the input data

## Scripts 2 to 4: 
These Scripts reshape the time series and coordinate files in a proper format for Cluster Analysis. 
They also add Climate Zone and Continent information to the Scripts.
