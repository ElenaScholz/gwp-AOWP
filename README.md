# DLR-GWP-PCA

Analysis pipeline for classifying the seasonal dynamics of global water bodies
(Annual Open-Water Patterns, AOWPs) from the DLR Global WaterPack (GWP) time
series, using k-means cluster analysis.

# Folder Structure

```
project_root/
├── scripts/
├──── 00_preprocessing
├──── 01_prepareClusterAnalysis
├──── 02_ClusterAnalysis
├──── 03_visualisation
├──── utils
└── docs/
```

# Setting your data root

Every script starts with a `config` list containing a `ROOT` entry, e.g.:

```r
config <- list(
  # EDIT THIS: set to your local project root
  ROOT = "path-to-your-gwpAOWP-folder/",
  ...
)
```

Before running any script, replace this placeholder with the absolute path to
your local copy of the data folder (see "Data Folder Structure" below). All
other paths in the script are built relative to `ROOT`, so this is the only
change required per script. No other configuration should require editing
unless you renamed folders or files from the convention described below.

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
| `renv::snapshot()` aborts with "pre-flight validation failure" mentioning Bioconductor | Your installed `BiocVersion` doesn't match `bioconductor.version` in `renv/settings.json`. Check the installed version with `installed.packages()["BiocVersion", "Version"]` and set `renv/settings.json`'s `bioconductor.version` to match (e.g. `BiocVersion` `3.21.1` → `"3.21"`), then re-run `renv::snapshot()`. |

For further help, see the [renv documentation](https://rstudio.github.io/renv/articles/renv.html).

# Data Folder Structure

```
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
│   │   │   ├── kmns_cla_2.txt
│   │   │   ├── kmns_cla_3.txt
│   │   │   ├── kmns_cla_...txt
│   │   │   └── kmns_cla_15.txt
│   │   └── plots
│   │   └── CLA_DAT_full.dat
│   │   └── CLA_DAT_full_withMetaData.dat
│   │   ├── data
│   │   │   ├── 03_AnnualClassResults.txt
│   │   │   ├── AnnualClassResults.txt
│   │   │   ├── KMN_LakeClass_results.txt
└   └   └   └── KMN_LakeClass_results_new.txt
```

# Pipeline overview

Run the scripts in numeric/alphabetic order within each folder. Each stage
reads the output of the previous one:

```
00_preprocessing        raw GWP timeseries -> cleaned, gap-filled, calendar-aligned timeseries
01_prepareClusterAnalysis  cleaned timeseries -> CluDat/ input matrices for cluster analysis
02_ClusterAnalysis      CluDat/ matrices -> normalized data, k-means runs, cluster count diagnostics
03_visualisation        cluster results -> figures and summary tables
```

---

# 00_preprocessing

Preprocessing transforms the raw timeseries data into ready-to-use time
series for further analysis.

## Information for all three scripts
1. Run the three scripts in this folder in order (1-3).
2. Check the paths given in the `config` section at the top of each script — set `ROOT` to your local data folder (see "Setting your data root" above).
3. Make sure the folder with the GWP timeseries is named consistently with the convention used throughout the pipeline (e.g. `01_timeseries_8247`), or update the folder names in `config` consistently across all scripts.

## 00-1_RemoveDuplicates.R
Checks whether a lake is located on two MODIS tiles. If so, removes the duplicated time series and recombines/recalculates the area values for each date.

- **Input:** `Input/01_timeseries_8247/`
- **Output:** `Input/03_timeseries_8247_cor/` (corrected time series), plus a quarantine copy of the original per-tile files in `Input/02_timeseries_8247_preliminary/` and a summary log at `Input/00-1_RemoveDuplicates_summary_log.txt`

## 00-2_CalculateMissingDate.R
Checks whether dates are missing within a time series and fills them in. Since only single days are typically missing (e.g. Dec 31 in some years), linear interpolation is used.

- **Input:** `Input/03_timeseries_8247_cor/`
- **Output:** `Input/04_timeseries_8247_allDates/`

## 00-3_rm2902.R
Removes February 29 from all leap years, so every year has a consistent 365-day calendar for later reshaping.

- **Input:** `Input/04_timeseries_8247_allDates/`
- **Output:** `Input/05_timeseries_8247_rm2902/`

---

# 01_prepareClusterAnalysis

These scripts reshape the time series and coordinate files into the matrix
format needed for cluster analysis, and attach climate zone and continent
metadata.

## 01-1_RestructureTimeSeries.R

Reads all SGV timeseries files from the input directory and produces four
aggregated output tables — a daily and a monthly version, each available
with and without metadata identifiers.

**Input:**
- Directory: `Input/05_timeseries_8247_rm2902/`
- Format: semicolon-separated `.txt` files with columns `Date` (YYYY-MM-DD) and `Area`
- Naming convention: `<prefix_id>_SGV-timeseries_allDates_rm2902.txt`

**Output** (written to `CluDat/`, space-separated, decimal point `.`):

| File | Contents |
|---|---|
| `area_combination.txt` | Daily matrix — values only, no headers |
| `area_combination_withPrefix.txt` | Daily matrix — with `prefix_id` and `Year` columns |
| `area_combination_month.txt` | Monthly average matrix — values only, no headers |
| `area_combination_month_withPrefix.txt` | Monthly average matrix — with `prefix_id` and `Year` columns |

## 01-2_nameVectorFile.R

Builds a `<lake_id>_<row_index>` identifier for every row of the daily
area-combination matrix (one row per lake per year), so each row of the
cluster analysis output can later be traced back to a specific lake and
position.

- **Input:** `Input/03_timeseries_8247_cor/` (only used to list lake files and derive IDs)
- **Output:** `CluDat/complete_name_vec.txt`

## 01-3_CreateCoordinatesFiles.R

Matches coordinate files to timeseries files by prefix ID, combines them into
one table, and splits out latitude/longitude columns.

- **Input:** `Input/00_coordinates_8247/` (per-lake coordinate files) and `Input/05_timeseries_8247_rm2902/` (to determine which lakes have a matching timeseries)
- **Output:**
  - `CluDat/all_coordinates_complete.txt`
  - `CluDat/coordinates_latitude_only.txt`
  - `CluDat/coordinates_longitude_only.txt`

## 01-4_add_ClimateZonesAndContinents.R

Prerequisite: `gwp_HylaksWithClimateZone.gpkg` — a spatial join of
Köppen-Geiger climate zones with all GWP coordinates.

Joins each lake's coordinates against the Köppen-Geiger classification and
country/continent information, and writes two lookup tables used later for
grouping results by climate zone and continent.

- **Input:** `Input/gwp_HylaksWithClimateZone.gpkg`, `CluDat/all_coordinates_complete.txt`
- **Output:** `CluDat/Points_Climate.txt`, `CluDat/Points_Cont.txt`

**Notes:**
- The monthly aggregation (in 01-1) uses the arithmetic mean across all days within each month (`na.rm = TRUE`).
- The `prefix_id` is extracted from each filename by stripping a fixed suffix pattern — ensure filenames follow the expected convention.
- If files have unequal row counts, a warning is printed but processing continues; check for date range mismatches in the input data.
- One lake (`h09v07_9798_1`) lies in the water-area of the Köppen-Geiger raster and needs to be classified manually.

---

# 02_ClusterAnalysis

## 02.1_create_CLA_INPUTS.R

Normalizes the daily area-combination matrix by each row's (year's) maximum
lake extent, producing the input matrix used for k-means clustering.

**Workflow:**
1. Read `area_combination_withPrefix.txt`.
2. Replace invalid records (negative values, `NA`) with 0.
3. Split off the metadata columns (`prefix_id`, `Year`).
4. Normalize each row by its own maximum value.
5. Write the normalized matrix, with and without metadata.

- **Input:** `CluDat/area_combination_withPrefix.txt`
- **Output:** `CluDatOutput/CLA_DAT_full.dat` (normalized data only), `CluDatOutput/CLA_DAT_full_withMetaData.dat` (normalized data + `prefix_id`/`Year`)

The file also contains a `#==== not used` section for generating
latitude-stratified random subsamples of lakes; it is not part of the current
pipeline and is left as-is for reference.

## 02.2_FindRightCluster.R

Runs k-means clustering (k = 2 to 15) against a fixed set of pre-computed
cluster centroids (`.cnt` files), producing a cluster assignment for every
row of the normalized data at each value of k.

- **Input:** `CluDatOutput/CLA_DAT_full.dat`, centroid files `CluDat/cla_runs/kmn_<k>.cnt`
- **Output:** `CluDatOutput/kmns_cla/kmns_cla_<k>.txt` for k = 2..15

## 02.3_NCL_analysis.R

Evaluates cluster quality across k = 2 to 15 to help choose the number of
clusters (NCL = "number of clusters"). For each k, computes:

- Within-cluster sum of squares (WSS)
- Explained variance
- Faster Silhouette Index (Beck et al., after Rousseeuw 1984)
- Davies-Bouldin index
- Krzanowski-Lai index (when computable)

- **Input:** `CluDatOutput/CLA_DAT_full.dat`, `CluDat/cla_runs/kmn_<k>.cnt`, `CluDatOutput/kmns_cla/kmns_cla_<k>.txt`
- **Output:** a PDF of all metrics vs. k, written to `CluDatOutput/plots/NCL_analysis_eucl_cla-<iter.max>.pdf`

---

# 03_visualisation

## 03_VisClassResults.R

Produces the final classification figures and summary tables once the
number of clusters (k) has been chosen. Reads the normalized lake data,
climate/continent lookups, coordinates, and the k-means centroids/assignments
for the selected k.

Generates:
- Cluster center panels with per-AOWP percentage of water bodies, plus a
  world map of the dominant AOWP per lake
  (`CluDatOutput/plots_NewLayout/03.1...tif`)
- The same panels/map per individual year
  (`CluDatOutput/plots_NewLayout/03_NL_Class_results_annual.tiff`)
- A world map of AOWP variability (number of distinct AOWPs per lake over the
  study period) (`.../03.2...tiff`)
- Annual classification results per lake, written to
  `CluDatOutput/data/03_AnnualClassResults.txt`
- Classification summary tables, written to
  `CluDatOutput/data/KMN_LakeClass_results.txt` and `KMN_LakeClass_results_new.txt`
- Maps and transition matrices of AOWP changes between the first and second
  half of the study period, globally and split by climate zone / continent
  (`.../03.3...tiff`, `.../03.4...tiff`)
- Annual distribution heatmaps by climate zone and continent
  (`.../03_AnnualLakeDistribution_Climates.tif`, `.../03_AnnualLakeDistribution_Continents.tif`)

- **Input:** `CluDat/Points_Climate.txt`, `CluDat/Points_Cont.txt`, `CluDat/all_coordinates_complete.txt`, `CluDatOutput/CLA_DAT_full.dat`, `CluDat/cla_runs/kmn_<k>.cnt`, `CluDatOutput/kmns_cla/kmns_cla_<k>.txt`
- **Output:** figures in `CluDatOutput/plots_NewLayout/`, tables in `CluDatOutput/data/`

---

# scripts/utils

Shared helper functions sourced by the scripts above; not run directly.

## helperfunctions.R
General-purpose I/O and data-cleaning helpers used throughout the pipeline:
`read_data()` (bulk-read a folder of timeseries files), `save_datasets()`,
`check_for_invalid_data()` (zero-fills negative/`NA` records), and
`normalize_data()` (row-wise max normalization used in 02.1).

## vis.R
Plotting and post-processing functions used by `03_VisClassResults.R`:
building the lake color palette, reordering/relabeling clusters, computing
each lake's dominant cluster and variability, world map plotting (via
`rnaturalearth` + Robinson projection), and the annual/transition-matrix
figure functions.

## 99_TSA_customfunctions.R
Older auxiliary functions from the original TIMESAT-based seasonality
workflow (pre-/post-processing for TIMESAT, field-vs-remote-sensing
comparison plots). Only `AddRowVal()` is used by the current pipeline
(in `00-1_RemoveDuplicates.R`); the rest is retained for reference to earlier
analysis steps not part of this repository's pipeline.
