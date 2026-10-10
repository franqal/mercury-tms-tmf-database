# Mercury Trophic Magnification Database — Shiny app

## Files
- `app.R`: Shiny application.
- `Datos.csv`: validated dataset loaded automatically when the app starts.

## Run locally
Open this folder as an RStudio project or set it as the working directory, then run:

```r
install.packages(c("shiny", "bslib", "DT", "leaflet", "dplyr", "readr", "tibble", "htmltools"))
shiny::runApp()
```

The app loads `Datos.csv` automatically. Visitors can optionally upload another CSV for their current session; uploaded data are not saved to the server.

## Publishing
Publish this folder (both `app.R` and `Datos.csv`) using `rsconnect` to a Shiny hosting service.

## Data citation
This package does not yet have a DOI. After reviewing the metadata, license, authorship, and references, archive a release of the dataset and/or software in Zenodo to obtain a DOI. Cite the exact archived version used.
