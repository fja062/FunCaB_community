# FunCaB_community

## Project Overview

This repository contains the analysis pipeline and data processing code for the **FunCaB (Functional group removal experiment in alpine and boreal grasslands)** project. This is a comprehensive plant functional group removal experiment conducted across climate gradients in Western Norway to understand how climate change affects biodiversity and ecosystem functioning in alpine grasslands.

## Background

Climate change poses significant threats to alpine ecosystems by altering plant distributions, community composition, and biotic interactions. The FunCaB experiment was designed to disentangle the direct and indirect consequences of climate on ecosystems by manipulating plant functional groups across environmental gradients.

## Experimental Design

The experiment was conducted across **12 sites** in the Vestland Climate Grid (VCG) spanning:
- **4°C** in growing season temperature
- **2000 mm** in mean annual precipitation
- Boreal and alpine regions of Western Norway

### Experimental Setup
- **Fully factorial plant functional group removal experiment**
- **Three functional groups**: graminoids, forbs, bryophytes
- **384 experimental and control plots** monitored over 6 years
- **Climate grid approach** to assess interactions across environmental gradients

## Data Used in This Repository

This repository focuses on analyzing the **biomass** and **plant community** data from the FunCaB experiment:

| Data Type | Records | Description |
|-----------|---------|-------------|
| Biomass | 5,412 | Plant functional group biomass measurements |
| Species-level biomass | 360 | Individual species biomass records |
| Plant community | 17,181 | Plant records covering 206 taxa |

### Biomass Data
- Functional group biomass measurements (graminoids, forbs, bryophytes)
- Individual species biomass records
- Collected over multiple years across all experimental plots

### Plant Community Data
- Species composition and abundance data
- Covers 206 plant taxa across all sites
- Includes both experimental and control plots
- Temporal data spanning the experimental period

## Repository Structure

```
FunCaB_community/
├── R/                          # R analysis scripts
│   ├── functions/              # Custom functions for data processing
│   ├── analysis_plan.R         # Main analysis pipeline
│   ├── download_plan.R         # Data download and preparation
│   └── figure_plan.R          # Figure generation
├── manuscript/                 # Manuscript files (Quarto)
├── data/                      # Data storage
├── tests/                     # Test files for pipeline validation
├── _targets.R                 # Targets pipeline configuration
└── run.R                      # Main execution script
```

## Key Features

- **Reproducible analysis pipeline** using the `targets` package
- **Modular function design** for data cleaning and analysis
- **Comprehensive testing** of pipeline components
- **Integration with climate data** from the Vestland Climate Grid
- **Community ecology analysis** tools
- **Statistical modeling** with mixed effects approaches

## Getting Started

1. **Install dependencies**:
   ```r
   source("libraries.R")
   ```

2. **Run the analysis pipeline**:
   ```r
   source("run.R")
   ```

3. **View pipeline network**:
   ```r
   targets::tar_visnetwork()
   ```

## Citation

If you use this code or data, please cite:

Vandvik, V., Althuizen, I.H.J., Jaroszynska, F. *et al.* The role of plant functional groups mediating climate impacts on carbon and biodiversity of alpine grasslands. *Sci Data* **9**, 451 (2022). https://doi.org/10.1038/s41597-022-01559-0

## Contact

For questions about the analysis pipeline or data processing, please contact the repository maintainers.

---

*This repository contains the computational workflow for analyzing plant functional group removal experiment data from alpine grasslands in Western Norway.*
