## Code for the Cognitive Flexibility in Aging Paper

Grahek, I., Leng, X., Fengler, A., & Shenhav, A. (2025) Slower transitions between control states lead to reductions in cognitive flexibility over the lifespan. <i>bioRxiv</i>. https://www.biorxiv.org/content/10.1101/2025.08.27.672689v1.abstract

## Contents
---

## 1. `analyses` Directory

Contains scripts and models for response time, accuracy analyses, and Drift Diffusion Model (DDM) analyses.

### 1.1. `brms` Folder

**Purpose:** Scripts related to Bayesian regression models using `brms`.

- **`Cluster/`**:  
  - **`bash/`**: Job submission scripts for cluster computing.  
  - **`models/`**: Scripts for fitting specific models.  
  - **`output/`**: Model output files (currently empty; limited by GitHub file size).  

- **`CAC_Aging_Brms.Rmd`**:  
  R Markdown file for analyzing `brms` models and generating figures.

---

### 1.2. `hssm` Folder

**Purpose:** Scripts to fit Drift Diffusion Models using the HSSM package.

- **`analysis/`**:
  - **`analyze_models.ipynb`**: Jupyter notebook for analyzing HSSM models and saving posteriors.  
  - **`hssm_analysis_helpers.py`**: Helper functions used in the Jupyter notebook.  
  - **`DDM_analyses_and_plots.Rmd`**: R Markdown file for DDM model analysis and figure creation.  
  - **`.csv` files**: Posterior samples generated from the notebook, used in Markdown analyses.  
  - **`.rds` files**: Regression models derived from Markdown analyses.

- **`data/`**:
  - **`.csv` files**: Preprocessed data used to fit models.

- **`models/`**:
  - DDM models fitted to the data.

- **`output/`**:
  - Model files (currently empty due to size limitations).

---

## 2. `data` Directory

Contains raw and preprocessed behavioral data.

- **`ProcessedData/`**:  
  - `.csv` file with preprocessed data.

- **`RawData/`**:  
  - Contains raw data files (not uploaded due demographic information).

---

## 3. `preprocessing` Directory

**Purpose:** Scripts for preprocessing raw data into format suitable for analysis.

- **`Preprocessing.R`**: Main script for data preprocessing.
- **`PreprocessingSummary.csv`**: Summary of the excluded participants and total participant numbers after preprocessing. 

---
