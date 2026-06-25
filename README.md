# InhibitoryControl

A collection of R scripts for preprocessing, quality control, and behavioral analysis of inhibitory control tasks used in cognitive neuroscience research.

## Overview

This repository contains utilities for processing behavioral data from computerized inhibitory control paradigms. The scripts include data validation, quality control, generation of fMRI timing files, and computation of behavioral performance measures, producing standardized outputs for statistical analyses.

The repository is designed to expand over time as additional inhibitory control paradigms (e.g., AX-CPT) are incorporated.

---

## Repository Contents

### `Flanker4_timingfile.R`

Processes raw Flanker task behavioral data exported from E-Prime.

Features include:

* Reads participant behavioral files across multiple runs
* Validates participant IDs and session dates
* Verifies task completeness and expected trial counts
* Automatically handles different file encodings
* Computes stimulus onset times relative to run onset
* Separates congruent and incongruent trials
* Classifies responses as correct, commission, omission, or error
* Calculates reaction time and accuracy measures
* Generates AFNI-compatible `.1D` timing files for first-level fMRI analyses
* Produces participant-level summary statistics
* Appends group-level behavioral summaries across participants

---

## Current Outputs

For each participant generates:

* AFNI timing files for each task condition
* Run-wise behavioral summaries
* Overall participant summary statistics
* Group-level summary dataset across all processed participants

---

## Planned Additions

This repository will continue to expand with preprocessing utilities for additional inhibitory control paradigms, including:

* AX Continuous Performance Task (AX-CPT)
* Shared quality-control utilities
* Group-level behavioral analysis scripts
* Task-specific summary pipelines

---

## Needs the following packages
```
* tidyverse
* dplyr
* purrr
* data.table
* readr
```
---
