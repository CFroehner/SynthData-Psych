# SynthData-Psych

This repository contains the code used in the study "A Statistical Evaluation of LLM-Generated Data for Psychometrics," which explores the use of GPT4o-generated synthetic response data for psychometric pretesting by evaluating its fidelity and utility.

## Repository Structure

- `Thesis.pdf`: Bachelor's thesis
- `datagen.py`: Script to generate synthetic data via GPT API. Requires `Data/HumanPromptBase.csv` and personal OpenAI API key.
- `functions.R`: Script with R functions for fidelity analysis.
- `analysis.R`: Generates tables and plots for fidelity and utility analysis. Uses `functions.R`, as well as results from `datagen.py` or `Data/HumanBaseSynthetic.csv`, and `Data/ComparisonData.csv`.
- `visualizations_theory.R`: Additional plots to illustrate theoretical concepts (e.g., KL divergence). Requires `Data/HumanBaseSynthetic.csv`.

## Data Access

The required `Data/` folder is not included in this repository.  
Please contact the author to request access. Once granted, add the folder to the root of the repository (i.e., the top-level folder alongside the script files) before running any scripts.

- `Data/HumanPromptBase.csv`: Human base dataset including trait scores and demographics per participant
- `Data/HumanBaseSynthetic.csv`: Combined human and LLM-generated item-level responses per participant
- `Data/ComparisonData.csv`: Additional human dataset for comparison

## How to Use

1. Clone this repository to your local machine:
  ```bash
  git clone https://github.com/CFroehner/SynthData-Psych.git
  cd SynthData-Psych
  ```

2. **Generate Data**: Run `datagen.py` to create the LLM-generated dataset.
  ```bash
  python datagen.py
  ```
  Alternatively, use the pregenerated `Data/HumanBaseSynthetic.csv`.

3. **Analyze Results**: Run `analysis.R` to produce tables and plots for fidelity and utility analysis.
  ```bash
  Rscript analysis.R
  ```

4. **Generate Additional Plots**: Run `visualizations_theory.R` to create supplementary visualizations.
  ```bash
  Rscript visualizations_theory.R
  ```

