# Trying to Understand Mixed Models (from an Experimental Perspective)

This repository contains a Quarto report documenting my attempt to better understand mixed models.

I did not start from statistical theory.

I started from a practical question in analytical chemistry:

> how much of the variability I observe is due to the method, and how much comes from the matrix?

## Motivation

In many experiments, especially in analytical chemistry, data are not truly independent.

Measurements are often:

- repeated within the same system  
- influenced by hidden structure (batch, day, instrument, matrix)  

Ignoring this structure can lead to:

- overestimating the amount of information  
- underestimating uncertainty  
- drawing misleading conclusions  

This project explores these issues through a concrete example.

## The example

The report is based on a simulated experiment:

- 6 soil samples  
- 3 temperatures  
- 4 extraction solvents  
- 2 replicates per condition  

The response variable is extraction yield.

The key feature of the experiment is that:

> observations within the same soil are more similar than observations across soils.

## What this project tries to do

This is not a tutorial.

It is a structured attempt to answer a few questions:

- What is the real experimental unit?
- When are observations not independent?
- What changes when I move from `lm()` to `lmer()`?
- What am I actually estimating with a random effect?
- How does the structure of the experiment affect uncertainty?

## Key ideas explored

- The model must reflect how the experiment was physically performed  
- Variability is not a single quantity; it has structure  
- More data does not necessarily mean more information  
- Mixed models are not about complexity, but about alignment with reality  

## Contents

- `report.qmd`  
  Main Quarto document containing the full analysis and narrative

- `R/create_dataset.R`  
  Script used to generate the dataset

## Tools

The analysis is intentionally kept minimal:

- `data.table` for data manipulation  
- `ggplot2` for visualization  
- `lme4` and `lmerTest` for mixed models

## How to run

1. Clone the repository:

```
git clone https://github.com/andreabz/mixed_models.git
cd mixed_models
```

2. Reproduce the R environment:

```
install.packages("renv")
renv::restore()
```

3. Render the report

```
quarto render report.qmd
```

## Notes

This work reflects an ongoing learning process, some interpretations may evolve as my understanding improves.
The goal is not to provide definitive answers, but to make the reasoning explicit.

## Why this might be useful

If you work with experimental data, especially in applied contexts:

- chemistry
- biology
- engineering

you may recognize situations where observations are treated as independent by default.
This repository is an attempt to question that assumption.

## Next steps

Areas I am still trying to understand:

- how many groups are needed to estimate variability reliably
- when mixed models materially change decisions
- how to connect variance components to experimental design choices