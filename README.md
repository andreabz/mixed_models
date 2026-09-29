# Mixed Models from an Experimental Perspective

This repository explores **mixed-effects models as a way to represent the structure of experimental data**, rather than as a more complicated alternative to ordinary linear models.

The starting point is a practical analytical-chemistry question:

> **How much of the variability I observe is associated with the experimental conditions, and how much is associated with the samples themselves?**

The project uses a simulated extraction experiment to examine what changes when observations are not independent and the experimental unit is different from the individual measurement.

The analysis is written in **R** and **Quarto**, with an intentionally small and transparent example.

## The experimental structure

The simulated experiment contains:

- 6 soil samples;
- 3 temperatures;
- 4 extraction solvents;
- 2 replicates per condition.

The response variable is **extraction yield**.

The important feature is the grouping by soil: observations from the same soil are expected to share characteristics that are not captured by temperature or solvent alone.

This means that treating every measurement as an independent observation can misrepresent the amount of information contained in the experiment.

---

## From the experiment to the model

The project is organised around a sequence of questions:

1. **What is the experimental unit?**
2. **Which observations can reasonably be considered independent?**
3. **What changes when the grouping structure is ignored?**
4. **What does a random effect represent in this experiment?**
5. **How does the model change the estimated uncertainty?**
6. **What can variance components tell us about the experimental process?**

The transition from `lm()` to `lmer()` is therefore not presented as a progression from a simple model to a sophisticated one.

It is a response to a different description of the data.

---

## Why mixed models?

Experimental data often contain sources of structure such as:

- samples or subjects;
- batches;
- analytical runs;
- instruments;
- days;
- laboratories;
- repeated measurements.

When observations share one of these sources, their variability can be correlated or otherwise structured.

A mixed-effects model provides a framework for representing some of this structure through **fixed effects and random effects**. More generally, mixed models are widely used for grouped, hierarchical, longitudinal, and repeated-measures data. citeturn0search0

The important point for this project is not the terminology itself, but the connection between the model and the way the experiment was actually performed.

---

## What the project tries to make visible

The analysis focuses on a few principles:

- **The experimental unit matters.**
- **Variability has structure.**
- **More observations do not necessarily provide proportionally more information.**
- **Random effects are a way of representing sources of variation, not simply a technical option in a model formula.**
- **Model choice affects uncertainty as well as estimated effects.**

These ideas are particularly relevant when analytical measurements are repeated on samples, batches, instruments, days, or other naturally grouped experimental units.

---

## Contents

- `report.qmd`  
  Main Quarto document containing the analysis and narrative.

- `R/create_dataset.R`  
  Script used to generate the simulated dataset.

---

## Reproducibility

The analysis uses a small, explicit R environment managed with `renv`.

To reproduce the report:

```bash
git clone https://github.com/andreabz/mixed_models.git
cd mixed_models
```

Then restore the project environment:

```r
install.packages("renv")
renv::restore()
```

Render the Quarto report:

```bash
quarto render report.qmd
```

---

## Software

The analysis intentionally uses a small set of packages:

- `data.table` for data manipulation;
- `ggplot2` for visualisation;
- `lme4` and `lmerTest` for mixed-effects modelling;
- `Quarto` for reproducible reporting;
- `renv` for environment management.

---

## Scope and limitations

This is a **simulation-based methodological example**, not a general introduction to mixed models.

The experiment is deliberately small and simplified. The simulated data cannot reproduce all the complexities of a real analytical method, and the conclusions depend on the assumed data-generating process.

The purpose is instead to make the connection between **experimental design, dependence, variance structure, and statistical modelling** explicit.

In particular, the project does not claim that a mixed model is automatically required whenever data are grouped. The relevant question is whether the grouping reflects a meaningful source of variation or dependence and whether that structure matters for the scientific objective.

---

## The broader question

The project is ultimately less about `lmer()` than about a more general principle:

> **Before choosing a statistical model, understand how the data were generated.**

When the physical structure of an experiment changes, the appropriate statistical representation may need to change with it.
