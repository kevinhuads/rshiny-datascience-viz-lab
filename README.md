# Interactive Data Science Visual Lab in R Shiny

Interactive R Shiny application that illustrates core data science and applied mathematics topics through narrative explanations and interactive visualisations.

The app is organised as an "interactive blog" and covers nine themes: clustering, regression, natural language processing, time series, probability and hypothesis testing, optimisation, epidemiology, Monte Carlo simulation and Markov chains. The emphasis is on intuition, visual storytelling and connections to real datasets.

---

## Live application

The application is deployed on shinyapps.io:

**Live demo:** https://kevinhua.shinyapps.io/DS_viz/

---

## Table of contents

1. Overview  
2. Topics covered  
3. Data sources  
4. Application architecture  
5. Running the app locally  
6. Repository structure  
7. Limitations and future directions  
8. License  

---

## Overview

This project is an R Shiny application created as a data science portfolio and teaching tool. It combines:

- Explanatory text for each topic, written for a technical but non-specialist audience.  
- Interactive visualisations that allow users to explore models, parameters and results.  
- Classical datasets from Kaggle and the UCI Machine Learning Repository, together with simulated examples for probabilistic and stochastic concepts.

The app is positioned as a complement to production grade, Python based projects that focus on MLOps and deep learning. Here the focus is on classical statistics, machine learning and applied mathematics, showcased through a mature R and Shiny ecosystem.

---

## Topics covered

Each section of the Shiny app corresponds to one of the following themes.

### 1. Clustering: grouping US states by exports

- Dataset: US state exports (in million USD) by product category around 2011.  
- Goal: group states according to their export profiles rather than geography.  
- Methods:  
  - Correlation analysis and visual correlation matrix.  
  - Principal Components Analysis (PCA) to reduce 14 export variables to a small number of components.  
  - Clustering on principal components with hierarchical clustering and dendrograms.  
- Visualisations:  
  - Correlation heatmap with several ordering options.  
  - PCA scatterplots for components 1-2, 1-3, 2-3, showing states and variables.  
  - Dendrogram and choropleth map of clusters over the US map.

### 2. Regression: predicting the number of bike rentals

- Dataset: Bike sharing data from the UCI Machine Learning Repository, with hourly counts for 2011-2012.  
- Goal: predict the number of bike rentals given weather, season, calendar and time-of-day features.  
- Methods:  
  - Exploratory analysis of the target distribution and daily patterns.  
  - Correlation analysis and feature screening.  
  - Supervised learning using:  
    - Generalised Linear Model (Gamma regression).  
    - K Nearest Neighbours.  
    - Support Vector Regression.  
    - Regression trees.  
    - Random forest.  
    - Gradient boosting trees.  
  - Train/validation/test split with hyperparameter tuning on the validation set.  
  - Evaluation via deviance, error reduction ratio and visual comparison of observed vs predicted.  

### 3. Natural Language Processing: analysing hotel reviews

- Dataset: large hotel review dataset with several hundred thousand reviews, ratings and metadata.  
- Goal: understand how review content relates to ratings and how vocabulary changes across rating levels.  
- Methods:  
  - Descriptive statistics on review length and rating.  
  - Temporal analysis of average rating by country.  
  - Bag-of-words and n-gram frequency analysis for positive vs negative reviews.  
  - Basic co-occurrence graph and Markov style transitions between words.  
- Visualisations:  
  - Word clouds and bar charts of most frequent tokens by rating slice.  
  - Network graph of word co-occurrences with multiple layout algorithms.  
  - Time series plots of rating trends.

### 4. Time Series: forecasting item sales

- Dataset: daily item level sales across several stores over multiple years (Kaggle style "store item demand" dataset).  
- Goal: forecast daily sales for a specific item and store.  
- Methods:  
  - Train/test split based on time (for example, train on 2013-2016, test on 2017).  
  - Baseline models such as average and naive forecasts.  
  - Prophet based models with configurable seasonality and additive vs multiplicative components.  
- Visualisations:  
  - Time series plots of historical and forecasted sales.  
  - Comparison of baseline and Prophet forecasts using MAE and RMSE.

### 5. Probabilities and Statistics: testing if a coin is fair

- Scenario: repeated coin tosses with an unknown bias.  
- Goal: illustrate the law of large numbers, sampling variability and hypothesis testing.  
- Methods:  
  - Simulation of Bernoulli trials with user controlled probability of heads and number of tosses.  
  - Binomial distribution visualisation and p-value computation for different hypotheses.  
  - Illustration of Type I and Type II errors and their trade-off when changing the p-value threshold.  
- Visualisations:  
  - Empirical proportion of heads vs number of tosses.  
  - Probability mass function of the binomial distribution.  
  - Acceptance and rejection regions for hypothesis tests.

### 6. Optimisation: the Travelling Salesman Problem (TSP)

- Scenario: a salesman needs to visit a set of cities once and return to the starting point.  
- Goal: show the combinatorial explosion of possible routes and the role of heuristic algorithms.  
- Methods:  
  - Naive random route generation as a baseline.  
  - Greedy heuristics.  
  - Simulated annealing to move from poor routes to near optimal ones by controlled random perturbations.  
- Visualisations:  
  - Maps of current and best routes.  
  - Progressive improvement of route length through the simulated annealing process.

### 7. Epidemiology: SIRD model for coronavirus dynamics

- Goal: illustrate how simple compartmental models can capture the spread of an infectious disease.  
- Methods:  
  - SIRD model with compartments for Susceptible, Infectious, Recovered and Deceased.  
  - Parameters controlling contagiousness, recovery duration, mortality, hospital capacity and interventions such as lockdown or vaccination.  
  - Numerical solution of ordinary differential equations.  
- Visualisations:  
  - Time series of compartment sizes.  
  - Effects of changing parameters or introducing interventions on peak load and total deaths.

### 8. Monte Carlo Simulation: approximating π

- Goal: show how random sampling can approximate quantities that have a closed form but are used as pedagogical examples.  
- Methods:  
  - Uniform sampling of points in the unit square.  
  - Estimating π via the proportion of points falling inside the quarter circle.  
- Visualisations:  
  - Scatterplot of sampled points coloured by inside or outside.  
  - Convergence of the π estimate as the number of samples increases.

### 9. Markov Chains: simulating words in different languages

- Dataset: large text corpora for several languages (for example from Project Gutenberg).  
- Goal: demonstrate the Markov property and character based text generation.  
- Methods:  
  - Estimation of transition matrices between characters for each language.  
  - Simulation of synthetic "words" based solely on these transitions.  
- Visualisations:  
  - Tables or lists of generated words per language.  
  - Optional representation of transition probabilities.

---

## Data sources

The application relies on a mix of public datasets and simulated data. The main external datasets are:

- **US agricultural exports by state**  
  Yearly exports per state and product category, originally sourced from a Kaggle dataset referenced in the app.

- **Bike sharing dataset (UCI)**  
  Hourly bike rental counts from 2011 to 2012 with weather, calendar and time-of-day features, from the UCI Machine Learning Repository.

- **Hotel reviews dataset**  
  Several hundred thousand hotel reviews with ratings and metadata from a public Kaggle dataset.

- **Store item sales dataset**  
  Daily item level sales across multiple stores and years, typical of Kaggle forecasting competitions.

Probabilistic examples (coin tosses, Monte Carlo π) and several optimisation and Markov chain scenarios rely on synthetic data generated inside the application.

---

## Application architecture

At a high level, the Shiny app is structured as follows:

- A main overview page with profile, skills and a timeline of professional experience.  
- A table of contents that links to each topic specific section.  
- One tab or page per topic, combining:  
  - Explanatory text.  
  - Interactive controls for parameters, model choices and visual options.  
  - Outputs that combine tables and rich plots (correlation maps, PCA plots, time series, network graphs, word clouds, choropleth maps, etc.).  

The implementation uses standard Shiny patterns and popular libraries for plotting and interactive components, in particular:

- `shiny`, `shinydashboard` or similar for the layout.  
- `ggplot2`, `plotly`, `highcharter` and related packages for visualisations.  
- `DT` for data tables.  
- `prophet` and `deSolve` for time series and ODE modelling.

In this repository, the Shiny application code and its resources live under the `Application/` directory. Scripts used to prepare the datasets are stored separately under `Data_Prep/`.

---

## Running the app locally

### 1. Prerequisites

- R 4.x or newer.  
- Recommended: RStudio for a smoother Shiny development experience.

Install the main R packages used in the app (names to adapt to the actual implementation):

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "DT",
  "plotly",
  "highcharter",
  "prophet",
  "deSolve",
  "wordcloud2",
  "igraph"
))
```

If the project uses `renv`, restore from the lockfile instead:

```r
install.packages("renv")
renv::restore()
```

### 2. Clone the repository

```bash
git clone https://github.com/kevinhuads/rshiny-datascience-viz-lab.git
cd rshiny-datascience-viz-lab
```

### 3. Run the Shiny app

From an R session opened in the repository root:

```r
shiny::runApp("Application")
```

This tells Shiny to use the application located in the `Application/` subdirectory, which contains the UI, server logic and associated resources.

---

## Repository structure

The repository is organised as follows. Only the top level folders and key files are listed.

```text
.
├─ Application/
│  ├─ Saved/              # .RData files prepared by Data_Prep
│  ├─ ui.R, server.R,
│  │  global.R            # Main Shiny entry scripts
│  ├─ shiny_functions.R,
│  │  global_functions.R  # Shared utility functions
│  ├─ servers_scripts/    # Server code for each module
│  ├─ ui_scripts/         # UI code for each module
│  ├─ ...                 # Local resources (data, images, css, js, etc.)
│
├─ Data_Prep/
│  ├─ Rawdata/            # Original raw datasets
│  ├─ *.R                 # One prep script per module
│  │                      # (download, clean, analyse, save to .RData)
│  └─ ...
│
├─ renv/                  # Managed by renv (library infrastructure)
├─ renv.lock              # Locked R package versions
├─ Application.Rproj      # RStudio project file
├─ .gitignore
├─ .gitattributes
├─ LICENSE                # MIT license
└─ README.md              # Project documentation
```

The repository is structured around three main components. The `Application/` folder contains the Shiny app itself, including the main `ui.R`, `server.R` and `global.R` scripts, shared utility functions (`shiny_functions.R`, `global_functions.R`), module specific server and UI scripts under `servers_scripts/` and `ui_scripts/`, and a `Saved/` directory where the prepared `.RData` files used by the app are stored, together with local resources such as images, CSS and JavaScript. The `Data_Prep/` folder contains one R script per module that downloads, explores, cleans and transforms the raw datasets located in `Rawdata/`, then saves the processed data into `.RData` files consumed by the app. Environment reproducibility is handled by `renv/` and `renv.lock`, while the project level files at the root (`Application.Rproj`, `.gitignore`, `.gitattributes`, `LICENSE`, `README.md`) define the RStudio project, version control rules, licensing and documentation.

---

## Limitations and future directions

The app focuses on classical datasets and techniques. It intentionally uses mostly first principles models (GLM, tree ensembles, Prophet, SIRD, Markov chains) rather than deep learning and large language models. This keeps the focus on pedagogy and transparent visualisations.

Potential directions for extension include:

- Adding brief references to more recent techniques (for example, modern NLP embeddings) for context.  
- Including simple model diagnostics (residual plots, cross validation) in the time series and regression sections.  
- Providing direct links from each Shiny section to the corresponding code in the `Application/` directory.

---

## License

This project is licensed under the MIT License. The full text is available in the [`LICENSE`](LICENSE) file.
