# Geopolitical Shock Detection & Energy Price Regime Classification

This repository contains a quantitative framework for identifying structural breaks in energy markets caused by geopolitical events and classifying market states into distinct price/volatility regimes. This project leverages time-series econometrics and unsupervised learning to model the relationship between global instability and energy commodity dynamics.

## Project Overview
Energy markets are characterized by non-linear dynamics and high sensitivity to exogenous shocks. This project implements:
* **Shock Detection:** Identifying statistically significant anomalies and structural breaks in price series (e.g., Brent Crude, Henry Hub).
* **Regime Classification:** Utilizing latent variable models to distinguish between stable, volatile, and transitionary market states.
* **Risk Modeling:** Correlating market shifts with the Geopolitical Risk (GPR) Index to assess the impact of global events on price elasticity.

## Methodology

### 1. Statistical Shock Detection
We apply structural break tests to identify points where the data-generating process changes:
* **Quandt Likelihood Ratio (QLR) Test:** Used for detecting unknown breakpoints in time-series data.
* **Z-Score Anomaly Detection:** Flagging extreme price spikes that deviate from historical moving averages.

### 2. Regime Switching Models
The core of the classification relies on stochastic processes that assume the existence of "hidden" states:
* **Markov Switching Models (MSM):** Modeling time-varying mean and variance to capture "High-Volatility" vs "Low-Volatility" regimes.
* **Hidden Markov Models (HMM):** Decoding latent market states based on observed daily returns and trading volumes.
* **Gaussian Mixture Models (GMM):** Clustering price-volatility pairs to define economic market structures.

### 3. Exogenous Factors
Integration of the **Caldara and Iacoviello Geopolitical Risk (GPR) Index** to validate if detected structural breaks coincide with significant global political events.

## 📂 Repository Structure
```text
├── datasets/        # Historical energy prices and GPR indices
├── codes/           # Core R/Python implementations of HMM and structural tests
├── outputs/         # Exploratory Data Analysis and model training logs
|    ├── figures/     # Plots showing detected regimes and structural breaks
├── Report           #Final reports containing all the findings
└── README.md
```

## Getting started

### 1.Prerequisites
**R**: depmixS4, strucchange, vars, ggplot2

### 2.Statistical Considerations
* **Non-Stationarity**: All price data is transformed into log-returns to ensure mean-reverting properties before modeling.
* **The Markov Assumption**: We evaluate whether the transition probability $P(S_t | S_{t-1})$ is sufficient to capture regime persistence during prolonged geopolitical conflicts.
* **Endogeneity**: Consideration of how energy price shocks may act as a feedback loop, further escalating geopolitical tensions.

## References
* Hamilton, J. D. (1989). A New Approach to the Economic Analysis of Nonstationary Time Series and the Business Cycle.
* Caldara, D., & Iacoviello, M. (2022). Measuring Geopolitical Risk. American Economic Review.


