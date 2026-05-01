# RFM-Based Customer Segmentation & Pricing Optimization

## Overview

This project combines customer segmentation with demand estimation to analyze how price changes impact revenue. Customers are grouped using RFM features, and price sensitivity is estimated to simulate revenue under different pricing strategies.

---

## Key Steps

* Cleaned transaction-level retail data
* Built RFM features (Recency, Frequency, Monetary)
* Segmented customers using K-means clustering
* Estimated price elasticity using a log-log model
* Simulated revenue under price changes (0–30%)
* Identified the revenue-maximizing price increase

---

## Key Insight

Revenue increases with price up to a point, after which demand drops dominate.
Different customer segments exhibit different price sensitivities, making uniform pricing suboptimal.

---

## Tools

R, dplyr, ggplot2, factoextra, purrr

---

## Data

The dataset (Online Retail II) is publicly available but not included due to size.
Download separately and place in your working directory.

---

## Limitations

* No controls for product, time, or promotions
* Elasticity assumed constant in estimation
* Nonlinearity introduced via heuristic adjustment

