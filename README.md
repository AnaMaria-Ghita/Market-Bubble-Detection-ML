# Financial Bubble Detection & Analysis

An empirical and quantitative framework combining advanced econometrics and machine learning algorithms to detect, date-stamp, and forecast speculative price bubbles across traditional and emerging financial assets.

---

## 📌 Project Overview

Speculative bubbles represent periods where asset prices significantly deviate from their intrinsic fundamental values, followed by abrupt market crashes. This project implements a two-stage methodology:

1. **Econometric Identification & Date-Stamping (R):**
   - Unit root and stationarity confirmation via **ADF** (Augmented Dickey-Fuller) and **KPSS** (Kwiatkowski-Phillips-Schmidt-Shin) tests.
   - Exponential trend divergence detection via **EXCF** (Exponential Curve Fitting).
   - Recursive real-time bubble detection and chronologic date-stamping via the **GSADF** (Generalized Supremum Augmented Dickey-Fuller) test with Monte Carlo critical values.

2. **Predictive Classification & Risk Profiling (Python):**
   - Feature engineering of technical and behavioral market indicators (returns, 20-day rolling volatility, 20-day skewness, 10d/20d momentum, and volume variations).
   - Binary classification using **Random Forest** and **XGBoost** to predict bubble regimes against the econometric ground truth.
   - Handling class imbalance using balanced weights and cost-sensitive scale ratios.

---

## 📊 Analyzed Assets (2015 – 2026)

The framework is tested across multiple financial dimensions:

| Asset Class | Ticker / Symbol | Description | Source |
| :--- | :--- | :--- | :--- |
| **Broad Market** | `^GSPC` | S&P 500 Index | Yahoo Finance |
| **Tech Equities** | `^IXIC` | NASDAQ Composite Index | Yahoo Finance |
| **Precious Metals** | `GLD` | SPDR Gold Shares ETF (Safe-Haven) | Yahoo Finance |
| **Energy Commodity**| `USO` | United States Oil Fund (Crude Oil) | Yahoo Finance |
| **Crypto Assets** | `ETH-USD` | Ethereum (Decentralized digital asset) | Yahoo Finance |

---

## 🔬 Methodology & Architecture

### 1. Data Cleaning & Statistical Testing (`R`)
- Logarithmic price transformations and daily log returns calculation.
- Order of integration confirmation: testing $I(1)$ vs $I(0)$ stationarity properties.
- Rolling window estimation ($T_i = 150$) for explosive parameter $\omega_1 > 1$ with persistence filtering (minimum 5 consecutive days).
- Flexible sub-sample recursive estimation using backward supremum ADF sequences ($BSADF_{r_2}(r_0)$).

### 2. Machine Learning Pipeline (`Python`)
- **Features Extracted:**
  - Daily Returns ($R_t$)
  - 20-day Rolling Volatility ($\sigma_{20, t}$)
  - 20-day Skewness
  - Momentum indicators ($MOM_{10, t}$, $MOM_{20, t}$)
  - Volume change rate ($\Delta V_t$)
- **Validation Strategy:** Chronological temporal split (80% Train / 20% Test Out-of-Sample) to prevent data leakage.
- **Models Evaluated:** Random Forest (`class_weight='balanced'`) & XGBoost (`scale_pos_weight`).

---

## 📈 Key Findings

- **Integration Order:** All 5 asset price series were confirmed to be non-stationary at level and stationary after first differencing, exhibiting an $I(1)$ integration order.
- **Asset Divergence:** Digital and high-growth assets (Ethereum, NASDAQ) exhibited higher frequency and longer durations of explosive bubble episodes compared to defensive assets.
- **Safe-Haven Behavior:** While volatility is the primary predictive feature for equities and crypto, **Skewness (asymmetry)** is the dominant predictor for Gold (`GLD`), reflecting unique safe-haven capital inflow dynamics.
- **Model Trade-offs:** Random Forest delivers exceptional precision (minimizing false alarms), whereas XGBoost achieves superior Recall and overall $F_1$-Score in capturing rare macroeconomic bubble regimes.