# Elab
# Predicting Price to Earnings Ratios Using Compustat Financial Data

# Overview
This project was completed as part of the ELAB 1 (EBC1049) course at Maastricht University, where our team worked with real Compustat financial data to clean, transform, and model firm-level indicators in order to predict Price-to-Earnings (P/E) ratios.
The goal was to simulate the work of a financial analyst by using valuation multiples and statistical models to identify overvalued or undervalued firms.


# Objectives
1. Clean and preprocess real-world corporate finance data from Compustat.
2. Engineer financial ratios such as Market-to-Book (MTB), Return on Assets (ROA), Return on Equity (ROE), and Investment Growth (inv).
3. Model the 3-year smoothed P/E ratio using accounting variables.
4. Compete in a prediction challenge to achieve the lowest Mean Squared Error (MSE) on a withheld test set.

# Dataset
- Source: WRDS – Compustat Database
- Scope: Active domestic U.S. firms (Industrial Format, consolidated, USD reporting)
- Period: Annual data from 2004 onward
- Key Raw Variables: Total assets, liabilities, earnings, share price, equity, number of shares, interest expenses.

- Engineered Variables:

  - MTB – Market-to-Book ratio

  - size – Log of total assets

  - EPS – Earnings per share

  - PE – Price to Earnings ratio (3-year smoothed)

  - inv – Investment growth rate

  - ROA – Return on Assets

  - ROE – Return on Equity

# Results
- Models were trained on 95% of available firms and tested on 5% withheld data.
- Best-performing models achieved significant accuracy improvements over the linear baseline in certain years.
- Key insight: Larger, more profitable firms tended to have lower P/E ratios, while high-growth firms had higher ratios consistent with corporate finance theory.
