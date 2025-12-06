# Toronto Airbnb Analysis

## Project Overview

This project provides a comprehensive analysis of Airbnb listings in Toronto, comparing pricing dynamics, guest satisfaction, and location-based factors with Vancouver. Using data from Inside Airbnb containing 19,276 listings across 18 variables, we explored what drives rental prices and guest experiences in Canada's major cities.

## Dataset

**Source:** Inside Airbnb  
**Scope:** Toronto Airbnb listings  
**Size:** 19,276 rows × 18 columns  
**Key Variables:** Price, location (latitude/longitude), room type, ratings, availability, host information

## Key Research Questions

1. How do Airbnb prices compare between Toronto and Vancouver downtown?
2. Does proximity to landmarks (CN Tower) significantly impact pricing?
3. What is the relationship between host experience and listing prices?
4. How does availability affect pricing?
5. Does the number of host listings correlate with ratings?

## Methodology

### Data Preprocessing
- Extracted subset of relevant categorical and numerical variables
- Parsed listing names to extract ratings, bedrooms, beds, and baths
- Handled missing values and converted data types
- Filtered downtown areas using latitude/longitude boundaries

### Analysis Techniques
- **Exploratory Data Analysis (EDA):** Distribution analysis, correlation matrices, interactive mapping
- **Hypothesis Testing:** Two-sample t-tests, one-sample t-tests, Pearson correlation
- **Regression Analysis:** Linear regression models for price prediction
- **Geospatial Analysis:** Distance calculations from CN Tower and city center

## Key Findings

### Price Comparison
- **Toronto vs Vancouver Downtown:** Vancouver listings are significantly more expensive than Toronto (p = 0.01102)
- **CN Tower Proximity:** Listings within 1 km of CN Tower command higher prices (p ≈ 6.5×10⁻¹⁴²)
- **Distance from City Center:** Properties within 4.7 km of downtown are priced significantly higher

### Guest Satisfaction
- **No significant difference** in ratings between Toronto and Vancouver downtown (p = 0.8793)
- Both cities maintain consistent guest satisfaction levels regardless of number of available listings

### Host Factors
- **Host Experience:** Weak negative correlation with price (R² = 0.027), suggesting experienced hosts may slightly lower prices
- **Host Listing Count:** No significant correlation with ratings (p = 0.4224), indicating quantity of listings doesn't impact guest satisfaction

### Market Dynamics
- **Availability:** No meaningful relationship with pricing (R² = 0.001988)
- **Room Type Distribution:** Entire home/apartment listings dominate the market
- **Geographic Concentration:** Central Toronto (Downtown) has the highest density of listings

## Technologies Used

- **R Programming Language**
- **Statistical Analysis:** t-tests, correlation analysis, linear regression
- **Visualization:** ggplot2, correlation matrices, interactive maps (leaflet)
- **Data Manipulation:** tidyverse, data preprocessing libraries

## Conclusion

The Airbnb rental landscape in Toronto and Vancouver is shaped by multiple interconnected factors beyond simple supply and demand. Location remains the dominant pricing factor, with proximity to landmarks like the CN Tower commanding premium rates. Surprisingly, host experience and the number of available listings show minimal impact on pricing and ratings, suggesting that property characteristics and location matter more than host tenure or market saturation.

Both cities maintain comparable guest satisfaction levels despite price differences, indicating that value perception varies by market. The findings challenge conventional assumptions about the sharing economy, revealing that amenities, unique features, and strategic location play more significant roles than host experience or listing availability in determining both price and guest satisfaction.

## Detailed Documentation

**For comprehensive analysis, statistical outputs, and visualizations, please refer to the [Final Report](Final%20Report-compressed.pdf).**

The detailed report includes:
- Complete descriptive statistics tables
- All hypothesis test results with statistical significance
- Regression model summaries
- Interactive maps and correlation matrices
- Distribution plots and comparative visualizations
- Full methodology and references
