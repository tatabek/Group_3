
# Group 3 – Production Planning

## Members
| Name | GitHub username |

| Elif İrem Atabek | tatabek |
| Sena Tuncer | senatuncer |
| Meltem Derya Çetin | meltemderya |
| Furkan Kaan - | kullanici4 |

## Modules
- Learning Curve
- Moving Average
- Forecasting
  - Stationary Series: Moving Average, Best N (MA), Weighted MA, Exponential Smoothing, Quick and Dirty Method, Seasonal Factors
  - Trend: Holt's Method, Seasonal Decomposition (MA), Winters' Method
  - Time Series (ARIMA): auto.arima, time series plot, ACF, PACF
- Aggregate Planning
  - Heuristic plans: zero-inventory (chase), constant workforce (level), mixed plan
  - Linear programming: overtime, idle time and subcontracting options
  - Transportation method: north-west corner, optimal solution (cheapest cell first)
  - Disaggregation: square-root rule for product families

## Data
Forecasting: upload a .csv or .xlsx file with one demand column (e.g. Period, Demand).

Aggregate Planning: upload a .csv or .xlsx file with the columns Period, Demand and Working days
(a sample file can be downloaded inside the module). Period names can be edited in the app.

## Packages
shiny, readxl, forecast, lpSolve (new, used by Aggregate Planning).
After installing lpSolve run `rsconnect::writeManifest()` to refresh manifest.json before deploying.

## Live app
(Connect Cloud link)
