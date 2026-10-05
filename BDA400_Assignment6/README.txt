BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase

Student Name: [YOUR NAME]
Repository: [PASTE YOUR PUBLIC GITHUB REPOSITORY LINK HERE]

Project Description
This project is an interactive R Shiny dashboard that downloads historical stock data from Yahoo Finance and visualizes it using line, candlestick, and area charts. Users can choose a stock symbol, date range, and Daily/Weekly/Monthly time frame. The dashboard can dynamically toggle Moving Averages, RSI, and MACD overlays. It implements a customizable short/long moving-average crossover strategy, identifies Buy and Sell crossover events, annotates those events on the chart, and reports the latest trading state.

How to Run
1. Open app.R in RStudio.
2. Install packages if needed:
   install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "scales"))
3. Click Run App, or run: shiny::runApp()
4. Enter a valid Yahoo Finance ticker such as AAPL, MSFT, TSLA, or SHOP.TO.

Assignment Sections Covered
- Data Collection and Setup: Yahoo Finance via quantmod with error handling.
- Visualizing Stock Data: Shiny UI, date/time-frame controls, line/candlestick/area charts.
- Overlay Technical Indicators: SMA, RSI, MACD with dynamic on/off controls.
- Trading Rules and Annotations: customizable moving-average crossover rule with Buy/Sell annotations.

Submission Checklist
- Replace [YOUR NAME] and repository placeholder.
- Create a public GitHub repository.
- Add app.R, README.txt, and the cover page.
- Test the Shiny app in RStudio.
- Commit and push all files.
- Submit the cover page on the LMS using the college naming convention.
