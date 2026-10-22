# ============================================================
# FORECASTING (one file for the whole Forecasting module)
# File location: R/forecasting.R
# Functions used by app.R: forecasting_ui and forecasting_server
#
# CONTENTS
#   1. Shared helpers      colours, sample data, csv/xlsx input, initial values, error measures
#   2. Stationary Series   Moving Average, Best N (MA), Weighted Moving Average,
#                          Simple Exponential Smoothing, Seasonal Factors (quick and dirty)
#   3. Trend Series        Holt's Method
#   4. Seasonal Series     Seasonal Decomposition (MA), Winters' Method
#   5. Box-Jenkins         AR, MA, ARMA, ARIMA
#   6. Main menu           forecasting_ui and forecasting_server
# ============================================================
library(shiny)
library(readxl)

# ############################################################
# 1. SHARED HELPERS
# ############################################################

# ---------- Look and feel ----------
fc_banner <- function(title, subtitle) {
  div(class = "fc-banner", h2(title), p(subtitle))
}

fc_css <- tags$style(HTML("
  .navbar-default{background-color:#0f766e;border-color:#0f766e;}
  .navbar-default .navbar-brand{color:#fff;font-weight:700;}
  .navbar-default .navbar-nav>li>a{color:#d1fae5;}
  .navbar-default .navbar-nav>li>a:hover{color:#fff;}
  .navbar-default .navbar-nav>.active>a,.navbar-default .navbar-nav>.active>a:hover,
  .navbar-default .navbar-nav>.active>a:focus{background-color:#115e59;color:#fff;border-bottom:3px solid #f59e0b;}
  .fc-banner{background:linear-gradient(120deg,#0f766e,#14b8a6);color:#fff;padding:18px 24px;
             border-radius:12px;margin:14px 0 16px 0;}
  .fc-banner h2{margin:0 0 4px 0;font-size:24px;font-weight:700;}
  .fc-banner p{margin:0;opacity:.92;}
  .kpi{display:inline-block;min-width:150px;margin:0 10px 12px 0;padding:10px 16px;border-radius:10px;
       background:#fff;border:1px solid #d9e6e4;border-left:5px solid #14b8a6;}
  .kpi .lab{font-size:11px;color:#5b6b6a;text-transform:uppercase;letter-spacing:.5px;}
  .kpi .val{font-size:26px;font-weight:700;color:#134e4a;}
  .kpi.best{background:#0f766e;border-color:#0f766e;border-left-color:#f59e0b;}
  .kpi.best .lab{color:#ccfbf1;} .kpi.best .val{color:#fff;}
  .btn-run{background:#0f766e;color:#fff;border:none;font-weight:600;}
  .btn-run:hover,.btn-run:focus{background:#115e59;color:#fff;}
  .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{color:#0f766e;font-weight:600;}
"))


# ---------- Shared helpers ----------

# Picks the column whose name contains "demand"; otherwise the last numeric column
fc_pick_demand_col <- function(cols) {
  i <- grep("demand", cols, ignore.case = TRUE)
  if (length(i)) cols[i[1]] else cols[length(cols)]
}

# Sample data 1: 24 periods, no trend, no seasonality
fc_sample_demand <- data.frame(
  Period = 1:24,
  Demand = c(240, 231, 220, 245, 238, 191, 185, 186, 216, 221, 235, 242,
             252, 255, 257, 252, 262, 256, 235, 227, 250, 225, 249, 287)
)

# Sample data 2: 36 months with an upward trend and a yearly seasonal pattern
# (used by the trend methods and ARIMA)
fc_sample_seasonal <- data.frame(
  Period = 1:36,
  Demand = c(171, 182, 196, 209, 219, 240, 261, 255, 238, 221, 211, 201,
             191, 210, 228, 242, 248, 267, 288, 286, 266, 250, 239, 224,
             224, 235, 251, 277, 288, 313, 325, 319, 295, 279, 267, 253)
)

# Sample data 3: 36 months with a yearly seasonal pattern and NO trend
# (used by the stationary-series seasonal methods)
fc_sample_seasonal_flat <- data.frame(
  Period = 1:36,
  Demand = c(155, 172, 188, 198, 207, 228, 242, 236, 205, 189, 189, 186,
             166, 174, 184, 199, 222, 227, 237, 227, 210, 195, 185, 184,
             161, 173, 188, 201, 205, 231, 234, 231, 204, 194, 183, 185)
)

# Sample data 4: stationary ARMA-type series, 60 observations (AR, MA and ARMA modules)
fc_sample_arma <- data.frame(
  Period = 1:60,
  Demand = c(
    88.5, 87.3, 88.4, 93.4, 90.3, 83.6, 91.5, 98.0, 103.5, 102.9,
    103.0, 96.3, 94.8, 99.4, 92.5, 91.8, 89.1, 89.9, 91.4, 98.8,
    99.2, 100.3, 102.2, 99.9, 96.9, 98.9, 96.5, 96.5, 99.9, 108.6,
    103.5, 100.7, 102.0, 104.4, 106.3, 112.5, 108.4, 101.1, 96.8, 93.0,
    91.4, 88.0, 94.1, 96.9, 101.8, 102.5, 107.8, 110.7, 109.9, 107.5,
    111.4, 108.9, 97.2, 98.0, 101.9, 93.4, 97.2, 95.3, 96.4, 98.6
  )
)

# Sample data 5: random walk with drift, 60 observations (ARIMA module)
fc_sample_arima <- data.frame(
  Period = 1:60,
  Demand = c(
    110.6, 107.4, 106.1, 105.9, 103.6, 101.3, 105.8, 106.8, 108.9, 119.2,
    122.1, 134.5, 145.1, 147.9, 157.0, 160.3, 158.3, 158.5, 160.0, 165.5,
    170.3, 174.6, 181.4, 177.3, 183.9, 186.1, 190.6, 194.5, 192.1, 192.5,
    190.5, 194.9, 196.8, 198.0, 197.8, 197.1, 202.6, 199.6, 200.6, 203.3,
    209.7, 208.4, 208.8, 205.0, 205.0, 204.8, 211.8, 215.6, 217.5, 222.7,
    223.2, 224.7, 227.6, 236.0, 240.4, 243.8, 239.0, 241.8, 243.9, 241.8
  )
)

# MAD, MSE and MAPE of forecasts f against demand d (periods where f exists)
fc_errors <- function(d, f) {
  idx <- which(!is.na(f) & !is.na(d))
  e   <- f[idx] - d[idx]                       # e = F - D
  nz  <- d[idx] != 0
  list(values  = c(MAD  = mean(abs(e)),
                   MSE  = mean(e^2),
                   MAPE = mean(abs(e[nz] / d[idx][nz])) * 100),
       periods = paste0(min(idx), "\u2013", max(idx)))
}

# ---------- Initial values for the exponential smoothing methods ----------
# The user can choose how the initial level (and slope) is obtained:
#   "regression": the app fits a straight line to the first periods
#   "manual"    : the user types the values
#   "simple"    : first demand (and first difference)
fc_init_ui <- function(ns, slope = TRUE) {
  choices <- c("Fit a regression (the app estimates them)" = "regression",
               "Enter them manually" = "manual")
  choices[if (slope) "Simple: first demand and first difference" else "Simple: first demand"] <- "simple"
  tagList(
    radioButtons(ns("init"), if (slope) "Initial level and slope" else "Initial level",
                 choices = choices),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'regression'", ns("init")),
      numericInput(ns("init_n"), "Number of periods used in the regression",
                   value = 12, min = 3, step = 1)
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'manual'", ns("init")),
      numericInput(ns("init_level"), "Initial level", value = 200),
      if (slope) numericInput(ns("init_slope"), "Initial slope", value = 0)
    )
  )
}

# Returns list(level, slope, start): the state at time `start` (0 = before the first period).
# kind = "es" (level only) or "holt" (level and slope)
fc_init_values <- function(d, method = "regression", n0 = 12, level = 0, slope = 0, kind = "es") {
  T <- length(d)
  if (method == "manual") return(list(level = level, slope = slope, start = 0L))
  if (method == "regression") {
    m  <- min(max(as.integer(n0), 3L), T)
    co <- coef(lm(y ~ t, data = data.frame(y = d[1:m], t = 1:m)))
    return(list(level = unname(co[1]), slope = unname(co[2]), start = 0L))
  }
  if (kind == "es") list(level = d[1], slope = 0, start = 1L)
  else list(level = d[2], slope = d[2] - d[1], start = 2L)
}

# Reactive that reads the initial-value inputs of a module
fc_init_reactive <- function(input, d, kind) {
  reactive({
    req(input$init)
    if (input$init == "manual") req(input$init_level)
    fc_init_values(d(), input$init,
                   if (is.null(input$init_n)) 12 else input$init_n,
                   if (is.null(input$init_level)) 0 else input$init_level,
                   if (is.null(input$init_slope)) 0 else input$init_slope, kind)
  })
}

# ---------- Data input (sample data or an uploaded .csv / .xlsx file) ----------
fc_data_ui <- function(ns, label = "Data") {
  tagList(
    radioButtons(ns("source"), label,
                 choices = c("Use the sample data" = "sample",
                             "Upload my own file (.csv / .xlsx)" = "upload")),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'upload'", ns("source")),
      fileInput(ns("file"), NULL, accept = c(".csv", ".xlsx"))
    ),
    uiOutput(ns("column_ui")),
    div(style = "margin-bottom:14px;", downloadLink(ns("sample_csv"), "Download the sample CSV"))
  )
}

# Call inside moduleServer(). Returns a reactive with the demand vector.
fc_data_server <- function(input, output, session, sample_df, min_obs = 4) {
  raw <- reactive({
    if (input$source == "sample") return(sample_df)
    req(input$file)
    if (grepl("\\.xlsx$", input$file$name, ignore.case = TRUE)) {
      read_excel(input$file$datapath)
    } else {
      df <- read.csv(input$file$datapath)
      if (ncol(df) == 1) df <- read.csv2(input$file$datapath)   # ; separated files
      df
    }
  })
  output$column_ui <- renderUI({
    v <- raw()
    cols <- names(v)[sapply(v, is.numeric)]
    selectInput(session$ns("column"), "Demand column", choices = cols,
                selected = fc_pick_demand_col(cols))
  })
  output$sample_csv <- downloadHandler(
    filename = function() "sample_demand.csv",
    content  = function(file) write.csv(sample_df, file, row.names = FALSE)
  )
  reactive({
    req(input$column)
    d <- as.numeric(raw()[[input$column]]); d <- d[!is.na(d)]
    validate(need(length(d) >= min_obs,
                  paste("At least", min_obs, "periods of data are required.")))
    d
  })
}

# ---------- Standard result block (title, plot, error table, forecast table) ----------
fc_result_ui <- function(ns) {
  tagList(
    h4(textOutput(ns("title"))),
    plotOutput(ns("plot")),
    h4("Error measures"),
    tableOutput(ns("error_table")),
    h4("Forecasts by period"),
    tableOutput(ns("forecast_table"))
  )
}

# dr, fr, ur: reactives with demand, fitted forecasts (length T, NA allowed)
# and future forecasts. title: reactive with the heading text.
fc_result_server <- function(output, dr, fr, ur, title, name = "forecast") {
  output$title <- renderText(title())
  
  output$plot <- renderPlot({
    d <- dr(); f <- fr(); u <- ur(); T <- length(d); h <- length(u)
    plot(1:T, d, type = "b", pch = 16, col = "grey30", xlim = c(1, T + h),
         ylim = range(c(d, f, u), na.rm = TRUE), xlab = "Period", ylab = "Demand",
         main = paste("Actual demand and", name))
    lines(1:T, f, type = "b", pch = 17, lty = 2, col = "#0f766e")
    lines(c(T, T + seq_len(h)), c(d[T], u), type = "b", pch = 17, lty = 3, col = "#f59e0b")
    legend("topleft", bty = "n", pch = c(16, 17, 17), lty = c(1, 2, 3),
           col = c("grey30", "#0f766e", "#f59e0b"),
           legend = c("Actual demand", "Forecast", "Future forecast"))
  })
  
  output$error_table <- renderTable({
    e <- fc_errors(dr(), fr())
    data.frame(Measure = c("MAD", "MSE", "MAPE (%)"), Value = round(unname(e$values), 2),
               `Periods evaluated` = e$periods, check.names = FALSE)
  })
  
  output$forecast_table <- renderTable({
    d <- dr(); f <- fr(); u <- ur(); h <- length(u)
    data.frame(Period = 1:(length(d) + h),
               `Demand (D)` = c(round(d, 2), rep(NA, h)),
               `Forecast (F)` = round(c(f, u), 2),
               `Error (e = F - D)` = round(c(f - d, rep(NA, h)), 2),
               check.names = FALSE)
  }, na = "")
}


# ############################################################
# 2. STATIONARY SERIES
# ############################################################

# ============================================================
# MOVING AVERAGE
# ============================================================
# ---------- Helper functions ----------



# d: demand vector, n: number of past periods to average
# Returns a forecast vector of length(d) + 1
# (the last element is the next-period forecast)
fc_ma_forecast <- function(d, n) {
  T <- length(d)
  f <- rep(NA_real_, T + 1)
  for (t in (n + 1):(T + 1)) {
    f[t] <- mean(d[(t - n):(t - 1)])
  }
  f
}

# Error measures over the periods where a forecast exists (n+1 ... T)
fc_ma_errors <- function(d, f, n) {
  T   <- length(d)
  idx <- (n + 1):T
  e   <- f[idx] - d[idx]                    # e = F - D
  nz  <- d[idx] != 0                        # avoid division by zero in MAPE
  list(
    MAD  = mean(abs(e)),
    MSE  = mean(e^2),
    MAPE = mean(abs(e[nz] / d[idx][nz])) * 100,
    periods = paste0(n + 1, "\u2013", T)
  )
}

# Tries N = 1 ... nmax and returns the error measures for each N
fc_ma_simulation <- function(d, nmax = 10) {
  nmax <- min(nmax, length(d) - 2)
  res <- lapply(1:nmax, function(n) {
    h <- fc_ma_errors(d, fc_ma_forecast(d, n), n)
    data.frame(N = n, MAD = h$MAD, MSE = h$MSE, MAPE = h$MAPE)
  })
  do.call(rbind, res)
}

# ---------- User interface ----------
fc_moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns),
      radioButtons(ns("mode"), "How should N be chosen?",
                   choices = c("I will choose N" = "manual",
                               "Find the best N" = "auto")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("mode")),
        numericInput(ns("n"), "N (number of past periods)", value = 3, min = 1, step = 1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        selectInput(ns("criterion"), "Choose the best N by", choices = c("MSE", "MAPE", "MAD"))
      )
    ),
    mainPanel(
      h4(textOutput(ns("title"))),
      plotOutput(ns("plot")),
      h4("Error measures"),
      tableOutput(ns("error_table")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        h4("Simulation: errors for N = 1 to 10"),
        tableOutput(ns("sim_table"))
      ),
      h4("Forecasts by period"),
      tableOutput(ns("forecast_table"))
    )
  )
}

# ---------- Server ----------
fc_moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_data_server(input, output, session, fc_sample_demand)
    
    n_used <- reactive({
      d <- demand()
      if (input$mode == "auto") {
        req(input$criterion)
        sim <- fc_ma_simulation(d, 10)
        sim$N[which.min(sim[[input$criterion]])]
      } else {
        req(input$n)
        validate(need(input$n >= 1 && input$n < length(d),
                      "N must be between 1 and (number of periods - 1)."))
        as.integer(input$n)
      }
    })
    
    fc <- reactive(fc_ma_forecast(demand(), n_used()))
    
    output$title <- renderText({
      f <- fc()
      paste0("MA(", n_used(), ") \u2014 next-period forecast: ", round(f[length(f)], 2))
    })
    
    output$plot <- renderPlot({
      d <- demand(); f <- fc(); T <- length(d)
      plot(1:T, d, type = "b", pch = 16, col = "grey30",
           xlim = c(1, T + 1), ylim = range(c(d, f), na.rm = TRUE),
           xlab = "Period", ylab = "Demand",
           main = paste0("Actual demand and MA(", n_used(), ") forecast"))
      lines(1:(T + 1), f, type = "b", pch = 17, lty = 2, col = "steelblue")
      points(T + 1, f[T + 1], pch = 17, cex = 2, col = "orangered")
      legend("topleft", bty = "n", pch = c(16, 17, 17), lty = c(1, 2, NA),
             col = c("grey30", "steelblue", "orangered"),
             legend = c("Actual demand", "Forecast", "Next-period forecast"))
    })
    
    output$error_table <- renderTable({
      h <- fc_ma_errors(demand(), fc(), n_used())
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
                 Value = round(c(h$MAD, h$MSE, h$MAPE), 2),
                 `Periods evaluated` = h$periods, check.names = FALSE)
    })
    
    output$sim_table <- renderTable({
      sim <- fc_ma_simulation(demand(), 10)
      sim[, -1] <- round(sim[, -1], 2)
      sim$Best <- ifelse(sim$N == n_used(), "<-- best", "")
      sim
    })
    
    output$forecast_table <- renderTable({
      d <- demand(); f <- fc(); T <- length(d)
      data.frame(Period = 1:(T + 1),
                 `Demand (D)` = c(round(d, 2), NA),
                 `Forecast (F)` = round(f, 2),
                 `Error (e = F - D)` = round(f - c(d, NA), 2),
                 check.names = FALSE)
    }, na = "")
  })
}

# ============================================================
# BEST N FOR MOVING AVERAGE (normal method or with simulation)
# ============================================================


# Fast MAD / MSE / MAPE for N = 1 ... nmax (same numbers as fc_ma_simulation)
fc_ma_errors_fast <- function(d, nmax = 10) {
  T  <- length(d)
  cs <- c(0, cumsum(d))
  out <- matrix(NA_real_, nmax, 3, dimnames = list(NULL, c("MAD", "MSE", "MAPE")))
  for (n in 1:nmax) {
    t  <- (n + 1):T
    f  <- (cs[t] - cs[t - n]) / n          # mean of the previous n demands
    e  <- f - d[t]
    nz <- d[t] != 0
    out[n, ] <- c(mean(abs(e)), mean(e^2), mean(abs(e[nz] / d[t][nz])) * 100)
  }
  data.frame(N = 1:nmax, out)
}

# One simulated series: keeps the overall pattern of the data (smooth curve)
# and replaces the random noise by noise resampled from the data's own residuals.
fc_simulate_series <- function(d) {
  T      <- length(d)
  smooth <- lowess(seq_len(T), d, f = 0.4)$y
  pmax(smooth + sample(d - smooth, T, replace = TRUE), 0)
}

# method: "normal" or "simulation"; criterion: "MSE", "MAPE" or "MAD"
fc_best_n_analysis <- function(d, method = "normal", nsim = 500, criterion = "MSE",
                               seed = 123, nmax = 10) {
  nmax <- min(nmax, length(d) - 2)
  if (method == "normal") {
    tab <- fc_ma_errors_fast(d, nmax)
    share <- NULL
  } else {
    set.seed(seed)
    total <- matrix(0, nmax, 3)
    wins  <- numeric(nmax)
    for (i in seq_len(nsim)) {
      e <- fc_ma_errors_fast(fc_simulate_series(d), nmax)
      total <- total + as.matrix(e[, c("MAD", "MSE", "MAPE")])
      wins[which.min(e[[criterion]])] <- wins[which.min(e[[criterion]])] + 1
    }
    tab <- data.frame(N = 1:nmax, total / nsim)
    colnames(tab) <- c("N", "MAD", "MSE", "MAPE")
    share <- 100 * wins / nsim
  }
  best <- tab$N[which.min(tab[[criterion]])]
  list(table = tab, best_n = best, share = share, criterion = criterion,
       method = method, nsim = nsim,
       next_forecast = mean(tail(d, best)))
}

fc_best_n_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h5(strong("1 \u00b7 DATA")),
      fc_data_ui(ns, label = NULL),
      hr(),
      h5(strong("2 \u00b7 METHOD")),
      radioButtons(ns("method"), NULL,
                   choices = c("Normal Moving Average" = "normal",
                               "With simulation" = "simulation")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'simulation'", ns("method")),
        sliderInput(ns("nsim"), "Number of simulated series", min = 100, max = 2000,
                    value = 500, step = 100),
        numericInput(ns("seed"), "Random seed (same seed = same result)", value = 123)
      ),
      hr(),
      h5(strong("3 \u00b7 CHOOSE THE BEST N BY")),
      radioButtons(ns("criterion"), NULL, choices = c("MSE", "MAPE", "MAD"), inline = TRUE),
      actionButton(ns("run"), "Run analysis", class = "btn-run", width = "100%")
    ),
    mainPanel(
      uiOutput(ns("kpis")),
      tabsetPanel(
        tabPanel("Results", br(), plotOutput(ns("plot_results"), height = "380px"),
                 h4(textOutput(ns("table_title"))), tableOutput(ns("table"))),
        tabPanel("Demand & forecast", br(), plotOutput(ns("plot_forecast")),
                 h4("Forecasts by period (with the best N)"), tableOutput(ns("forecast_table"))),
        tabPanel("How it works", br(),
                 p(strong("Moving average:"), "the forecast for a period is the average of the previous N periods' demand."),
                 p(strong("Normal method:"), "tries N = 1 to 10 on your data and reports MAD, MSE and MAPE for each N.",
                   "The best N is the one with the smallest error for the measure you chose."),
                 p(strong("With simulation:"), "creates many artificial demand series that keep the pattern of your data",
                   "but with randomly re-drawn noise. For every simulated series it finds the best N, then reports the",
                   "average errors for each N and how often each N was the best (% of series). A higher share means the",
                   "N is the best choice more reliably, not only for one lucky data set."),
                 p(strong("Random seed:"), "fixes the random numbers, so the same seed always gives the same result."),
                 p(strong("Next period:"), "forecast of the first future period using the best N on your real data.")
        )
      )
    )
  )
}

fc_best_n_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_data_server(input, output, session, fc_sample_demand)
    
    result <- eventReactive(input$run, {
      d <- demand()
      withProgress(message = "Running analysis...", value = 0.5, {
        fc_best_n_analysis(d, input$method, input$nsim, input$criterion, input$seed)
      })
    })
    
    output$kpis <- renderUI({
      if (input$run == 0) return(p(em("Choose your settings and press 'Run analysis'.")))
      r <- result(); b <- r$table[r$table$N == r$best_n, ]
      kpi <- function(lab, val, cls = "") div(class = paste("kpi", cls), div(class = "lab", lab), div(class = "val", val))
      tagList(
        p(span(style = "background:#fef3c7;color:#92400e;padding:2px 10px;border-radius:10px;font-size:12px;font-weight:600;",
               if (r$method == "normal") "Normal method" else paste0("Simulation \u2014 ", r$nsim, " series"))),
        kpi(paste("Best N by", r$criterion), r$best_n, "best"),
        kpi("MSE", round(b$MSE, 1)), kpi("MAPE", paste0(round(b$MAPE, 2), "%")),
        kpi("MAD", round(b$MAD, 2)),
        kpi(paste0("Next period (N=", r$best_n, ")"), round(r$next_forecast, 1))
      )
    })
    
    output$plot_results <- renderPlot({
      r <- result(); tab <- r$table
      cols <- ifelse(tab$N == r$best_n, "#0f766e", "#b7dcd6")
      if (!is.null(r$share)) par(mfrow = c(1, 2))
      barplot(tab[[r$criterion]], names.arg = tab$N, col = cols, border = NA, xlab = "N",
              ylab = paste(r$criterion, if (!is.null(r$share)) "(average)" else ""),
              main = paste0(r$criterion, " by N \u2014 best N = ", r$best_n))
      if (!is.null(r$share)) {
        barplot(r$share, names.arg = tab$N, col = cols, border = NA, xlab = "N",
                ylab = "% of simulated series",
                main = paste0("How often each N was best (", r$criterion, ")"))
      }
    })
    
    output$table_title <- renderText({
      r <- result()
      if (is.null(r$share)) "N = 1, ..., 10" else "N = 1, ..., 10 (average over the simulated series)"
    })
    
    output$table <- renderTable({
      r <- result(); tab <- r$table
      out <- data.frame(N = tab$N, MAD = round(tab$MAD, 2), MSE = round(tab$MSE, 2), MAPE = round(tab$MAPE, 2))
      if (!is.null(r$share)) out[["Best in (% of series)"]] <- round(r$share, 1)
      out$Best <- ifelse(tab$N == r$best_n, "<-- best N", "")
      out
    }, digits = 2)
    
    output$plot_forecast <- renderPlot({
      r <- result(); d <- demand(); n <- r$best_n; f <- fc_ma_forecast(d, n); T <- length(d)
      plot(1:T, d, type = "b", pch = 16, col = "grey30", xlim = c(1, T + 1),
           ylim = range(c(d, f), na.rm = TRUE), xlab = "Period", ylab = "Demand",
           main = paste0("Actual demand and MA(", n, ") forecast"))
      lines(1:(T + 1), f, type = "b", pch = 17, lty = 2, col = "#0f766e")
      points(T + 1, f[T + 1], pch = 17, cex = 2, col = "#f59e0b")
      legend("topleft", bty = "n", pch = c(16, 17, 17), lty = c(1, 2, NA),
             col = c("grey30", "#0f766e", "#f59e0b"),
             legend = c("Actual demand", "Forecast", "Next-period forecast"))
    })
    
    output$forecast_table <- renderTable({
      r <- result(); d <- demand(); f <- fc_ma_forecast(d, r$best_n); T <- length(d)
      data.frame(Period = 1:(T + 1), `Demand (D)` = c(round(d, 2), NA),
                 `Forecast (F)` = round(f, 2), `Error (e = F - D)` = round(f - c(d, NA), 2),
                 check.names = FALSE)
    }, na = "")
  })
}

# ============================================================
# WEIGHTED MOVING AVERAGE
# ============================================================

# w: weights, most recent period first (w[1] multiplies D_(t-1)), sum(w) = 1
fc_wma_forecast <- function(d, w) {
  n <- length(w); T <- length(d)
  f <- rep(NA_real_, T)
  for (t in (n + 1):T) f[t] <- sum(w * d[(t - 1):(t - n)])
  list(fitted = f, future = sum(w * d[T:(T - n + 1)]))
}

# All sets of n non-negative weights (multiples of `step`) that sum to 1; one set per row
fc_wma_grid <- function(n, step) {
  k <- round(1 / step)
  rec <- function(n, k) {
    if (n == 1) return(matrix(k, nrow = 1))
    do.call(cbind, lapply(0:k, function(i) rbind(i, rec(n - 1, k - i))))
  }
  t(rec(n, k)) / k
}

# Searches the weights with the smallest error
fc_wma_best <- function(d, n, criterion = "MSE") {
  grid <- fc_wma_grid(n, if (n <= 4) 0.05 else 0.1)
  v <- apply(grid, 1, function(w) fc_errors(d, fc_wma_forecast(d, w)$fitted)$values[[criterion]])
  list(weights = grid[which.min(v), ], value = min(v))
}

fc_wma_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns), hr(),
      radioButtons(ns("mode"), "How should the weights be chosen?",
                   choices = c("I will enter the weights" = "manual",
                               "Find the best weights" = "auto")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("mode")),
        textInput(ns("weights"), "Weights (most recent period first)", value = "0.5, 0.3, 0.2"),
        helpText("Separate the weights with commas. They are rescaled so that they sum to 1.")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        numericInput(ns("nw"), "Number of weights", value = 3, min = 2, max = 6, step = 1),
        selectInput(ns("criterion"), "Minimize", choices = c("MSE", "MAPE", "MAD"))
      )
    ),
    mainPanel(fc_result_ui(ns))
  )
}

fc_wma_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    d <- fc_data_server(input, output, session, fc_sample_demand)
    w <- reactive({
      if (input$mode == "auto") {
        req(input$nw, input$criterion)
        n <- as.integer(input$nw)
        validate(need(n >= 2 && n <= 6 && n < length(d()), "Use between 2 and 6 weights (fewer than the number of periods)."))
        fc_wma_best(d(), n, input$criterion)$weights
      } else {
        x <- suppressWarnings(as.numeric(strsplit(gsub(";", ",", input$weights), ",")[[1]]))
        x <- x[!is.na(x)]
        validate(need(length(x) >= 1 && all(x >= 0) && sum(x) > 0,
                      "Enter non-negative weights, for example 0.5, 0.3, 0.2."))
        validate(need(length(x) < length(d()),
                      "The number of weights must be smaller than the number of periods."))
        x / sum(x)
      }
    })
    res <- reactive(fc_wma_forecast(d(), w()))
    fc_result_server(output, d, reactive(res()$fitted), reactive(res()$future),
                     title = reactive(paste0("WMA(", length(w()), ") \u2014 weights ", paste(round(w(), 2), collapse = ", "),
                                             " \u2014 next-period forecast: ", round(res()$future, 2))),
                     name = "weighted moving average forecast")
  })
}

# ============================================================
# SIMPLE EXPONENTIAL SMOOTHING
# ============================================================

# S_t = alpha * D_t + (1 - alpha) * S_(t-1) ; the forecast for period t is S_(t-1).
# init: list(level, start) from fc_init_values()
fc_es_forecast <- function(d, a, init) {
  T <- length(d); f <- rep(NA_real_, T)
  S <- init$level
  for (t in (init$start + 1):T) {
    f[t] <- S
    S <- a * d[t] + (1 - a) * S
  }
  list(fitted = f, future = S)
}

# Tries alpha = 0.01 ... 0.99 and returns the one with the smallest error
fc_es_best <- function(d, init, criterion = "MSE", step = 0.01) {
  grid <- seq(step, 1 - step, by = step)
  v <- sapply(grid, function(a) fc_errors(d, fc_es_forecast(d, a, init)$fitted)$values[[criterion]])
  list(alpha = grid[which.min(v)], value = min(v))
}

fc_exp_smoothing_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns), hr(),
      radioButtons(ns("mode"), "How should alpha be chosen?",
                   choices = c("I will choose alpha" = "manual", "Find the best alpha" = "auto")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("mode")),
        sliderInput(ns("alpha"), "Alpha", min = 0.01, max = 0.99, value = 0.3, step = 0.01)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        selectInput(ns("criterion"), "Minimize", choices = c("MSE", "MAPE", "MAD"))
      ),
      hr(),
      fc_init_ui(ns, slope = FALSE)
    ),
    mainPanel(fc_result_ui(ns))
  )
}

fc_exp_smoothing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    d <- fc_data_server(input, output, session, fc_sample_demand)
    init <- fc_init_reactive(input, d, "es")
    alpha <- reactive({
      if (input$mode == "auto") { req(input$criterion); fc_es_best(d(), init(), input$criterion)$alpha }
      else input$alpha
    })
    res <- reactive(fc_es_forecast(d(), alpha(), init()))
    fc_result_server(output, d, reactive(res()$fitted), reactive(res()$future),
                     title = reactive(paste0("Simple exponential smoothing: alpha = ", alpha(),
                                             " \u2014 next-period forecast: ", round(res()$future, 2))),
                     name = "exponential smoothing forecast")
  })
}

# ============================================================
# SEASONAL FACTORS (quick and dirty method)
# ============================================================

# For every complete cycle: divide each demand by the average demand of its cycle.
# The seasonal factor of a season is the average of these ratios over all cycles.
fc_qd_factors <- function(d, L) {
  ncyc <- floor(length(d) / L)
  m <- matrix(d[1:(ncyc * L)], nrow = L)               # one column per cycle
  ratios <- sweep(m, 2, colMeans(m), "/")
  fac <- rowMeans(ratios)
  list(factors = fac / mean(fac), ratios = ratios, ncycles = ncyc)
}

# 1) seasonal factors (quick and dirty method)   2) deseasonalize: D / factor
# 3) forecast the deseasonalized series with simple exponential smoothing
# 4) reseasonalize: forecast * factor of the period being forecast
# spec: list(method, n0, level) = how the initial level of the smoothing is obtained
fc_sf_forecast <- function(d, L, a, h, spec) {
  T <- length(d)
  fac <- fc_qd_factors(d, L)$factors
  pos <- function(t) ((t - 1) %% L) + 1
  cT <- fac[pos(seq_len(T))]
  ds <- d / cT
  init <- fc_init_values(ds, spec$method, spec$n0, spec$level, 0, "es")
  es <- fc_es_forecast(ds, a, init)
  list(fitted = es$fitted * cT,
       future = es$future * fac[pos(T + seq_len(h))],
       factors = fac, level = es$future, ds = ds, init = init)
}

fc_sf_best <- function(d, L, spec, criterion = "MSE", step = 0.01) {
  grid <- seq(step, 1 - step, by = step)
  v <- sapply(grid, function(a) fc_errors(d, fc_sf_forecast(d, L, a, 1, spec)$fitted)$values[[criterion]])
  list(alpha = grid[which.min(v)], value = min(v))
}

fc_seasonal_factors_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns), hr(),
      numericInput(ns("L"), "Season length (12 = monthly, 4 = quarterly)", value = 12, min = 2, step = 1),
      radioButtons(ns("mode"), "How should alpha be chosen?",
                   choices = c("I will choose alpha" = "manual", "Find the best alpha" = "auto")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("mode")),
        sliderInput(ns("alpha"), "Alpha", min = 0.01, max = 0.99, value = 0.3, step = 0.01)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        selectInput(ns("criterion"), "Minimize", choices = c("MSE", "MAPE", "MAD"))
      ),
      hr(),
      fc_init_ui(ns, slope = FALSE),
      helpText("The initial level refers to the deseasonalized series."),
      numericInput(ns("h"), "Periods ahead to forecast", value = 12, min = 1, max = 36, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast", br(), fc_result_ui(ns),
                 h4("Seasonal factors used"), tableOutput(ns("factor_table"))),
        tabPanel("Seasonal factors", br(),
                 h4(textOutput(ns("qd_title"))),
                 plotOutput(ns("plot_factors"), height = "300px"),
                 h4("Seasonal factors and the ratios of every cycle"),
                 tableOutput(ns("ratio_table")),
                 h4("Actual and deseasonalized demand"),
                 plotOutput(ns("plot_deseason"), height = "300px"),
                 p(strong("How it works:"), "for every full cycle (for example a year) the average demand is computed.",
                   "Each period's demand is divided by the average of its own cycle. The seasonal factor of a",
                   "season (for example January) is the average of these ratios over all cycles.",
                   "A factor above 1 means the season is stronger than average, below 1 weaker than average.",
                   "The deseasonalized demand is then smoothed and multiplied by the factor of the forecasted period."))
      )
    )
  )
}

fc_seasonal_factors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    d <- fc_data_server(input, output, session, fc_sample_seasonal_flat, min_obs = 6)
    L <- reactive({
      req(input$L); L <- as.integer(input$L)
      validate(need(L >= 2 && length(d()) >= 2 * L,
                    "At least two full seasons of data are needed (periods >= 2 x season length)."))
      L
    })
    spec <- reactive({
      req(input$init)
      if (input$init == "manual") req(input$init_level)
      list(method = input$init,
           n0 = if (is.null(input$init_n)) 12 else input$init_n,
           level = if (is.null(input$init_level)) 0 else input$init_level)
    })
    alpha <- reactive({
      if (input$mode == "auto") { req(input$criterion); fc_sf_best(d(), L(), spec(), input$criterion)$alpha }
      else input$alpha
    })
    res <- reactive({ req(input$h); fc_sf_forecast(d(), L(), alpha(), as.integer(input$h), spec()) })
    fc_result_server(output, d, reactive(res()$fitted), reactive(res()$future),
                     title = reactive(paste0("Seasonal factors + exponential smoothing: alpha = ", alpha(),
                                             " \u2014 next-period forecast: ", round(res()$future[1], 2))),
                     name = "forecast with seasonal factors")
    output$factor_table <- renderTable({
      data.frame(Season = seq_len(L()), `Seasonal factor` = round(res()$factors, 3), check.names = FALSE)
    }, digits = 3)
    q <- reactive(fc_qd_factors(d(), L()))
    output$qd_title <- renderText(paste0("Quick and dirty seasonal factors \u2014 ", q()$ncycles,
                                         " full cycles of length ", L()))
    output$plot_factors <- renderPlot({
      f <- q()$factors
      barplot(f, names.arg = seq_along(f), col = ifelse(f >= 1, "#0f766e", "#b7dcd6"), border = NA,
              xlab = "Season", ylab = "Seasonal factor", main = "Seasonal factors")
      abline(h = 1, lty = 2, col = "grey40")
    })
    output$ratio_table <- renderTable({
      r <- q()$ratios
      out <- data.frame(Season = seq_len(L()), `Seasonal factor` = round(q()$factors, 3), check.names = FALSE)
      for (j in seq_len(ncol(r))) out[[paste("Cycle", j, "ratio")]] <- round(r[, j], 3)
      out
    }, digits = 3)
    output$plot_deseason <- renderPlot({
      dd <- d(); T <- length(dd); ds <- res()$ds
      plot(seq_len(T), dd, type = "b", pch = 16, col = "grey30", ylim = range(c(dd, ds)),
           xlab = "Period", ylab = "Demand", main = "Actual vs deseasonalized demand")
      lines(seq_len(T), ds, type = "b", pch = 17, col = "#0f766e")
      legend("topleft", bty = "n", pch = c(16, 17), lty = 1, col = c("grey30", "#0f766e"),
             legend = c("Actual demand", "Deseasonalized demand"))
    })
  })
}

# ============================================================
# STATIONARY SERIES: menu
# ============================================================
fc_stationary_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Moving Average", fc_moving_average_ui(ns("ma"))),
    tabPanel("Best N (MA)", fc_best_n_ui(ns("best_n"))),
    tabPanel("Weighted Moving Average", fc_wma_ui(ns("wma"))),
    tabPanel("Simple Exponential Smoothing", fc_exp_smoothing_ui(ns("es"))),
    tabPanel("Seasonal Factors (Quick and Dirty)", fc_seasonal_factors_ui(ns("sf")))
  )
}

fc_stationary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    fc_moving_average_server("ma")
    fc_best_n_server("best_n")
    fc_wma_server("wma")
    fc_exp_smoothing_server("es")
    fc_seasonal_factors_server("sf")
  })
}


# ############################################################
# 3. TREND SERIES
# ############################################################

# ============================================================
# HOLT'S METHOD (double exponential smoothing)
# ============================================================

# L_t = alpha * D_t + (1 - alpha) * (L_(t-1) + T_(t-1))
# T_t = beta * (L_t - L_(t-1)) + (1 - beta) * T_(t-1)
# The forecast for period t is L_(t-1) + T_(t-1).
# init: list(level, slope, start) from fc_init_values()
fc_holt_forecast <- function(d, a, b, init, h = 1) {
  T <- length(d)
  L <- init$level; Tr <- init$slope
  f <- rep(NA_real_, T); Ls <- rep(NA_real_, T); Ts <- rep(NA_real_, T)
  if (init$start > 0) { Ls[init$start] <- L; Ts[init$start] <- Tr }
  for (t in (init$start + 1):T) {
    f[t] <- L + Tr
    Ln <- a * d[t] + (1 - a) * (L + Tr)
    Tn <- b * (Ln - L) + (1 - b) * Tr
    L <- Ln; Tr <- Tn
    Ls[t] <- L; Ts[t] <- Tr
  }
  list(f = f, L = Ls, Tr = Ts, future = L + (1:h) * Tr)     # F_(T+k) = L_T + k * T_T
}

# Tries alpha and beta from 0.01 to 0.99 and returns the best pair
fc_holt_best <- function(d, init, criterion = "MSE", step = 0.01) {
  grid <- seq(step, 1 - step, by = step)
  best <- Inf; a_best <- NA; b_best <- NA
  for (a in grid) for (b in grid) {
    v <- fc_errors(d, fc_holt_forecast(d, a, b, init)$f)$values[[criterion]]
    if (is.finite(v) && v < best) { best <- v; a_best <- a; b_best <- b }
  }
  list(alpha = a_best, beta = b_best, value = best)
}

fc_holt_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns),
      radioButtons(ns("mode"), "How should alpha and beta be chosen?",
                   choices = c("I will choose" = "manual",
                               "Find the best alpha and beta" = "auto")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("mode")),
        sliderInput(ns("alpha"), "Alpha (level)", min = 0.01, max = 0.99, value = 0.3, step = 0.01),
        sliderInput(ns("beta"),  "Beta (trend)",  min = 0.01, max = 0.99, value = 0.2, step = 0.01)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        selectInput(ns("criterion"), "Minimize", choices = c("MSE", "MAPE", "MAD"))
      ),
      hr(),
      fc_init_ui(ns, slope = TRUE),
      numericInput(ns("horizon"), "Periods ahead to forecast", value = 3, min = 1, max = 24, step = 1)
    ),
    mainPanel(
      h4(textOutput(ns("title"))),
      plotOutput(ns("plot")),
      h4("Error measures"),
      tableOutput(ns("error_table")),
      h4("Future forecasts"),
      tableOutput(ns("future_table")),
      h4("Calculations by period"),
      tableOutput(ns("detail_table"))
    )
  )
}

fc_holt_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    demand <- fc_data_server(input, output, session, fc_sample_demand)
    init <- fc_init_reactive(input, demand, "holt")
    
    params <- reactive({
      if (input$mode == "auto") { req(input$criterion); fc_holt_best(demand(), init(), input$criterion) }
      else list(alpha = input$alpha, beta = input$beta)
    })
    
    res <- reactive({ req(input$horizon); fc_holt_forecast(demand(), params()$alpha, params()$beta, init(), input$horizon) })
    
    output$title <- renderText({
      p <- params()
      paste0("Holt: alpha = ", p$alpha, ", beta = ", p$beta,
             " \u2014 next-period forecast: ", round(res()$future[1], 2))
    })
    
    output$plot <- renderPlot({
      d <- demand(); s <- res(); T <- length(d); h <- length(s$future)
      plot(1:T, d, type = "b", pch = 16, col = "grey30",
           xlim = c(1, T + h), ylim = range(c(d, s$f, s$future), na.rm = TRUE),
           xlab = "Period", ylab = "Demand", main = "Actual demand and Holt forecast")
      lines(1:T, s$f, type = "b", pch = 17, lty = 2, col = "#0f766e")
      lines(c(T, T + (1:h)), c(d[T], s$future), type = "b", pch = 17, lty = 3, col = "#f59e0b")
      legend("topleft", bty = "n", pch = c(16, 17, 17), lty = c(1, 2, 3),
             col = c("grey30", "#0f766e", "#f59e0b"),
             legend = c("Actual demand", "Forecast", "Future forecast"))
    })
    
    output$error_table <- renderTable({
      e <- fc_errors(demand(), res()$f)
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"), Value = round(unname(e$values), 2),
                 `Periods evaluated` = e$periods, check.names = FALSE)
    })
    
    output$future_table <- renderTable({
      data.frame(Period = length(demand()) + seq_along(res()$future), Forecast = round(res()$future, 2))
    })
    
    output$detail_table <- renderTable({
      d <- demand(); s <- res()
      data.frame(Period = seq_along(d), `Demand (D)` = round(d, 2), Level = round(s$L, 2),
                 Trend = round(s$Tr, 2), `Forecast (F)` = round(s$f, 2),
                 `Error (e = F - D)` = round(s$f - d, 2), check.names = FALSE)
    }, na = "")
  })
}

# ============================================================
# TREND SERIES: menu
# ============================================================
fc_trend_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Holt's Method", fc_holt_ui(ns("holt")))
  )
}

fc_trend_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    fc_holt_server("holt")
  })
}


# ############################################################
# 4. SEASONAL SERIES
# ############################################################

# ============================================================
# SEASONAL DECOMPOSITION (centered moving average)
# ============================================================

# 1) centered moving average -> trend   2) ratio (or difference) to trend -> seasonal factors
# 3) straight line fitted to the trend   4) forecast = trend line x seasonal factor
fc_decomp_forecast <- function(d, L, type = "multiplicative", h = 1) {
  T <- length(d); t_idx <- seq_len(T)
  dec <- decompose(ts(d, frequency = L), type = type)
  tr  <- as.numeric(dec$trend); ok <- !is.na(tr)
  fit <- lm(y ~ t, data = data.frame(y = tr[ok], t = t_idx[ok]))
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])
  s <- as.numeric(dec$figure)
  pos <- function(t) ((t - 1) %% L) + 1
  comb <- function(t) if (type == "multiplicative") (a + b * t) * s[pos(t)] else (a + b * t) + s[pos(t)]
  list(fitted = comb(t_idx), future = comb(T + seq_len(h)), dec = dec,
       intercept = a, slope = b, seasonal = s)
}

fc_decomp_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns), hr(),
      numericInput(ns("L"), "Season length (12 = monthly, 4 = quarterly)", value = 12, min = 2, step = 1),
      radioButtons(ns("type"), "Seasonal pattern",
                   choices = c("Multiplicative (factors)" = "multiplicative",
                               "Additive (amounts)" = "additive")),
      numericInput(ns("h"), "Periods ahead to forecast", value = 12, min = 1, max = 36, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast", br(), fc_result_ui(ns)),
        tabPanel("Decomposition", br(),
                 plotOutput(ns("components"), height = "480px"),
                 h4("Trend line and seasonal factors"),
                 textOutput(ns("trend_text")),
                 tableOutput(ns("seasonal_table")))
      )
    )
  )
}

fc_decomp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    d <- fc_data_server(input, output, session, fc_sample_seasonal, min_obs = 6)
    L <- reactive({
      req(input$L); L <- as.integer(input$L)
      validate(need(L >= 2 && length(d()) >= 2 * L,
                    "At least two full seasons of data are needed (periods >= 2 x season length)."))
      L
    })
    res <- reactive({ req(input$h, input$type); fc_decomp_forecast(d(), L(), input$type, as.integer(input$h)) })
    fc_result_server(output, d, reactive(res()$fitted), reactive(res()$future),
                     title = reactive(paste0("Seasonal decomposition (MA) \u2014 next-period forecast: ",
                                             round(res()$future[1], 2))),
                     name = "decomposition forecast")
    output$components <- renderPlot(plot(res()$dec))
    output$trend_text <- renderText(paste0("Trend line: ", round(res()$intercept, 2),
                                           " + ", round(res()$slope, 3), " x period"))
    output$seasonal_table <- renderTable({
      data.frame(Season = seq_len(L()),
                 `Seasonal factor` = round(res()$seasonal, 3), check.names = FALSE)
    }, digits = 3)
  })
}

# ============================================================
# WINTERS' METHOD (multiplicative seasonality + trend)
# ============================================================

# Initial values:
#  - seasonal factors: quick and dirty method
#  - level and slope (of the deseasonalized series), chosen by the user:
#      "regression": a straight line fitted to the first two seasons (intercept and slope)
#      "manual"    : typed by the user
#      "simple"    : first demand and the difference of the first two demands
fc_winters_prep <- function(d, L, init = "regression", level = 0, slope = 0) {
  T <- length(d)
  pos <- function(t) ((t - 1) %% L) + 1
  fac <- fc_qd_factors(d, L)$factors
  ds  <- d / fac[pos(seq_len(T))]
  if (init == "manual") {
    S0 <- level; G0 <- slope
  } else if (init == "regression") {
    m <- min(T, 2 * L)
    co <- coef(lm(y ~ t, data = data.frame(y = ds[1:m], t = 1:m)))
    S0 <- unname(co[1]); G0 <- unname(co[2])
  } else {
    S0 <- ds[1]; G0 <- ds[2] - ds[1]
  }
  list(factors = fac, S0 = S0, G0 = G0, L = L, skip_first = (init == "simple"))
}

fc_winters_core <- function(d, p, a, b, g, h = 1) {
  T <- length(d); L <- p$L
  pos <- function(t) ((t - 1) %% L) + 1
  S <- p$S0; G <- p$G0; cf <- p$factors
  f <- rep(NA_real_, T)
  for (t in seq_len(T)) {
    k <- pos(t); ck <- cf[k]
    f[t]  <- (S + G) * ck                              # F_t
    Sn    <- a * d[t] / ck + (1 - a) * (S + G)         # level
    G     <- b * (Sn - S) + (1 - b) * G                # trend
    cf[k] <- g * d[t] / Sn + (1 - g) * ck              # seasonal factor
    S     <- Sn
  }
  if (isTRUE(p$skip_first)) f[1] <- NA_real_           # the first forecast is not meaningful here
  list(fitted = f, future = (S + (1:h) * G) * cf[pos(T + 1:h)],
       level = S, slope = G, final_factors = cf)
}

# Tries alpha, beta, gamma = 0.05, 0.10, ..., 0.95
fc_winters_best <- function(d, p, criterion = "MSE", step = 0.05) {
  grid <- seq(step, 1 - step, by = step)
  best <- Inf; par <- c(NA, NA, NA)
  for (a in grid) for (b in grid) for (g in grid) {
    v <- fc_errors(d, fc_winters_core(d, p, a, b, g)$fitted)$values[[criterion]]
    if (is.finite(v) && v < best) { best <- v; par <- c(a, b, g) }
  }
  list(alpha = par[1], beta = par[2], gamma = par[3], value = best)
}

fc_winters_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns), hr(),
      numericInput(ns("L"), "Season length (12 = monthly, 4 = quarterly)", value = 12, min = 2, step = 1),
      radioButtons(ns("mode"), "How should alpha, beta and gamma be chosen?",
                   choices = c("I will choose" = "manual", "Find the best combination" = "auto")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("mode")),
        sliderInput(ns("alpha"), "Alpha (level)",    min = 0.01, max = 0.99, value = 0.3, step = 0.01),
        sliderInput(ns("beta"),  "Beta (trend)",     min = 0.01, max = 0.99, value = 0.1, step = 0.01),
        sliderInput(ns("gamma"), "Gamma (seasonal)", min = 0.01, max = 0.99, value = 0.3, step = 0.01)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        selectInput(ns("criterion"), "Minimize", choices = c("MSE", "MAPE", "MAD"))
      ),
      hr(),
      radioButtons(ns("init"), "Initial level and slope",
                   choices = c("Fit a regression (the app estimates them)" = "regression",
                               "Enter them manually" = "manual",
                               "Simple: first demand and first difference" = "simple")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("init")),
        numericInput(ns("init_level"), "Initial level (deseasonalized)", value = 200),
        numericInput(ns("init_slope"), "Initial slope (deseasonalized)", value = 0)
      ),
      numericInput(ns("h"), "Periods ahead to forecast", value = 12, min = 1, max = 36, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast", br(), fc_result_ui(ns)),
        tabPanel("Initial values and factors", br(),
                 h4("Initial values"), tableOutput(ns("init_table")),
                 h4("Seasonal factors"), tableOutput(ns("factor_table")),
                 p(strong("How the initial values are set:"),
                   "the seasonal factors come from the quick and dirty method. With the regression option a",
                   "straight line is fitted to the deseasonalized demand of the first two seasons and its intercept",
                   "and slope are the initial level and slope. With the manual option you type them (on the",
                   "deseasonalized scale). With the simple option the initial level is the first deseasonalized",
                   "demand and the initial slope is the difference between the first two deseasonalized demands."))
      )
    )
  )
}

fc_winters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    d <- fc_data_server(input, output, session, fc_sample_seasonal, min_obs = 6)
    prep <- reactive({
      req(input$L, input$init); L <- as.integer(input$L)
      validate(need(L >= 2 && length(d()) >= 2 * L,
                    "At least two full seasons of data are needed (periods >= 2 x season length)."))
      if (input$init == "manual") req(input$init_level)
      fc_winters_prep(d(), L, input$init,
                      if (is.null(input$init_level)) 0 else input$init_level,
                      if (is.null(input$init_slope)) 0 else input$init_slope)
    })
    pars <- reactive({
      if (input$mode == "auto") {
        req(input$criterion)
        withProgress(message = "Searching the best alpha, beta and gamma...", value = 0.5,
                     fc_winters_best(d(), prep(), input$criterion))
      } else list(alpha = input$alpha, beta = input$beta, gamma = input$gamma)
    })
    res <- reactive({ req(input$h); p <- pars(); fc_winters_core(d(), prep(), p$alpha, p$beta, p$gamma, as.integer(input$h)) })
    fc_result_server(output, d, reactive(res()$fitted), reactive(res()$future),
                     title = reactive({ p <- pars()
                     paste0("Winters: alpha = ", p$alpha, ", beta = ", p$beta, ", gamma = ", p$gamma,
                            " \u2014 next-period forecast: ", round(res()$future[1], 2)) }),
                     name = "Winters forecast")
    output$init_table <- renderTable({
      data.frame(Item = c("Initial level", "Initial slope"),
                 Value = round(c(prep()$S0, prep()$G0), 3))
    }, digits = 3)
    output$factor_table <- renderTable({
      data.frame(Season = seq_len(prep()$L), `Initial factor` = round(prep()$factors, 3),
                 `Final factor` = round(res()$final_factors, 3), check.names = FALSE)
    }, digits = 3)
  })
}

# ============================================================
# SEASONAL SERIES: menu
# ============================================================
fc_seasonal_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Seasonal Decomposition (MA)", fc_decomp_ui(ns("decomp"))),
    tabPanel("Winters' Method", fc_winters_ui(ns("winters")))
  )
}

fc_seasonal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    fc_decomp_server("decomp")
    fc_winters_server("winters")
  })
}

# ############################################################
# 5. BOX-JENKINS (AR, MA, ARMA, ARIMA)
# ############################################################

# Label of a fitted model, for example "AR(2)", "ARMA(1,1)" or "ARIMA(1,1,0)(0,1,0)[12]"
fc_bj_label <- function(fit, kind) {
  a <- fit$arma
  p <- a[1]; q <- a[2]; P <- a[3]; Q <- a[4]; m <- a[5]; d <- a[6]; D <- a[7]
  switch(kind,
         AR   = paste0("AR(", p, ")"),
         MA   = paste0("MA(", q, ")"),
         ARMA = paste0("ARMA(", p, ",", q, ")"),
         {
           s <- paste0("ARIMA(", p, ",", d, ",", q, ")")
           if (m > 1 && (P + D + Q) > 0) s <- paste0(s, "(", P, ",", D, ",", Q, ")[", m, "]")
           s
         })
}

# Estimates the model. mode = "manual" uses the orders given by the user,
# mode = "auto" searches for the best model by `criterion` ("aicc", "aic" or "bic").
fc_bj_estimate <- function(y, kind, mode, p = 1, d = 0, q = 1, P = 0, D = 0, Q = 0,
                           period = 1, constant = TRUE, criterion = "aicc") {
  fit_one <- function(p, d, q, P = 0, D = 0, Q = 0) {
    forecast::Arima(y, order = c(p, d, q),
                    seasonal = list(order = c(P, D, Q), period = max(period, 1)),
                    include.mean  = constant && (d + D) == 0,
                    include.drift = constant && (d + D) == 1)
  }
  if (mode == "manual") {
    return(switch(kind,
                  AR   = fit_one(p, 0, 0),
                  MA   = fit_one(0, 0, q),
                  ARMA = fit_one(p, 0, q),
                  fit_one(p, d, q, P, D, Q)))
  }
  if (kind == "ARIMA") {
    return(forecast::auto.arima(y, ic = criterion, allowdrift = constant, allowmean = constant))
  }
  grid <- switch(kind,
                 AR   = data.frame(p = 1:6, q = 0),
                 MA   = data.frame(p = 0, q = 1:6),
                 expand.grid(p = 1:4, q = 1:4))
  best <- NULL; best_v <- Inf
  for (i in seq_len(nrow(grid))) {
    fit <- tryCatch(fit_one(grid$p[i], 0, grid$q[i]), error = function(e) NULL)
    if (is.null(fit)) next
    v <- fit[[criterion]]
    if (is.finite(v) && v < best_v) { best <- fit; best_v <- v }
  }
  if (is.null(best)) stop("none of the candidate models could be estimated")
  best
}

fc_bj_ui <- function(id, kind) {
  ns <- NS(id)
  manual <- switch(kind,
                   AR = tagList(
                     numericInput(ns("p"), "AR order (p)", value = 1, min = 1, max = 10, step = 1)),
                   MA = tagList(
                     numericInput(ns("q"), "MA order (q)", value = 1, min = 1, max = 10, step = 1)),
                   ARMA = tagList(
                     numericInput(ns("p"), "AR order (p)", value = 1, min = 1, max = 10, step = 1),
                     numericInput(ns("q"), "MA order (q)", value = 1, min = 1, max = 10, step = 1)),
                   ARIMA = tagList(
                     numericInput(ns("p"), "AR order (p)", value = 1, min = 0, max = 10, step = 1),
                     numericInput(ns("d"), "Differencing (d)", value = 1, min = 0, max = 2, step = 1),
                     numericInput(ns("q"), "MA order (q)", value = 1, min = 0, max = 10, step = 1),
                     conditionalPanel(
                       condition = sprintf("input['%s'] > 1", ns("period")),
                       numericInput(ns("P"), "Seasonal AR order (P)", value = 0, min = 0, max = 3, step = 1),
                       numericInput(ns("D"), "Seasonal differencing (D)", value = 0, min = 0, max = 1, step = 1),
                       numericInput(ns("Q"), "Seasonal MA order (Q)", value = 0, min = 0, max = 3, step = 1))))
  search_note <- switch(kind,
                        AR   = "Tries p = 1 to 6 and keeps the best model.",
                        MA   = "Tries q = 1 to 6 and keeps the best model.",
                        ARMA = "Tries p = 1 to 4 and q = 1 to 4 and keeps the best model.",
                        "Uses auto.arima to search p, d and q (and the seasonal terms if the seasonal period is larger than 1).")
  sidebarLayout(
    sidebarPanel(
      fc_data_ui(ns), hr(),
      if (kind == "ARIMA")
        numericInput(ns("period"), "Seasonal period (1 = no seasonality, 12 = monthly, 4 = quarterly)",
                     value = 1, min = 1, step = 1),
      radioButtons(ns("mode"), "How should the orders be chosen?",
                   choices = c("I will enter the orders" = "manual", "Find the best model" = "auto")),
      conditionalPanel(condition = sprintf("input['%s'] == 'manual'", ns("mode")), manual),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("mode")),
        selectInput(ns("criterion"), "Choose the best model by",
                    choices = c("AICc" = "aicc", "AIC" = "aic", "BIC" = "bic")),
        helpText(search_note)),
      checkboxInput(ns("const"), "Include a constant (mean / drift)", value = TRUE),
      numericInput(ns("h"), "Periods ahead to forecast", value = 12, min = 1, max = 60, step = 1),
      actionButton(ns("run"), "Fit the model", class = "btn-run", width = "100%")
    ),
    mainPanel(
      uiOutput(ns("kpis")),
      tabsetPanel(
        tabPanel("Time series plot", br(), plotOutput(ns("plot_ts"), height = "380px")),
        tabPanel("ACF and PACF", br(), plotOutput(ns("plot_acf"), height = "480px"),
                 p("The ACF shows how strongly each period is correlated with earlier periods. The PACF shows the",
                   "direct correlation at each lag after removing the effect of the shorter lags. Bars outside the",
                   "dashed blue lines are statistically significant. For a model with differencing the plots of the",
                   "differenced series are shown as well.")),
        tabPanel("Parameter estimates", br(),
                 h4("Parameter estimates"), tableOutput(ns("param_table")),
                 h4("Model statistics"), tableOutput(ns("stat_table")),
                 h4("Error measures (in-sample fit)"), tableOutput(ns("error_table")),
                 plotOutput(ns("plot_fit"), height = "320px"),
                 plotOutput(ns("plot_resid"), height = "300px")),
        tabPanel("Forecast", br(), plotOutput(ns("plot_fc"), height = "380px"),
                 h4("Forecasts with 80% and 95% prediction intervals"), tableOutput(ns("fc_table"))),
        tabPanel("How it works", br(),
                 p(strong("AR(p):"), "each value depends on the previous p values. ",
                   strong("MA(q):"), "each value depends on the previous q forecast errors. ",
                   strong("ARMA(p,q):"), "both together. ",
                   strong("ARIMA(p,d,q):"), "ARMA applied after differencing the series d times, which removes a trend."),
                 p(strong("Choosing the orders:"), "the PACF often suggests p (it cuts off after lag p for an AR process)",
                   "and the ACF suggests q (it cuts off after lag q for an MA process). You can also let the app",
                   "search for the model with the smallest AICc, AIC or BIC."),
                 p(strong("Parameter estimates:"), "estimated by maximum likelihood. A small p-value means the",
                   "parameter is significantly different from zero."))
      )
    )
  )
}

fc_bj_server <- function(id, kind) {
  moduleServer(id, function(input, output, session) {
    sample_df <- if (kind == "ARIMA") fc_sample_arima else fc_sample_arma
    d <- fc_data_server(input, output, session, sample_df, min_obs = 12)
    
    ival <- function(x, default) if (is.null(x) || is.na(x)) default else as.integer(x)
    period <- reactive(ival(input$period, 1L))
    
    series <- reactive({
      m <- period()
      validate(need(m >= 1, "The seasonal period must be 1 or more."))
      validate(need(length(d()) >= 2 * m + 4, "Not enough data for this seasonal period."))
      ts(d(), frequency = m)
    })
    
    fit <- eventReactive(input$run, {
      y <- series()
      withProgress(message = "Fitting the model...", value = 0.5, {
        res <- tryCatch(
          fc_bj_estimate(y, kind, input$mode,
                         p = ival(input$p, 1L), d = ival(input$d, 0L), q = ival(input$q, 1L),
                         P = ival(input$P, 0L), D = ival(input$D, 0L), Q = ival(input$Q, 0L),
                         period = period(), constant = isTRUE(input$const),
                         criterion = if (is.null(input$criterion)) "aicc" else input$criterion),
          error = function(e) e)
        validate(need(!inherits(res, "error"),
                      paste("The model could not be estimated:", if (inherits(res, "error")) conditionMessage(res) else "")))
        res
      })
    })
    
    output$kpis <- renderUI({
      if (input$run == 0) return(p(em("Choose your settings and press 'Fit the model'.")))
      f <- fit()
      kpi <- function(lab, val, cls = "") div(class = paste("kpi", cls), div(class = "lab", lab), div(class = "val", val))
      tagList(kpi("Model", fc_bj_label(f, kind), "best"), kpi("AICc", round(f$aicc, 1)),
              kpi("AIC", round(f$aic, 1)), kpi("BIC", round(f$bic, 1)))
    })
    
    output$plot_ts <- renderPlot({
      y <- series()
      plot(y, type = "o", pch = 16, col = "#0f766e", xlab = "Time", ylab = "Demand",
           main = "Time series plot")
    })
    
    output$plot_acf <- renderPlot({
      y <- series(); f <- fit()
      m <- f$arma[5]; dd <- f$arma[6]; DD <- f$arma[7]
      z <- y
      if (DD > 0) z <- diff(z, lag = m, differences = DD)
      if (dd > 0) z <- diff(z, differences = dd)
      differenced <- dd > 0 || DD > 0
      par(mfrow = if (differenced) c(2, 2) else c(1, 2))
      acf(y,  main = "ACF of the series",  col = "#0f766e", lwd = 2)
      pacf(y, main = "PACF of the series", col = "#0f766e", lwd = 2)
      if (differenced) {
        acf(z,  main = "ACF after differencing",  col = "#f59e0b", lwd = 2)
        pacf(z, main = "PACF after differencing", col = "#f59e0b", lwd = 2)
      }
    })
    
    output$param_table <- renderTable({
      f <- fit(); co <- f$coef
      if (length(co) == 0) return(data.frame(Message = "This model has no estimated coefficients."))
      se <- tryCatch(sqrt(diag(f$var.coef)), error = function(e) rep(NA_real_, length(co)))
      if (length(se) != length(co)) se <- rep(NA_real_, length(co))
      tv <- co / se
      data.frame(Parameter = names(co), Estimate = round(co, 4), `Std. error` = round(se, 4),
                 `t value` = round(tv, 4), `p-value` = round(2 * (1 - pnorm(abs(tv))), 4),
                 check.names = FALSE)
    }, digits = 4)
    
    output$stat_table <- renderTable({
      f <- fit()
      data.frame(Statistic = c("sigma^2", "Log likelihood", "AIC", "AICc", "BIC"),
                 Value = round(c(f$sigma2, f$loglik, f$aic, f$aicc, f$bic), 3))
    }, digits = 3)
    
    output$error_table <- renderTable({
      e <- fc_errors(as.numeric(series()), as.numeric(fitted(fit())))
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"), Value = round(unname(e$values), 2))
    })
    
    output$plot_fit <- renderPlot({
      y <- series()
      plot(y, type = "o", pch = 16, col = "grey30", xlab = "Time", ylab = "Demand",
           main = "Actual demand and fitted values")
      lines(fitted(fit()), col = "#0f766e", lwd = 2)
      legend("topleft", bty = "n", lty = 1, col = c("grey30", "#0f766e"), legend = c("Actual", "Fitted"))
    })
    
    output$plot_resid <- renderPlot({
      acf(residuals(fit()), main = "ACF of the residuals", col = "#0f766e", lwd = 2)
    })
    
    fc <- reactive({ req(input$h); forecast::forecast(fit(), h = as.integer(input$h)) })
    
    output$plot_fc <- renderPlot(plot(fc(), main = paste("Forecast from", fc_bj_label(fit(), kind)),
                                      xlab = "Time", ylab = "Demand"))
    
    output$fc_table <- renderTable({
      f <- fc()
      data.frame(Period = length(series()) + seq_along(f$mean), Forecast = round(as.numeric(f$mean), 2),
                 `Lo 80` = round(as.numeric(f$lower[, 1]), 2), `Hi 80` = round(as.numeric(f$upper[, 1]), 2),
                 `Lo 95` = round(as.numeric(f$lower[, 2]), 2), `Hi 95` = round(as.numeric(f$upper[, 2]), 2),
                 check.names = FALSE)
    })
  })
}

# ============================================================
# BOX-JENKINS: menu
# ============================================================
fc_boxjenkins_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("AR",    fc_bj_ui(ns("ar"),    "AR")),
    tabPanel("MA",    fc_bj_ui(ns("ma"),    "MA")),
    tabPanel("ARMA",  fc_bj_ui(ns("arma"),  "ARMA")),
    tabPanel("ARIMA", fc_bj_ui(ns("arima"), "ARIMA"))
  )
}

fc_boxjenkins_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    fc_bj_server("ar",    "AR")
    fc_bj_server("ma",    "MA")
    fc_bj_server("arma",  "ARMA")
    fc_bj_server("arima", "ARIMA")
  })
}


# ############################################################
# 6. MAIN MENU
# ############################################################

# ============================================================
# MAIN FORECASTING MODULE (app.R calls these two functions)
# ============================================================
forecasting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fc_css,
    tabsetPanel(
      tabPanel("Stationary Series",
               fc_banner("Stationary Series",
                         "Demand that fluctuates around a stable level: moving averages, exponential smoothing and seasonal factors."),
               fc_stationary_ui(ns("stationary"))),
      tabPanel("Trend Series",
               fc_banner("Trend Series",
                         "Demand that grows or falls over time: Holt's method (double exponential smoothing)."),
               fc_trend_ui(ns("trend"))),
      tabPanel("Seasonal Series",
               fc_banner("Seasonal Series",
                         "Demand with a repeating seasonal pattern: seasonal decomposition with moving averages and Winters' method."),
               fc_seasonal_ui(ns("seasonal"))),
      tabPanel("Box-Jenkins",
               fc_banner("Box-Jenkins",
                         "AR, MA, ARMA and ARIMA models with ACF and PACF plots, parameter estimates and forecasts."),
               fc_boxjenkins_ui(ns("boxjenkins")))
    )
  )
}

forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    fc_stationary_server("stationary")
    fc_trend_server("trend")
    fc_seasonal_server("seasonal")
    fc_boxjenkins_server("boxjenkins")
  })
}