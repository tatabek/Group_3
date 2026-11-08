# ============================================================
# AGGREGATE PLANNING (one file for the whole Aggregate Planning module)
# File location: R/aggregate_planning.R
# Functions used by app.R: aggregate_planning_ui and aggregate_planning_server
#
# CONTENTS
#   1. Shared helpers      sample data, file input, net demand, plan table and costs,
#                          whole-worker rounding, plots and formatting
#   2. Heuristic plans     chase, level and mixed plan (one segment-based function)
#   3. Linear programming  LP model with overtime / idle time / subcontracting options
#   4. Transportation      north-west corner, cheapest cell first, optimal solution
#   5. Disaggregation      square-root rule for product families
#   6. Main menu           aggregate_planning_ui and aggregate_planning_server
#
# NOTATION (Nahmias, Chapter 3)
#   D_t demand, n_t working days, K units per worker per day,
#   W_t workers, H_t hired, F_t fired, I_t ending inventory,
#   Net demand: D_1 - I_0 in the first period and D_n + I_n in the last period.
#   Regular-time cost is paid on the whole regular capacity K * n_t * W_t
#   (idle time is therefore also paid); overtime and subcontracting are per unit.
#
# Package needed in addition to the other modules: lpSolve
# ============================================================
library(shiny)
library(readxl)


# ############################################################
# 1. SHARED HELPERS
# ############################################################

# ---------- Look and feel ----------
ap_css <- tags$style(HTML("
  .ap-banner{background:linear-gradient(120deg,#0f766e,#14b8a6);color:#fff;padding:18px 24px;
             border-radius:12px;margin:14px 0 16px 0;}
  .ap-banner h2{margin:0 0 4px 0;font-size:24px;font-weight:700;}
  .ap-banner p{margin:0;opacity:.92;}
  .ap-tbl td,.ap-tbl th{text-align:right;} .ap-tbl td:first-child,.ap-tbl th:first-child{text-align:left;}
  .ap-note{background:#ecfdf5;border-left:4px solid #14b8a6;padding:8px 12px;margin:8px 0 14px 0;
           border-radius:4px;color:#134e4a;}
  .ap-warn{background:#fffbeb;border-left:4px solid #f59e0b;padding:8px 12px;margin:8px 0 14px 0;
           border-radius:4px;color:#78350f;}
  .ap-h{margin-top:22px;color:#134e4a;font-weight:600;}
"))

ap_banner <- function(title, subtitle) {
  div(class = "ap-banner", h2(title), p(subtitle))
}

# ---------- Sample data ----------
ap_sample <- data.frame(
  Period        = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
  Demand        = c(3200, 3800, 4600, 5400, 4400, 3600),
  `Working days` = c(20, 19, 22, 21, 22, 21),
  check.names = FALSE
)

# ---------- Reading a csv / xlsx file ----------
# Looks for a period-name column, a demand column and a working-days column
# (by name first, otherwise by position). Returns a data frame with the
# columns Period, Demand and Working days.
ap_read_file <- function(path, name) {
  if (grepl("\\.xlsx?$", name, ignore.case = TRUE)) {
    df <- as.data.frame(read_excel(path))
  } else {
    df <- read.csv(path, check.names = FALSE)
    if (ncol(df) == 1) df <- read.csv2(path, check.names = FALSE)   # ; separated files
  }
  validate(need(ncol(df) >= 2, "The file needs at least a demand and a working-days column."))
  cols <- names(df)
  is_num <- vapply(df, is.numeric, logical(1))
  is_id <- grepl("period|month|week|name", cols, ignore.case = TRUE)   # period-name column
  num_cols <- cols[is_num & !is_id]

  dem_col  <- num_cols[grep("demand|forecast", num_cols, ignore.case = TRUE)][1]
  days_col <- num_cols[grep("day", num_cols, ignore.case = TRUE)][1]
  if (is.na(dem_col)) dem_col <- setdiff(num_cols, days_col)[1]
  if (is.na(days_col)) days_col <- setdiff(num_cols, dem_col)[1]
  validate(need(!is.na(dem_col) && !is.na(days_col),
                "Could not find the demand and working-days columns. Name them 'Demand' and 'Working days'."))

  per_col <- cols[is_id][1]
  if (is.na(per_col) && any(!is_num)) per_col <- cols[!is_num][1]
  period <- if (is.na(per_col)) paste("Period", seq_len(nrow(df))) else as.character(df[[per_col]])

  out <- data.frame(Period = period,
                    Demand = as.numeric(df[[dem_col]]),
                    `Working days` = as.numeric(df[[days_col]]),
                    check.names = FALSE)
  out[stats::complete.cases(out), , drop = FALSE]
}

# ---------- Net demand ----------
# Starting inventory reduces the first period's demand,
# the required ending inventory is added to the last period's demand.
ap_net_demand <- function(p) {
  nd <- p$D
  n <- length(nd)
  nd[1] <- nd[1] - p$I0
  nd[n] <- nd[n] + p$In
  nd
}

# ---------- Plan table + cost ----------
# W: workers per period; ot: overtime units; sub: subcontracted units;
# idle: unused regular capacity. Regular production = capacity - idle,
# unless reg is given.
ap_build_plan <- function(p, names, W, ot, sub, idle, reg = NULL) {
  n <- length(p$D)
  cap <- p$K * p$days * W
  if (is.null(reg)) reg <- cap - idle
  prod <- reg + ot + sub
  I <- p$I0 + cumsum(prod) - cumsum(p$D)
  I[abs(I) < 1e-7] <- 0
  dW <- diff(c(p$W0, W))
  H <- pmax(dW, 0)
  Fi <- pmax(-dW, 0)
  H[H < 1e-9] <- 0
  Fi[Fi < 1e-9] <- 0

  tab <- data.frame(
    Period = names,
    `Working days` = p$days,
    `Net demand` = ap_net_demand(p),
    Workers = W,
    Hired = H,
    Fired = Fi,
    `Regular production` = reg,
    Overtime = ot,
    Subcontracted = sub,
    `Idle capacity` = idle,
    `Total production` = prod,
    Inventory = I,
    check.names = FALSE
  )
  cost <- c(Hiring = p$cH * sum(H),
            Firing = p$cF * sum(Fi),
            Holding = p$cI * sum(I),
            `Regular time` = p$cR * sum(cap),
            Overtime = p$cO * sum(ot),
            Subcontracting = p$cS * sum(sub))
  cost <- c(cost, Total = sum(cost))
  list(tab = tab, cost = cost)
}

# ---------- Whole workers ----------
ap_is_whole <- function(W) all(abs(W - round(W)) < 1e-6)
ap_round_up <- function(W) ceiling(W - 1e-6)

# Heuristic plans: round the workforce up, everybody works the full regular time,
# the extra production goes into inventory.
ap_round_heuristic <- function(p, names, plan) {
  W <- ap_round_up(plan$tab$Workers)
  n <- length(W)
  ap_build_plan(p, names, W, ot = rep(0, n), sub = rep(0, n), idle = rep(0, n))
}

# ---------- Formatting ----------
ap_num <- function(x, digits = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    s <- formatC(round(v, digits) + 0, format = "f", digits = digits)
    sub("\\.?0+$", "", s)       # drop trailing zeros: 3200.00 -> 3200, 120.50 -> 120.5
  }, character(1))
}

# Plan table for the screen. Columns that are never used are removed.
ap_display <- function(tab) {
  for (col in c("Overtime", "Subcontracted", "Idle capacity")) {
    if (all(abs(tab[[col]]) < 1e-7)) tab[[col]] <- NULL
  }
  out <- tab
  for (j in seq_along(out)) if (is.numeric(out[[j]])) out[[j]] <- ap_num(out[[j]])
  out
}

# Cost table: original and whole-worker plan side by side
ap_cost_table <- function(orig, rounded = NULL) {
  items <- names(orig$cost)
  out <- data.frame(Item = items, Original = ap_num(orig$cost), check.names = FALSE)
  if (!is.null(rounded)) {
    out[["Whole workers (rounded up)"]] <- ap_num(rounded$cost)
    out[["Difference"]] <- ap_num(rounded$cost - orig$cost)
  }
  out
}

# ---------- Plot: cumulative production vs cumulative net demand ----------
ap_plot_cumulative <- function(tab, title) {
  n <- nrow(tab)
  cum_prod <- cumsum(tab[["Total production"]])
  cum_dem  <- cumsum(tab[["Net demand"]])
  rng <- range(c(0, cum_prod, cum_dem))
  plot(1:n, cum_dem, type = "n", xaxt = "n", ylim = rng, xlim = c(0.7, n + 0.3),
       xlab = "Period", ylab = "Units (cumulative)", main = title)
  axis(1, at = 1:n, labels = tab$Period)
  grid(nx = NA, ny = NULL)
  lines(1:n, cum_dem, type = "o", pch = 19, lwd = 2, col = "grey30")
  lines(1:n, cum_prod, type = "o", pch = 17, lwd = 2, lty = 2, col = "#0f766e")
  legend("topleft", bty = "n",
         legend = c("Cumulative net demand", "Cumulative production"),
         col = c("grey30", "#0f766e"), pch = c(19, 17), lty = c(1, 2), lwd = 2)
}

# ---------- Result area shared by every method ----------
# One block = notes, plan table, cumulative plot and cost table
# (and the same for the whole-worker plan when the workforce is fractional).
ap_result_ui <- function(ns, id) {
  tagList(
    uiOutput(ns(paste0(id, "_note"))),
    h4(class = "ap-h", "Plan"),
    div(class = "ap-tbl", tableOutput(ns(paste0(id, "_plan")))),
    h4(class = "ap-h", "Cumulative production vs. cumulative net demand"),
    plotOutput(ns(paste0(id, "_plot")), height = "320px"),
    h4(class = "ap-h", "Cost of each item"),
    div(class = "ap-tbl", tableOutput(ns(paste0(id, "_cost")))),
    uiOutput(ns(paste0(id, "_rounded")))
  )
}

# res: reactive returning list(orig = plan, rounded = plan or NULL, note = text or NULL)
ap_result_server <- function(output, ns, id, res) {
  output[[paste0(id, "_note")]] <- renderUI({
    r <- res()
    msgs <- list()
    if (!is.null(r$note)) msgs <- c(msgs, list(div(class = "ap-warn", r$note)))
    if (is.null(r$rounded)) {
      msgs <- c(msgs, list(div(class = "ap-note",
                               "All numbers of workers are whole, so no rounding is needed.")))
    } else {
      extra <- r$rounded$cost[["Total"]] - r$orig$cost[["Total"]]
      msgs <- c(msgs, list(div(class = "ap-note", sprintf(
        "The workforce is fractional. Rounding it up to whole workers changes the total cost from %s to %s (%s%s).",
        ap_num(r$orig$cost[["Total"]]), ap_num(r$rounded$cost[["Total"]]),
        ifelse(extra >= 0, "+", ""), ap_num(extra)))))
    }
    tagList(msgs)
  })
  output[[paste0(id, "_plan")]] <- renderTable(ap_display(res()$orig$tab))
  output[[paste0(id, "_plot")]] <- renderPlot(ap_plot_cumulative(res()$orig$tab, "Original plan"))
  output[[paste0(id, "_cost")]] <- renderTable(ap_cost_table(res()$orig, res()$rounded))
  output[[paste0(id, "_rounded")]] <- renderUI({
    r <- res()
    if (is.null(r$rounded)) return(NULL)
    tagList(
      h4(class = "ap-h", "Plan with whole workers (rounded up)"),
      div(class = "ap-tbl", tableOutput(ns(paste0(id, "_plan2")))),
      plotOutput(ns(paste0(id, "_plot2")), height = "320px")
    )
  })
  output[[paste0(id, "_plan2")]] <- renderTable({
    r <- res()
    req(r$rounded)
    ap_display(r$rounded$tab)
  })
  output[[paste0(id, "_plot2")]] <- renderPlot({
    r <- res()
    req(r$rounded)
    ap_plot_cumulative(r$rounded$tab, "Plan with whole workers")
  })
}


# ############################################################
# 2. HEURISTIC PLANS (chase, level, mixed)
# ############################################################

# One function builds all three plans. change[t] = TRUE means that the workforce
# may change at the start of period t (period 1 always counts as a change from
# the starting workforce). Inside a segment the workforce is constant and is the
# smallest value that never leaves a period short of net demand.
#   chase (zero inventory) : change is TRUE in every period
#   level (constant)       : change is TRUE only in period 1
#   mixed                  : the user picks the periods
ap_segment_plan <- function(p, names, change) {
  n <- length(p$D)
  change[1] <- TRUE
  starts <- which(change)
  ends <- c(starts[-1] - 1, n)
  W <- numeric(n)
  P <- numeric(n)
  I_prev <- p$I0
  for (s in seq_along(starts)) {
    idx <- starts[s]:ends[s]
    target <- ifelse(idx == n, p$In, 0)
    need <- (cumsum(p$D[idx]) + target - I_prev) / (p$K * cumsum(p$days[idx]))
    w <- max(0, need)
    for (t in idx) {
      W[t] <- w
      P[t] <- p$K * p$days[t] * w
      I_prev <- I_prev + P[t] - p$D[t]
    }
  }
  n0 <- rep(0, n)
  ap_build_plan(p, names, W, ot = n0, sub = n0, idle = n0, reg = P)
}

# Plan + whole-worker plan packed for the result block
ap_heuristic_result <- function(p, names, change) {
  plan <- ap_segment_plan(p, names, change)
  rounded <- NULL
  if (!ap_is_whole(plan$tab$Workers)) rounded <- ap_round_heuristic(p, names, plan)
  note <- NULL
  if (plan$tab$Inventory[nrow(plan$tab)] > p$In + 1e-6) {
    note <- sprintf("The plan ends with %s units in inventory, %s more than the required ending inventory.",
                    ap_num(plan$tab$Inventory[nrow(plan$tab)]),
                    ap_num(plan$tab$Inventory[nrow(plan$tab)] - p$In))
  }
  list(orig = plan, rounded = rounded, note = note)
}

ap_heuristic_ui <- function(ns) {
  tabsetPanel(
    tabPanel("Zero-inventory (chase)",
             div(class = "ap-note", "The workforce is changed in every period so that production equals net demand and no inventory is carried (except the required ending inventory)."),
             ap_result_ui(ns, "chase")),
    tabPanel("Constant workforce (level)",
             div(class = "ap-note", "One constant workforce for the whole horizon: the smallest workforce that never runs short of cumulative net demand. Workers are hired or fired only once, in the first period."),
             ap_result_ui(ns, "level")),
    tabPanel("Mixed plan",
             div(class = "ap-note", "Choose the periods in which the workforce is allowed to change. Between two chosen periods the workforce stays constant. Period 1 always counts as a change from the starting workforce."),
             uiOutput(ns("mixed_periods")),
             ap_result_ui(ns, "mixed"))
  )
}


# ############################################################
# 3. LINEAR PROGRAMMING
# ############################################################

# Variables (each has n values, one per period):
#   W workers, H hired, F fired, O overtime, U idle, S subcontracted, I inventory
# Constraints:
#   W_t = W_{t-1} + H_t - F_t
#   I_t = I_{t-1} + K n_t W_t - U_t + O_t + S_t - D_t
#   O_t <= overtime limit * K n_t W_t      (O_t = 0 if overtime is switched off)
#   U_t <= K n_t W_t                       (U_t = 0 if idle time is switched off)
#   S_t <= subcontracting limit            (S_t = 0 if subcontracting is switched off)
#   I_n >= required ending inventory
# Objective: hiring + firing + holding + regular time (on capacity) + overtime + subcontracting.
# Wfix: optional vector of fixed workers (used for the whole-worker plan).
ap_lp_solve <- function(p, names, use_ot = TRUE, use_idle = TRUE, use_sub = TRUE, Wfix = NULL) {
  n <- length(p$D)
  nv <- 7 * n
  iW <- function(t) t
  iH <- function(t) n + t
  iF <- function(t) 2 * n + t
  iO <- function(t) 3 * n + t
  iU <- function(t) 4 * n + t
  iS <- function(t) 5 * n + t
  iI <- function(t) 6 * n + t

  rows <- list(); dirs <- character(0); rhs <- numeric(0)
  add <- function(idx, val, dir, b) {
    r <- numeric(nv)
    r[idx] <- val
    rows[[length(rows) + 1]] <<- r
    dirs <<- c(dirs, dir)
    rhs <<- c(rhs, b)
  }

  for (t in 1:n) {
    kn <- p$K * p$days[t]
    # workforce balance
    if (t == 1) add(c(iW(1), iH(1), iF(1)), c(1, -1, 1), "=", p$W0)
    else add(c(iW(t), iW(t - 1), iH(t), iF(t)), c(1, -1, -1, 1), "=", 0)
    # inventory balance
    if (t == 1) add(c(iI(1), iW(1), iU(1), iO(1), iS(1)), c(1, -kn, 1, -1, -1), "=", -p$D[1] + p$I0)
    else add(c(iI(t), iI(t - 1), iW(t), iU(t), iO(t), iS(t)), c(1, -1, -kn, 1, -1, -1), "=", -p$D[t])
    # overtime
    if (use_ot) add(c(iO(t), iW(t)), c(1, -p$otfrac * kn), "<=", 0)
    else add(iO(t), 1, "=", 0)
    # idle time
    if (use_idle) add(c(iU(t), iW(t)), c(1, -kn), "<=", 0)
    else add(iU(t), 1, "=", 0)
    # subcontracting
    if (!use_sub) add(iS(t), 1, "=", 0)
    else if (is.finite(p$subcap)) add(iS(t), 1, "<=", p$subcap)
    # fixed workers (whole-worker plan)
    if (!is.null(Wfix)) add(iW(t), 1, "=", Wfix[t])
  }
  add(iI(n), 1, ">=", p$In)

  obj <- numeric(nv)
  for (t in 1:n) {
    obj[iH(t)] <- p$cH
    obj[iF(t)] <- p$cF
    obj[iI(t)] <- p$cI
    obj[iW(t)] <- p$cR * p$K * p$days[t]
    obj[iO(t)] <- p$cO
    obj[iS(t)] <- p$cS
  }

  res <- lpSolve::lp("min", obj, do.call(rbind, rows), dirs, rhs)
  if (res$status != 0) return(NULL)

  sol <- res$solution
  clean <- function(x) { x[abs(x) < 1e-7] <- 0; x }
  ap_build_plan(p, names,
                W = clean(sol[1:n]),
                ot = clean(sol[3 * n + 1:n]),
                sub = clean(sol[5 * n + 1:n]),
                idle = clean(sol[4 * n + 1:n]))
}

ap_lp_ui <- function(ns) {
  tagList(
    div(class = "ap-note", "Linear programming model: minimise hiring + firing + holding + regular time + overtime + subcontracting cost. Workers are not forced to be whole numbers in the LP; if the optimal workforce is fractional it is rounded up and the production plan is re-optimised with those whole workers."),
    checkboxGroupInput(ns("lp_options"), "Options included in the model",
                       choices = c("Overtime" = "ot", "Idle time" = "idle", "Subcontracting" = "sub"),
                       selected = c("ot", "idle", "sub"), inline = TRUE),
    ap_result_ui(ns, "lp")
  )
}


# ############################################################
# 4. TRANSPORTATION METHOD
# ############################################################

# Sources: regular time, overtime and subcontracting of every period.
# Destinations: net demand of every period + one dummy column for unused capacity.
# Unit cost of source type in period s used for demand of period t (t >= s):
#   type cost + holding cost * (t - s).  Cells with t < s are forbidden (no backorders).
# Unused regular capacity is paid (cost cR in the dummy column), unused overtime
# and subcontracting capacity cost nothing.
ap_tr_setup <- function(p, W) {
  n <- length(p$D)
  capR <- p$K * p$days * W
  capO <- p$otfrac * capR
  capS <- if (is.finite(p$subcap)) rep(p$subcap, n) else rep(max(p$D) + p$In, n)   # "unlimited": enough for the largest single period

  dem <- p$D                      # starting inventory is used for the earliest demand
  rem <- p$I0
  for (t in 1:n) {
    use <- min(rem, dem[t])
    dem[t] <- dem[t] - use
    rem <- rem - use
  }
  dem[n] <- dem[n] + p$In

  cap <- as.vector(rbind(capR, capO, capS))        # R1, O1, S1, R2, O2, S2, ...
  type <- rep(1:3, n)
  src <- rep(1:n, each = 3)
  base <- c(p$cR, p$cO, p$cS)[type]
  big <- 1e6

  cost <- matrix(big, nrow = 3 * n, ncol = n + 1)
  allowed <- matrix(FALSE, nrow = 3 * n, ncol = n)
  for (i in seq_len(3 * n)) {
    for (j in 1:n) {
      if (j >= src[i]) {
        cost[i, j] <- base[i] + p$cI * (j - src[i])
        allowed[i, j] <- TRUE
      }
    }
    cost[i, n + 1] <- if (type[i] == 1) p$cR else 0
  }
  slack <- sum(cap) - sum(dem)
  list(cap = cap, dem = c(dem, max(slack, 0)), cost = cost, allowed = allowed,
       n = n, slack = slack)
}

ap_tr_labels <- function(names) {
  paste(rep(names, each = 3), c("Regular", "Overtime", "Subcontract"))
}

# North-west corner: start at the upper-left cell and move right / down.
# Forbidden cells (backorders) are skipped. The dummy column gets what is left.
ap_tr_nwc <- function(s) {
  n <- s$n
  sup <- s$cap
  dem <- s$dem
  x <- matrix(0, nrow = length(sup), ncol = n + 1)
  for (j in 1:n) {
    for (i in seq_along(sup)) {
      if (s$allowed[i, j] && sup[i] > 1e-9 && dem[j] > 1e-9) {
        q <- min(sup[i], dem[j])
        x[i, j] <- q
        sup[i] <- sup[i] - q
        dem[j] <- dem[j] - q
      }
    }
  }
  if (any(dem[1:n] > 1e-6)) return(NULL)
  x[, n + 1] <- sup
  x
}

# Cheapest cell first: fill the cheapest allowed cell, then the next cheapest, ...
ap_tr_cheapest <- function(s) {
  n <- s$n
  sup <- s$cap
  dem <- s$dem
  x <- matrix(0, nrow = length(sup), ncol = n + 1)
  cells <- expand.grid(i = seq_along(sup), j = 1:n)
  cells <- cells[s$allowed[cbind(cells$i, cells$j)], , drop = FALSE]
  ord <- order(s$cost[cbind(cells$i, cells$j)], cells$i, cells$j)
  for (k in ord) {
    i <- cells$i[k]; j <- cells$j[k]
    q <- min(sup[i], dem[j])
    if (q > 1e-9) {
      x[i, j] <- q
      sup[i] <- sup[i] - q
      dem[j] <- dem[j] - q
    }
  }
  if (any(dem[1:n] > 1e-6)) return(NULL)
  x[, n + 1] <- sup
  x
}

# Optimal solution of the transportation problem (lpSolve)
ap_tr_optimal <- function(s) {
  res <- lpSolve::lp.transport(s$cost, "min",
                               row.signs = rep("=", length(s$cap)), row.rhs = s$cap,
                               col.signs = rep("=", length(s$dem)), col.rhs = s$dem)
  if (res$status != 0) return(NULL)
  x <- res$solution
  x[abs(x) < 1e-7] <- 0
  if (any(x[, 1:s$n][!s$allowed] > 1e-6)) return(NULL)   # a forbidden cell (backorder) was used
  x
}

# Optimal solution by the cheapest-cell-first rule. The result is checked against
# the exact optimum (lpSolve); if cheapest cell first is not optimal, the exact
# optimal solution is used instead.
ap_tr_best <- function(s) {
  xc <- ap_tr_cheapest(s)
  xo <- ap_tr_optimal(s)
  if (is.null(xo)) return(xc)
  if (is.null(xc) || ap_tr_cost(s, xc) > ap_tr_cost(s, xo) + 1e-6) return(xo)
  xc
}

# Total cost of the flows on the tableau (production, holding and unused regular time)
ap_tr_cost <- function(s, x) sum(x * s$cost)

# Plan from the flows
ap_tr_plan <- function(p, names, W, x) {
  n <- length(p$D)
  used <- rowSums(x[, 1:n, drop = FALSE])
  reg <- used[seq(1, 3 * n, by = 3)]
  ot  <- used[seq(2, 3 * n, by = 3)]
  sub <- used[seq(3, 3 * n, by = 3)]
  idle <- p$K * p$days * W - reg
  idle[abs(idle) < 1e-7] <- 0
  ap_build_plan(p, names, W, ot = ot, sub = sub, idle = idle, reg = reg)
}

# Tableau for the screen: flows, with capacities in the first column
ap_tr_tableau <- function(s, x, names) {
  n <- s$n
  m <- cbind(s$cap, x)
  body <- matrix("", nrow = nrow(m) + 1, ncol = ncol(m))
  for (i in seq_len(nrow(m))) {
    body[i, 1] <- ap_num(m[i, 1])
    for (j in 2:ncol(m)) if (abs(m[i, j]) > 1e-9) body[i, j] <- ap_num(m[i, j])
  }
  body[nrow(m) + 1, ] <- c(ap_num(sum(s$cap)), ap_num(s$dem))
  out <- as.data.frame(body, stringsAsFactors = FALSE)
  names(out) <- c("Capacity", names, "Unused")
  cbind(Source = c(ap_tr_labels(names), "Demand"), out, stringsAsFactors = FALSE)
}

ap_tr_cost_matrix <- function(s, names) {
  m <- s$cost
  txt <- matrix("", nrow(m), ncol(m))
  for (i in seq_len(nrow(m))) for (j in seq_len(ncol(m))) {
    if (j <= s$n && !s$allowed[i, j]) txt[i, j] <- "-" else txt[i, j] <- ap_num(m[i, j])
  }
  out <- as.data.frame(txt, stringsAsFactors = FALSE)
  names(out) <- c(names, "Unused")
  cbind(Source = ap_tr_labels(names), out, stringsAsFactors = FALSE)
}

# Runs one method for a given workforce. Returns NULL when the method fails.
ap_tr_run <- function(p, names, W, method) {
  s <- ap_tr_setup(p, W)
  if (s$slack < -1e-6) return(NULL)
  x <- switch(method,
              nwc = ap_tr_nwc(s),
              cheapest = ap_tr_cheapest(s),
              optimal = ap_tr_best(s))
  if (is.null(x)) return(NULL)
  list(s = s, x = x, plan = ap_tr_plan(p, names, W, x), tableau_cost = ap_tr_cost(s, x))
}

ap_tr_result_ui <- function(ns, id) {
  tagList(
    uiOutput(ns(paste0(id, "_tr_note"))),
    h4(class = "ap-h", "Transportation tableau (units shipped)"),
    div(class = "ap-tbl", tableOutput(ns(paste0(id, "_tableau")))),
    tags$details(tags$summary("Show the unit cost matrix"),
                 div(class = "ap-tbl", tableOutput(ns(paste0(id, "_costmat"))))),
    ap_result_ui(ns, id)
  )
}

ap_transport_ui <- function(ns) {
  tagList(
    div(class = "ap-note", "Sources are the regular time, overtime and subcontracting capacity of every period. A unit made in period s and sold in period t costs the production cost of its source plus the holding cost for t - s periods. Backorders are not allowed (forbidden cells are shown with a dash). The workforce is fixed, so hiring and firing only occurs in the first period when the workforce differs from the starting workforce. Set the overtime or subcontracting limit to 0 on the left to remove that source. With the level-plan workforce regular time alone is enough, so overtime and subcontracting stay unused; enter a smaller workforce to see them used."),
    radioButtons(ns("tr_workforce"), "Workforce used in the transportation model",
                 choices = c("I will enter a constant workforce" = "manual",
                             "Level-plan workforce (smallest constant workforce)" = "level"), inline = TRUE),
    conditionalPanel(
      condition = "input.tr_workforce == 'manual'", ns = ns,
      numericInput(ns("tr_W"), "Workers in every period", value = 45, min = 0, step = 1)
    ),
    tabsetPanel(
      tabPanel("North-west corner (starting solution)",
               ap_tr_result_ui(ns, "nwc")),
      tabPanel("Optimal solution (cheapest cell first)",
               ap_tr_result_ui(ns, "optimal"))
    )
  )
}


# ############################################################
# 5. DISAGGREGATION (square-root rule)
# ############################################################

# The aggregate production of every period is split among the product families
# in proportion to their share of demand. For each family the square-root rule
# (economic order quantity) gives the run size and so the number of runs:
#   Q_i = sqrt(2 * S_i * lambda_i / h_i)
# lambda_i : demand of family i per period (share_i * average aggregate demand)
# S_i      : setup cost per run, h_i : holding cost per unit per period
# Setup cost per period = S_i * lambda_i / Q_i, holding cost per period = h_i * Q_i / 2
# (both equal sqrt(S_i * h_i * lambda_i / 2) at the optimum).
ap_disagg <- function(prod, dem, names, fam) {
  share <- fam$share / sum(fam$share)
  lambda <- share * mean(dem)
  Q <- sqrt(2 * fam$setup * lambda / fam$hold)
  alloc <- outer(prod, share)
  colnames(alloc) <- fam$name
  runs <- sweep(alloc, 2, Q, "/")
  runs[!is.finite(runs)] <- 0
  summary <- data.frame(
    Family = fam$name,
    `Share of demand (%)` = share * 100,
    `Setup cost S` = fam$setup,
    `Holding cost h` = fam$hold,
    `Demand per period` = lambda,
    `Run size Q = sqrt(2 S lambda / h)` = Q,
    `Runs per period` = lambda / Q,
    `Setup cost per period` = fam$setup * lambda / Q,
    `Holding cost per period` = fam$hold * Q / 2,
    check.names = FALSE
  )
  summary[["Total cost per period"]] <- summary[["Setup cost per period"]] + summary[["Holding cost per period"]]
  list(alloc = alloc, runs = runs, summary = summary, share = share)
}

ap_disagg_ui <- function(ns) {
  tagList(
    div(class = "ap-note", "Splits the production of an aggregate plan among product families. Production of each period is shared in proportion to the demand share of the families, and the square-root rule (economic order quantity) gives each family's run size, number of runs and setup and holding cost."),
    fluidRow(
      column(6, selectInput(ns("dg_plan"), "Aggregate plan to split",
                            choices = c("Zero-inventory (chase)" = "chase",
                                        "Constant workforce (level)" = "level",
                                        "Mixed plan" = "mixed",
                                        "Linear programming" = "lp",
                                        "Transportation (optimal)" = "optimal"))),
      column(6, checkboxInput(ns("dg_rounded"), "Use the whole-worker (rounded up) plan when it exists", value = FALSE),
             numericInput(ns("dg_n"), "Number of product families", value = 3, min = 1, max = 8, step = 1))
    ),
    uiOutput(ns("dg_families")),
    h4(class = "ap-h", "Aggregate production split among the families (units)"),
    div(class = "ap-tbl", tableOutput(ns("dg_alloc"))),
    plotOutput(ns("dg_plot"), height = "320px"),
    h4(class = "ap-h", "Square-root rule for each family"),
    div(class = "ap-tbl", tableOutput(ns("dg_summary"))),
    h4(class = "ap-h", "Number of runs in each period"),
    div(class = "ap-tbl", tableOutput(ns("dg_runs")))
  )
}

ap_fam_defaults <- data.frame(
  name  = c("Family A", "Family B", "Family C", "Family D", "Family E", "Family F", "Family G", "Family H"),
  share = c(50, 30, 20, 10, 10, 10, 10, 10),
  setup = c(300, 200, 150, 250, 250, 250, 250, 250),
  hold  = c(1.2, 0.8, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
  stringsAsFactors = FALSE
)


# ############################################################
# 6. MAIN MENU
# ############################################################

aggregate_planning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ap_css,
    ap_banner("Aggregate Planning",
              "Heuristic plans, linear programming, the transportation method and disaggregation. The data and costs on the left are shared by all methods."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("1. Data"),
        radioButtons(ns("source"), NULL,
                     choices = c("Use the sample data" = "sample",
                                 "Upload my own file (.csv / .xlsx)" = "upload")),
        conditionalPanel(
          condition = "input.source == 'upload'", ns = ns,
          fileInput(ns("file"), NULL, accept = c(".csv", ".xlsx")),
          helpText("Columns: period name, demand forecast, working days.")
        ),
        div(style = "margin-bottom:10px;", downloadLink(ns("sample_csv"), "Download the sample CSV")),
        textAreaInput(ns("period_names"), "Period names (one per line, you can edit them)",
                      rows = 6, resize = "vertical"),

        h4("2. Productivity K"),
        radioButtons(ns("k_mode"), NULL,
                     choices = c("Enter K (units per worker per day)" = "direct",
                                 "Compute K from past data" = "past")),
        conditionalPanel(
          condition = "input.k_mode == 'direct'", ns = ns,
          numericInput(ns("K"), "K (units per worker per day)", value = 4, min = 0.0001, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.k_mode == 'past'", ns = ns,
          numericInput(ns("past_units"), "Units produced", value = 8400, min = 0),
          numericInput(ns("past_workers"), "Number of workers", value = 35, min = 0),
          numericInput(ns("past_days"), "Working days", value = 60, min = 0),
          uiOutput(ns("k_computed"))
        ),

        h4("3. Costs"),
        numericInput(ns("cH"), "Hiring cost (per worker)", value = 400, min = 0),
        numericInput(ns("cF"), "Firing cost (per worker)", value = 600, min = 0),
        numericInput(ns("cI"), "Holding cost (per unit per period)", value = 3, min = 0),
        numericInput(ns("cR"), "Regular-time cost (per unit of regular capacity)", value = 12, min = 0),
        numericInput(ns("cO"), "Overtime cost (per unit)", value = 18, min = 0),
        numericInput(ns("cS"), "Subcontracting cost (per unit)", value = 22, min = 0),

        h4("4. Plant data"),
        numericInput(ns("W0"), "Starting workforce", value = 40, min = 0, step = 1),
        numericInput(ns("I0"), "Starting inventory", value = 200, min = 0),
        numericInput(ns("In"), "Ending inventory (required)", value = 300, min = 0),

        h4("5. Capacity limits (LP and transportation)"),
        numericInput(ns("ot_pct"), "Overtime limit (% of regular capacity)", value = 20, min = 0),
        numericInput(ns("sub_cap"), "Subcontracting limit (units per period, empty = unlimited)", value = 1000, min = 0)
      ),
      mainPanel(
        width = 8,
        uiOutput(ns("data_check")),
        tabsetPanel(
          tabPanel("Heuristic plans", ap_heuristic_ui(ns)),
          tabPanel("Linear programming", ap_lp_ui(ns)),
          tabPanel("Transportation method", ap_transport_ui(ns)),
          tabPanel("Disaggregation", ap_disagg_ui(ns))
        )
      )
    )
  )
}

aggregate_planning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------- Data ----------
    raw <- reactive({
      if (input$source == "sample") return(ap_sample)
      req(input$file)
      ap_read_file(input$file$datapath, input$file$name)
    })

    output$sample_csv <- downloadHandler(
      filename = function() "sample_aggregate_planning.csv",
      content = function(file) write.csv(ap_sample, file, row.names = FALSE)
    )

    # Period names are filled from the data and can then be edited
    observeEvent(raw(), {
      updateTextAreaInput(session, "period_names", value = paste(raw()$Period, collapse = "\n"))
    })

    periods <- reactive({
      n <- nrow(raw())
      txt <- trimws(strsplit(if (is.null(input$period_names)) "" else input$period_names,
                             "\n", fixed = TRUE)[[1]])
      out <- as.character(raw()$Period)
      k <- min(length(txt), n)
      if (k > 0) out[1:k] <- ifelse(txt[1:k] == "", out[1:k], txt[1:k])
      make.unique(out)
    })

    output$data_check <- renderUI({
      d <- raw()
      if (nrow(d) < 2) return(div(class = "ap-warn", "At least two periods are required."))
      NULL
    })

    # ---------- Productivity ----------
    K_value <- reactive({
      if (input$k_mode == "direct") {
        req(input$K)
        input$K
      } else {
        req(input$past_units, input$past_workers, input$past_days)
        validate(need(input$past_workers > 0 && input$past_days > 0,
                      "Number of workers and working days must be positive."))
        input$past_units / (input$past_workers * input$past_days)
      }
    })

    output$k_computed <- renderUI({
      div(class = "ap-note", sprintf("K = units / (workers x days) = %s units per worker per day",
                                     ap_num(K_value(), 4)))
    })

    # ---------- Parameters used by every method ----------
    params <- reactive({
      d <- raw()
      validate(need(nrow(d) >= 2, "At least two periods are required."),
               need(all(d$`Working days` > 0), "Working days must be positive in every period."),
               need(all(d$Demand >= 0), "Demand cannot be negative."))
      for (v in c("cH", "cF", "cI", "cR", "cO", "cS", "W0", "I0", "In", "ot_pct")) {
        validate(need(!is.null(input[[v]]) && !is.na(input[[v]]) && input[[v]] >= 0,
                      "Please fill in all cost and plant data fields with non-negative numbers."))
      }
      sub_cap <- if (is.null(input$sub_cap) || is.na(input$sub_cap)) Inf else input$sub_cap
      list(D = d$Demand, days = d$`Working days`, K = K_value(),
           W0 = input$W0, I0 = input$I0, In = input$In,
           cH = input$cH, cF = input$cF, cI = input$cI,
           cR = input$cR, cO = input$cO, cS = input$cS,
           otfrac = input$ot_pct / 100, subcap = sub_cap)
    })

    # ---------- Heuristic plans ----------
    output$mixed_periods <- renderUI({
      nm <- periods()
      keep <- isolate(input$mixed_change)
      sel <- if (is.null(keep)) c(1, ceiling(length(nm) / 2) + 1) else keep
      sel <- sel[sel <= length(nm)]
      checkboxGroupInput(ns("mixed_change"), "The workforce changes at the start of:",
                         choices = setNames(seq_along(nm), nm), selected = sel, inline = TRUE)
    })

    chase_res <- reactive({
      p <- params()
      ap_heuristic_result(p, periods(), rep(TRUE, length(p$D)))
    })
    level_res <- reactive({
      p <- params()
      ap_heuristic_result(p, periods(), c(TRUE, rep(FALSE, length(p$D) - 1)))
    })
    mixed_res <- reactive({
      p <- params()
      n <- length(p$D)
      sel <- as.integer(input$mixed_change)
      ch <- seq_len(n) %in% sel
      ap_heuristic_result(p, periods(), ch)
    })

    ap_result_server(output, ns, "chase", chase_res)
    ap_result_server(output, ns, "level", level_res)
    ap_result_server(output, ns, "mixed", mixed_res)

    # ---------- Linear programming ----------
    lp_res <- reactive({
      p <- params()
      nm <- periods()
      opt <- input$lp_options
      use <- function(x) x %in% opt
      plan <- ap_lp_solve(p, nm, use("ot"), use("idle"), use("sub"))
      validate(need(!is.null(plan), "The LP has no feasible solution with these options. Allow overtime or subcontracting, or change the limits."))
      rounded <- NULL
      note <- NULL
      if (!ap_is_whole(plan$tab$Workers)) {
        rounded <- ap_lp_solve(p, nm, use("ot"), use("idle"), use("sub"),
                               Wfix = ap_round_up(plan$tab$Workers))
        if (is.null(rounded)) note <- "The plan with whole workers is infeasible with these options."
      }
      list(orig = plan, rounded = rounded, note = note)
    })
    ap_result_server(output, ns, "lp", lp_res)

    # ---------- Transportation ----------
    tr_workforce <- reactive({
      p <- params()
      n <- length(p$D)
      if (input$tr_workforce == "level") {
        rep(ap_segment_plan(p, periods(), c(TRUE, rep(FALSE, n - 1)))$tab$Workers[1], n)
      } else {
        req(input$tr_W)
        validate(need(input$tr_W >= 0, "The workforce cannot be negative."))
        rep(input$tr_W, n)
      }
    })

    tr_method <- function(method, label) {
      reactive({
        p <- params()
        nm <- periods()
        W <- tr_workforce()
        run <- ap_tr_run(p, nm, W, method)
        validate(need(!is.null(run), paste0(
          "The ", label, " solution could not meet every period's net demand without backorders. ",
          "Increase the workforce, the overtime limit or the subcontracting limit.")))
        rounded <- NULL
        run2 <- NULL
        if (!ap_is_whole(W)) {
          run2 <- ap_tr_run(p, nm, ap_round_up(W), method)
          if (!is.null(run2)) rounded <- run2$plan
        }
        list(run = run, run2 = run2, orig = run$plan, rounded = rounded,
             note = if (!is.null(run$s) && method == "nwc")
               "The north-west corner solution is only a starting solution. It ignores costs, so it is usually much more expensive than the optimal solution." else NULL)
      })
    }

    tr_server <- function(id, method, label) {
      res <- tr_method(method, label)
      ap_result_server(output, ns, id, res)
      output[[paste0(id, "_tr_note")]] <- renderUI({
        r <- res()
        if (method == "optimal") {
          nwc <- ap_tr_run(params(), periods(), tr_workforce(), "nwc")
          chp <- ap_tr_cheapest(ap_tr_setup(params(), tr_workforce()))
          lines <- list(sprintf("Tableau cost of the optimal solution (production, holding and unused regular time): %s.",
                                ap_num(r$run$tableau_cost)))
          if (!is.null(nwc)) lines <- c(lines, sprintf("North-west corner start: %s.", ap_num(nwc$tableau_cost)))
          if (!is.null(chp)) {
            s0 <- ap_tr_setup(params(), tr_workforce())
            same <- abs(ap_tr_cost(s0, chp) - r$run$tableau_cost) < 1e-6
            lines <- c(lines, if (same) "The cheapest-cell-first allocation is optimal."
                              else "Cheapest cell first alone is not optimal here, so the optimal allocation is shown.")
          }
          div(class = "ap-note", paste(lines, collapse = " "))
        } else {
          div(class = "ap-note", sprintf("Tableau cost of this solution (production, holding and unused regular time): %s.",
                                         ap_num(r$run$tableau_cost)))
        }
      })
      output[[paste0(id, "_tableau")]] <- renderTable(ap_tr_tableau(res()$run$s, res()$run$x, periods()))
      output[[paste0(id, "_costmat")]] <- renderTable(ap_tr_cost_matrix(res()$run$s, periods()))
      res
    }
    nwc_res <- tr_server("nwc", "nwc", "north-west corner")
    optimal_res <- tr_server("optimal", "optimal", "optimal")

    # ---------- Disaggregation ----------
    output$dg_families <- renderUI({
      nf <- input$dg_n
      req(nf)
      nf <- max(1, min(8, round(nf)))
      rows <- lapply(1:nf, function(i) {
        keep <- function(field, default) {
          v <- isolate(input[[paste0("fam_", field, i)]])
          if (is.null(v)) default else v
        }
        fluidRow(
          column(3, textInput(ns(paste0("fam_name", i)), if (i == 1) "Family name" else NULL,
                              value = keep("name", ap_fam_defaults$name[i]))),
          column(3, numericInput(ns(paste0("fam_share", i)), if (i == 1) "Demand share (%)" else NULL,
                                 value = keep("share", ap_fam_defaults$share[i]), min = 0)),
          column(3, numericInput(ns(paste0("fam_setup", i)), if (i == 1) "Setup cost per run" else NULL,
                                 value = keep("setup", ap_fam_defaults$setup[i]), min = 0)),
          column(3, numericInput(ns(paste0("fam_hold", i)), if (i == 1) "Holding cost (unit / period)" else NULL,
                                 value = keep("hold", ap_fam_defaults$hold[i]), min = 0))
        )
      })
      tagList(h4(class = "ap-h", "Product families"), rows)
    })

    families <- reactive({
      nf <- max(1, min(8, round(req(input$dg_n))))
      g <- function(field, i) input[[paste0("fam_", field, i)]]
      fam <- data.frame(
        name  = vapply(1:nf, function(i) { v <- g("name", i); if (is.null(v) || v == "") paste("Family", i) else v }, character(1)),
        share = vapply(1:nf, function(i) { v <- g("share", i); if (is.null(v) || is.na(v)) NA_real_ else v }, numeric(1)),
        setup = vapply(1:nf, function(i) { v <- g("setup", i); if (is.null(v) || is.na(v)) NA_real_ else v }, numeric(1)),
        hold  = vapply(1:nf, function(i) { v <- g("hold", i); if (is.null(v) || is.na(v)) NA_real_ else v }, numeric(1)),
        stringsAsFactors = FALSE
      )
      validate(need(!anyNA(fam[, -1]), "Fill in the share, setup cost and holding cost of every family."),
               need(all(fam$share >= 0) && sum(fam$share) > 0, "Demand shares must be non-negative and not all zero."),
               need(all(fam$setup > 0) && all(fam$hold > 0), "Setup costs and holding costs must be positive."))
      fam$name <- make.unique(fam$name)
      fam
    })

    dg_source <- reactive({
      r <- switch(input$dg_plan,
                  chase = chase_res(), level = level_res(), mixed = mixed_res(),
                  lp = lp_res(), optimal = optimal_res())
      if (isTRUE(input$dg_rounded) && !is.null(r$rounded)) r$rounded else r$orig
    })

    dg <- reactive({
      plan <- dg_source()
      p <- params()
      ap_disagg(plan$tab[["Total production"]], p$D, periods(), families())
    })

    output$dg_alloc <- renderTable({
      d <- dg()
      out <- data.frame(Period = periods(), `Aggregate production` = ap_num(rowSums(d$alloc)),
                        check.names = FALSE, stringsAsFactors = FALSE)
      for (j in seq_len(ncol(d$alloc))) out[[colnames(d$alloc)[j]]] <- ap_num(d$alloc[, j])
      out
    })

    output$dg_plot <- renderPlot({
      d <- dg()
      m <- t(d$alloc)
      colnames(m) <- periods()
      barplot(m, beside = FALSE, col = grDevices::hcl.colors(nrow(m), "Teal"),
              xlab = "Period", ylab = "Units", main = "Production by product family",
              legend.text = rownames(m), args.legend = list(x = "topleft", bty = "n"))
    })

    output$dg_summary <- renderTable({
      s <- dg()$summary
      for (j in 2:ncol(s)) s[[j]] <- ap_num(s[[j]])
      s
    })

    output$dg_runs <- renderTable({
      d <- dg()
      out <- data.frame(Period = periods(), stringsAsFactors = FALSE)
      for (j in seq_len(ncol(d$runs))) out[[colnames(d$runs)[j]]] <- ap_num(d$runs[, j])
      out
    })
  })
}
