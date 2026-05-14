# ============================================================
# Regime Classification — Full Data Collection Script 
# Features: VIX, Crude Oil, S&P500, Gold, DXY,
#           Yield Spread, EPU Index, GPRD Index
# Period  : 2000-01-01 to 2025-12-31
# ============================================================


# ---- Install & Load Libraries ----------------------------

packages <- c("quantmod", "fredr", "readr", "dplyr",
              "lubridate", "tidyr", "zoo", "httr",
              "readxl", "roll")

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)



# ---- 1. Global Settings -------------------------------------

start_date <- as.Date("2000-01-01")
end_date   <- as.Date("2025-12-31")

# FRED API Key :
FRED_API_KEY <- "1332bca09322ac38ad81fb9d2658bd0d"   
fredr_set_key(FRED_API_KEY)
cat("API Key set:", fredr_get_key(), "\n")
cat("Key length:", nchar(fredr_get_key()), "characters\n")  

# Test with a tiny fetch
test <- fredr(series_id = "DGS10",
              observation_start = as.Date("2024-01-01"),
              observation_end   = as.Date("2024-01-10"))
print(test)


# ============================================================
# BLOCK 1 — Yahoo Finance via quantmod
# Tickers : VIX, Crude Oil, S&P 500, Gold, DXY
# ============================================================

fetch_yahoo <- function(ticker, col_name, start, end) {
  tryCatch({
    env <- new.env()                        # isolated environment
    
    getSymbols(
      ticker,
      src         = "yahoo",
      from        = start,
      to          = end,
      auto.assign = TRUE,
      env         = env,
      warnings    = FALSE
    )
    
    obj       <- get(ls(env)[1], envir = env)   
    price_col <- tryCatch(Ad(obj), error = function(e) Cl(obj))
    
    df <- data.frame(
      Date  = index(price_col),
      Value = as.numeric(price_col),
      stringsAsFactors = FALSE
    )
    colnames(df)[2] <- col_name
    
    cat("Fetched:", ticker, "| Rows:", nrow(df), "\n")
    return(df)
    
  }, error = function(e) {
    cat("Failed:", ticker, "|", e$message, "\n")
    return(NULL)
  })
}

# --- Fetch each ticker ---
vix_df   <- fetch_yahoo("^VIX",      "vix_price",       start_date, end_date)
oil_df   <- fetch_yahoo("CL=F",      "crude_oil_price",  start_date, end_date)
sp500_df <- fetch_yahoo("^GSPC",     "sp500_price",      start_date, end_date)
gold_df  <- fetch_yahoo("GC=F",      "gold_price",       start_date, end_date)
dxy_df   <- fetch_yahoo("DX-Y.NYB",  "dxy_price",        start_date, end_date)


# ============================================================
# BLOCK 2 — FRED API
# Features: 10Y Treasury Yield, 2Y Treasury Yield → Spread
# ============================================================

fetch_fred <- function(series_id, col_name, start, end) {
  tryCatch({
    df <- fredr(
      series_id         = series_id,
      observation_start = start,
      observation_end   = end,
      frequency         = "d"
    ) %>%
      select(date, value) %>%
      rename(Date = date, !!col_name := value)
    
    cat(" FRED fetched:", series_id, "| Rows:", nrow(df), "\n")
    return(df)
    
  }, error = function(e) {
    cat("FRED failed:", series_id, "|", e$message, "\n")
    return(NULL)
  })
}

treasury_10y <- fetch_fred("DGS10", "yield_10y", start_date, end_date)
treasury_2y  <- fetch_fred("DGS2",  "yield_2y",  start_date, end_date)

# Compute 10Y - 2Y spread
yield_spread_df <- treasury_10y %>%
  inner_join(treasury_2y, by = "Date") %>%
  mutate(yield_spread = yield_10y - yield_2y) %>%
  select(Date, yield_spread)




# ============================================================
# BLOCK 3 — EPU Index (Fixed Column Name)
# ============================================================

epu_url  <- "https://www.policyuncertainty.com/media/US_Policy_Uncertainty_Data.xlsx"
epu_file <- tempfile(fileext = ".xlsx")

tryCatch({
  download.file(epu_url, destfile = epu_file, mode = "wb", quiet = TRUE)
  epu_raw <- read_excel(epu_file)
  
  cat(" EPU columns detected:", paste(colnames(epu_raw), collapse = ", "), "\n")
  
  epu_clean <- epu_raw %>%
    rename_with(~ tolower(gsub(" ", "_", .x))) %>%
    filter(!is.na(year) & !is.na(month)) %>%
    mutate(
      Date = as.Date(paste(
        as.integer(year),
        as.integer(month),
        "01", sep = "-"
      )),
      epu_index = as.numeric(news_based_policy_uncert_index)
    ) %>%
    filter(Date >= start_date, Date <= end_date) %>%
    select(Date, epu_index) %>%
    filter(!is.na(epu_index))
  
  # Verify data loaded
  cat(" EPU clean rows:", nrow(epu_clean), "\n")
  cat(" EPU date range:", format(min(epu_clean$Date)),
      "to", format(max(epu_clean$Date)), "\n")
  
  # Interpolate 
  all_days  <- data.frame(Date = seq(start_date, end_date, by = "day"))
  epu_daily <- all_days %>%
    left_join(epu_clean, by = "Date") %>%
    mutate(epu_index = na.approx(epu_index, na.rm = FALSE))
  
  cat(" EPU Index interpolated to daily | Rows:", nrow(epu_daily), "\n")
  
}, error = function(e) {
  cat(" EPU fetch failed:", e$message, "\n")
  epu_daily <<- data.frame(
    Date      = seq(start_date, end_date, by = "day"),
    epu_index = NA_real_
  )
})


# ============================================================
# BLOCK 4 — GPRD Index (Geopolitical Risk Index)
# Source  : Caldara & Iacoviello (matteoiacoviello.com)
# ============================================================

gprd_url  <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_daily_recent.xls"
gprd_file <- tempfile(fileext = ".xls")

tryCatch({
  download.file(gprd_url, destfile = gprd_file, mode = "wb", quiet = TRUE)
  gprd_raw <- read_excel(gprd_file)
  
  cat(" GPRD columns detected:", paste(colnames(gprd_raw), collapse = ", "), "\n")
  
  gprd_df <- gprd_raw %>%
    rename_with(tolower) %>%
    mutate(Date = as.Date(date)) %>%
    filter(Date >= start_date, Date <= end_date) %>%
    mutate(gprd_index = do.call(coalesce, pick(any_of(c("gpr", "gprd", "gpr_index"))))) %>%
    mutate(gprd_index = as.numeric(gprd_index)) %>%
    select(Date, gprd_index) %>%
    filter(!is.na(gprd_index))
  
  cat(" GPRD Index loaded | Rows:", nrow(gprd_df), "\n")
  
}, error = function(e) {
  cat(" GPRD fetch failed:", e$message, "\n")
  gprd_df <<- data.frame(
    Date       = seq(start_date, end_date, by = "day"),
    gprd_index = NA_real_
  )
})


# ============================================================
# BLOCK 5 — Build Date Spine & Merge All Features
# ============================================================

# Trading day spine (Mon–Fri)
date_spine <- data.frame(
  Date = seq(start_date, end_date, by = "day")
) %>%
  filter(!weekdays(Date) %in% c("Saturday", "Sunday"))

cat("\n Date spine rows (trading days):", nrow(date_spine), "\n")

# Left join all features onto spine
master_df <- date_spine %>%
  left_join(vix_df,         by = "Date") %>%
  left_join(oil_df,         by = "Date") %>%
  left_join(sp500_df,       by = "Date") %>%
  left_join(gold_df,        by = "Date") %>%
  left_join(dxy_df,         by = "Date") %>%
  left_join(yield_spread_df, by = "Date") %>%
  left_join(epu_daily,      by = "Date") %>%
  left_join(gprd_df,        by = "Date")

cat(" All features merged | Shape:",
    nrow(master_df), "rows x", ncol(master_df), "cols\n")


# ============================================================
# BLOCK 6 — Handle Missing Values
# ============================================================

# Step 1: Forward-fill (carry last known value across holidays/gaps)
master_df <- master_df %>%
  arrange(Date) %>%
  mutate(across(where(is.numeric), ~ na.locf(.x, na.rm = FALSE)))

# Step 2: Backward-fill (catch leading NAs at start of series)
master_df <- master_df %>%
  mutate(across(where(is.numeric), ~ na.locf(.x, fromLast = TRUE, na.rm = FALSE)))

# Step 3: NA report
na_report <- colSums(is.na(master_df))
cat("\n Remaining NAs after fill:\n")
print(na_report)


# ============================================================
# BLOCK 7 — Feature Engineering
# ============================================================

master_df <- master_df %>%
  arrange(Date) %>%
  mutate(
    
    # --- Returns ---
    sp500_log_return  = c(NA, diff(log(sp500_price))),
    oil_log_return    = c(NA, diff(log(crude_oil_price))),
    gold_log_return   = c(NA, diff(log(gold_price))),
    
    # --- Rolling 30-day Volatility (std of log returns) ---
    sp500_vol_30d     = roll_sd(sp500_log_return,  width = 30),
    vix_ma_30d        = roll_mean(vix_price,        width = 30),
    oil_vol_30d       = roll_sd(oil_log_return,     width = 30),
    
    # --- Ratios ---
    gold_oil_ratio    = gold_price / crude_oil_price,  # safe-haven vs demand
    vix_epu_ratio     = vix_price  / epu_index,        # market vs policy fear
    
    # --- Regime signals ---
    yield_curve_flag  = ifelse(yield_spread < 0, 1, 0), # 1 = inverted (recession signal)
    vix_stress_flag   = ifelse(vix_price > 30,  1, 0)   # 1 = high stress (>30 = fear zone)
  )

cat(" Feature engineering complete |",
    ncol(master_df), "total columns\n")


# ============================================================
# BLOCK 8 — Final Summary & Export
# ============================================================

cat("\n", strrep("=", 55), "\n")
cat(" FINAL DATASET SUMMARY\n")
cat(strrep("=", 55), "\n")
cat("Date range :", format(min(master_df$Date)), "to",
    format(max(master_df$Date)), "\n")
cat("Rows       :", nrow(master_df), "\n")
cat("Columns    :", ncol(master_df), "\n")
cat("\nColumn list:\n")
print(colnames(master_df))
cat("\nHead of dataset:\n")
print(head(master_df, 5))

# Export CSV
output_file <- "regime_data_2000_2025.csv"
write_csv(master_df, output_file)
cat("\n Dataset saved to:", output_file, "\n")