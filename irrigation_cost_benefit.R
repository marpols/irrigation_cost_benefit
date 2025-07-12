lapply(list.files("src", pattern = ".R", full.names = T), source)

outdir <- rstudioapi::selectDirectory()
outdir <- "~/Research/Project - Irrigation Assessment/Results - 20250706"

javastics_path <- "C:\\Users\\marpo\\Documents\\Research\\STICS\\JavaSTICS-1.5.3-STICS-10.3.0"
workspace <- "irrigation_assessment"

usms_path <- file.path(javastics_path, workspace, "usms.xml")
usms <- get_usms_list(file = usms_path)
usm_files <- get_usms_files(file.path(javastics_path,
                                      workspace),
                            javastics=javastics_path)

#get climate data
load.climate()

#sims grouped by weather station x soil type
hills_sims <- get.mods.files("RESULTS", version = "hills")
hills2_sims <- get.mods.files("RESULTS", version = "hills2")
hills3_sims <- get.mods.files("RESULTS", version = "hills3")
hills4_sims <- get.mods.files("RESULTS", version = "hills4")

organise.data(hills_sims, fname = "yield")
organise.data(hills2_sims, fname = "yield")
organise.data(hills3_sims, fname = "yield")
organise.data(hills4_sims, fname = "yield")

sim_start <- unique(get_param_xml(file = usms_path, "datedebut")[[1]][[1]])
sim_end <- unique(get_param_xml(file = usms_path, "datefin")[[1]][[1]])


data_summary <- readRDS("data/hills_yield_summary.RDS")

cost.benefit.data(data_summary, "gross_benefit")



#WB TEST--------------------------------------------------
df <- hills_sims[[1]][["EP_ARY"]][[1]] |> filter(mo %in% c(6:9))
stn <- "EAST_POINT_(AUT)"
df <- data.table::as.data.table(df)

x <- df[mo == 6 & jo == 1,11:15] |> unlist()
initial_water <- sum(x)
y <- df[mo == 9 & jo == 30,11:15] |> unlist()
final_water <-  sum(y)
ETa <- (df[mo == 9 & jo == 30, 21] - df[mo == 6 & jo == 1, 21]) |> unlist()
hist <- data.table::as.data.table(historical)
cum_precip_e <- sum(df[,precip])
cum_precip <- hist[station == stn & period == "GS", 3] |> unlist()
water_loss <- cum_precip - cum_precip_e


WB = (cum_precip + initial_water) - (final_water + ETa + water_loss)


#monthly WB

df_wb <- df |> group_by(mo) |>
  summarise(ETa = sum(et),
            cum_precip_e = sum(precip),
  )


df_summary <- df |>
  # Step 1: Combine year, month, day into a proper Date column
  mutate(date = make_date(ian, mo, jo)) |>
  
  # Step 2: Add year-month group for monthly aggregation
  mutate(year_month = floor_date(date, "month")) |>
  
  # Step 3: Filter rows that are first or last date in each month
  group_by(year_month) |>
  filter(date == min(date) | date == max(date)) |>
  ungroup() |>
  
  # Step 4: Sum selected columns row-wise (adjust col1:col5 if needed)
  rowwise() |>
  mutate(sum_5_cols = sum(c_across(11:15), na.rm = TRUE)) |>
  
  # Step 5: Return relevant columns
  select(date, sum_5_cols)

monthly <- data.table::as.data.table(monthly)
june <- monthly[mo == 6 & ian == 2001 & station == stn, precip.cum]
july <-monthly[mo == 7 & ian == 2001 & station == stn, precip.cum]
aug <-monthly[mo == 8 & ian == 2001 & station == stn, precip.cum]
sept <- monthly[mo == 9 & ian == 2001 & station == stn, precip.cum]

june_wl <- june - df_wb[[1, 3]]
july_wl <- july - df_wb[[2, 3]] 
aug_wl <- aug - df_wb[[3, 3]] 
sept_wl <- sept - df_wb[[4, 3]] 

WB_june = (june + df_summary[[1,2]]) - (df_summary[[2,2]] + df_wb[[1,2]] + june_wl)
WB_july = (july + df_summary[[3,2]]) - (df_summary[[4,2]] + df_wb[[2,2]] + july_wl)
WB_aug = (aug + df_summary[[5,2]]) - (df_summary[[6,2]] + df_wb[[3,2]] + aug_wl)
WB_sept = (sept + df_summary[[7,2]]) - (df_summary[[8,2]] + df_wb[[4,2]] + sept_wl)


WB_june
WB_july
WB_aug
WB_sept

x <- hills_sims[[1]]
