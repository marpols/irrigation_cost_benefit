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



