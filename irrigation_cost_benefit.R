lapply(c("src/packages.R","src/files.R", "src/costs.R",
         "src/water.R", "src/climate.R"), source)

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

# #sims grouped by soil type
# soils_hills_sims <- get.mods.files("RESULTS", 
#                                  version = "hills", 
#                                  group_by = c("ARY","CLO", "CTW"))
# soils_hills2_sims <- get.mods.files("RESULTS", 
#                                   version = "hills2", 
#                                   group_by = c("ARY","CLO", "CTW"))
# soils_hills3_sims <- get.mods.files("RESULTS", 
#                                   version = "hills3", 
#                                   group_by = c("ARY","CLO", "CTW"))
# soils_hills4_sims <- get.mods.files("RESULTS", 
#                                   version = "hills4", 
#                                   group_by = c("ARY","CLO", "CTW"))
# 
# #sims grouped by weather station
# stn_hills_sims <- get.mods.files("RESULTS", 
#                                    version = "hills", 
#                                    group_by = c("S", "HCC", "EP", "NG"))
# stn_hills2_sims <- get.mods.files("RESULTS", 
#                                    version = "hills2", 
#                                    group_by = c("S", "HCC", "EP", "NG"))
# stn_hills3_sims <- get.mods.files("RESULTS", 
#                                     version = "hills3", 
#                                     group_by = c("S", "HCC", "EP", "NG"))
# stn_hills4_sims <- get.mods.files("RESULTS", 
#                                     version = "hills4", 
#                                     group_by = c("S", "HCC", "EP", "NG"))
# 
# 
# #sims grouped by year
# yearly_hills_sims <- get.mods.files("RESULTS", 
#                                     version = "hills", 
#                                     group_by = c(2001:2024))
# yearly_hills2_sims <- get.mods.files("RESULTS", 
#                                     version = "hills2", 
#                                     group_by = c(2001:2024)) 
# yearly_hills3_sims <- get.mods.files("RESULTS", 
#                                     version = "hills3", 
#                                     group_by = c(2001:2024)) 
# yearly_hills4_sims <- get.mods.files("RESULTS", 
#                                     version = "hills4", 
#                                     group_by = c(2001:2024)) 


sim_start <- unique(get_param_xml(file = usms_path, "datedebut")[[1]][[1]])
sim_end <- unique(get_param_xml(file = usms_path, "datefin")[[1]][[1]])



#### Edit to do the following for each usm group:

#calculate yearly marketable yields
stress_mrkt <- lapply(stress, calc.market.yield)
no_stress_mrkt <- lapply(no_stress, calc.market.yield)

#calculate yearly yield gains
gains <- yield.gain(stress_mrkt, no_stress_mrkt)

#calculate costs
irrigation_types <- irrigation.costs()
irrigation_costs <- list()

for(type in irrigation_types){
  costs <- lapply(gains, calc.yearly.costs, type = type)
  costs$total.asset <- type$total.asset
  irrigation_costs[[{{type$type}}]] <- costs
}

#calculate gross benefit: rise in marketable yield by SI * potato sale price
gross_benefit <- lapply(gains, calc.yearly.earnings)

total_costs <- lapply(irrigation_costs, sum.total)
total_gross_benefit <- sum.total(gross_benefit)

annual_net_benefit <- Map(function(c, n) {
  net_benefit <- total_gross_benefit - c
  net_benefit / 21
}, total_costs, names(total_costs))


#calculate crop water requirement deficit
water_reqs <- lapply(stress, calc.water.requirements)

water_reqs_rs <- lapply(stress, calc.water.requirements, method = "RZ")
