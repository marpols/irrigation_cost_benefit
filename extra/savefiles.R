
objlist <- tibble::lst(climate_hist, climate_proj, climate_REF,
     climhist_summary, climproj_summary, climREF_sum,
     climind_hist, climind_proj, climind_REF,
     data_hist$fixed$analysis, data_hist$dynamic$analysis,
     data_proj$fixed$analysis, data_proj$dynamic$analysis,
     data_REF,
     hist_fixed$noirr, hist_fixed$autoirr,
     hist_dynamic$noirr, hist_dynamic$autoirr,
     proj_fixed$noirr, proj_fixed$autoirr,
     proj_dynamic$noirr, proj_fixed$autoirr,
     REF,
     results_hist$fixed$pbps, results_hist$dynamic$pbps,
     results_proj$fixed$pbps, results_proj$dynamic$pbps,
     results_histext$fixed$pbps, results_histext$dynamic$pbps,
     results_projext$fixed$pbps, results_projext$dynamic$pbps,
     hist_SPEI, hist_SPEI2,
     proj_SPEI, REF_SPEI,
     nyrs_hist, nyrs_hist2,
     nyrs_proj, nyrs_REF,
     hist_fixed_spei2,
     hist_dynamic_spei2,
     histtbl$fixed, histtbl$dynamic,
     projtbl$fixed, projtbl$dynamic)

objlist2 <- tibble::lst(hist_SPEI2, nyrs_hist2, histtbl2$dynamic, histtbl2$fixed)

purrr::iwalk(objlist2, \(dt, nm) {
  fname <- stringr::str_replace_all(nm, "\\$", "_")
  data.table::fwrite(dt, sprintf("./data/%s.csv",
                                 fname))
  saveRDS(dt, sprintf("./data/%s.RData",
                  fname))
})

