
ebs.run.setup <- function(bal,unbal,years){
  scene  <- rsim.scenario(bal, unbal, years=years)
  fyears <- years[-1]
  for (species in all.species){
    scene     <- adjust.forcing(scene, "ForcedMort", species, 
                   sim.year = fyears, value=runif(length(fyears), 0.5, 1.5))
  } 

  scene <- adjust.fishing(scene, "ForcedEffort", "Fisheries", sim.year = fyears, value=0)   
  scene <- adjust.forcing(scene, "ForcedBio", "Discards.offal",
             sim.year = fyears, value=ebs.balanced$Biomass["Discards.offal"]) 
  FF <- as.vector((ebs.balanced$Landings+ebs.balanced$Discards)/(ebs.balanced$Biomass)) # note needs to be rowSums for multiple fisheries
  names(FF)<-ebs.balanced$Group
  for (species in all.species){
    scene <- adjust.fishing(scene, "ForcedFRate", species, sim.year = fyears, value=FF[species])
  }  
  return(scene)
  
}
  
  

fishery_output <- function(f_rates, prices, costs, years, rsim.scene, verbose=F){
  species_list <- names(f_rates); 
  final_year   <- as.character(years[length(years)])
  
  # Loop through species and apply f rates of each to the scenario 
  for (p in 1:length(f_rates)){
    rsim.scene <- adjust.fishing(rsim.scene, "ForcedFRate", species_list[p], 
                                 sim.year = years, value=f_rates[p])
  }
  
  # Run the model
  res <- rsim.run(rsim.scene, method="AB", years=years)
  
  # Extract landings from results
  landings <- res$annual_Catch[final_year,species_list]
  
  # This is the output to maximize
  output <- sum (495000000*(landings*prices) - (f_rates*costs))
  
  # verbose=TRUE returns the full run time series (default False)
  # otherwise just return the function value
  if (verbose){return(res)} else{return(output)}
}


## Rsim indicators function  
rsim.indicators<-function(run, bal, species, years){
  if (length(years)<2){print("Minimum of 2 years needed to calculate indicators"); return()}
  catchmat <- t(run$annual_Catch[as.character(years),species])
  biomat   <- t(run$annual_Biomass[as.character(years),species])
  qbmat    <- t(run$annual_QB[as.character(years),species])
  agemat   <- 1.0/(qbmat * ifelse(bal$GE[species]>0,bal$GE[species],1)) 
  
  tot_bio       <- colSums(biomat)
  TL_bio        <- colSums(biomat * bal$TL[species] )/tot_bio 
  age_bio       <- colSums(biomat * agemat          )/tot_bio 
  diversity_bio <- biomat/tot_bio 
  
  tot_catch  <- colSums(catchmat)
  TL_catch   <- colSums(catchmat * bal$TL[species] )/tot_catch
  age_catch  <- colSums(catchmat * agemat          )/tot_catch
  
  return(list(tot_bio=tot_bio, TL_bio=TL_bio, age_bio=age_bio, diversity_bio=diversity_bio, 
              tot_catch=tot_catch, TL_catch=TL_catch, age_catch=age_catch)) 
}




  
  
  
  



inds<- rsim.indicators(run.noise, ebs.balanced,  rpath.living(ebs.balanced), 1990:2089) 



# rsim.state.indicators <- function(run, bal, species){
#   Biomass   <- run$end_state$Biomass
#   Mortality <- run$dyt$LossPropToB
#   TL        <- c(0,bal$TL)
#   
#   years <- row.names(run$annual_Biomass)
#   last <- as.character(years[length(years)])
#   
#   catchmat <- run$annual_Catch[last,species]
#   biomat   <- run$annual_Biomass[last,species]
# }






years<-1990:2089

species <- as.character(bal$Group[1:bal$NUM_LIVING+bal$NUM_DEAD])

species<-c("P.cod","W.pollock")





