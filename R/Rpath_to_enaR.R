#' from Rpath model object, create network object 
#' network object can be used as input for enaR functions
#'
#' Rpath::ena_translate
#'
#' @param Rpath R object containing a balanced \code{Rpath} model
#' @param Rpath.params Rpath parameter object
#' @param grossPP numeric value of gross primary production #will need to adjust to allow for multiple pp groups
#'
#' @return enaR network object
#'
#' @export

ena_translate <- function(Rpath, Rpath.params, grossPP = NA){
  
  #Set up model with group names and types
  groups<-as.vector(Rpath$Group)
  
  #Count number of each group type
  nliving <- nrow(Rpath.params$model[Type <  2, ])
  ndead   <- nrow(Rpath.params$model[Type == 2, ])
  
  #Find index of pp groups
  pp <- which(Rpath.params$model[Type == 1, ])
  
  #Pull diet matrix
  diet<-Rpath$DC
  
  #Get consumption values by DC*QB*Biomass
  QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
  for (j in 1:nliving){
    QQ[,j]<-diet[,j]*Rpath$QB[j]*Rpath$Biomass[j]
  }
  
  #Ignore Imports
  QQ<-QQ[1:(nliving+ndead),]
  colnames(QQ)<-groups[1:nliving]
  rownames(QQ)<-groups[1:(nliving+ndead)]
  
  #Calculate flow to detritus
  M0<-Rpath$PB*(1-Rpath$EE)
  Detritus<-(M0*Rpath$Biomass+Rpath$QB*Rpath$Biomass*Rpath$Unassim)*Rpath$DetFate[,1]
  Detritus<-Detritus[1:(nliving+ndead)]
  #Flow to detritus from detritus = 0
  Detritus[(nliving+1)]<-0
  #Bind diet matrix (QQ) with flow to detritus, discards
  QQ<-cbind(QQ,Detritus)
  
  #Calculate exports
  #First sum catch
  Catch<-rowSums(Rpath$Landings)
  #Add positive biomass accumulation terms
  Export<-Catch+(ifelse(Rpath$BA>0,Rpath$BA,0))
  Export<-Export[1:(nliving+ndead)]
  for (i in 1:ndead){
    Export[nliving+i]<-Rpath$PB[(nliving+i)]*Rpath$Biomass[(nliving+i)]
  }
  
  #Calculate respiration
  #Assume detritus, discards have 0 respiration
  Resp<-((1-Rpath$Unassim)*Rpath$QB-Rpath$PB)*Rpath$Biomass
  Resp<-ifelse(Resp>0,Resp,0)
  Resp<-Resp[1:(nliving+ndead)]
  Resp[(nliving+1):(nliving+ndead)]<-0
  #Deal with Primary Production
  #First, estimate GROSS production = Imports
  #P/B in Rpath model gives NET production
  Resp[pp]<-gross-(Rpath$PB[pp]*Rpath$Biomass[pp])
  #Calculate imports
  #Negative biomass accumulation terms
  #Gross primary production
  Import<-abs(ifelse(Rpath$BA<0,Rpath$BA,0))
  Import[pp]<-gross
  Import<-Import[1:(nliving+ndead)]
  #Trim biomass
  Biomass<-Rpath$Biomass[1:(nliving+ndead)]
  #Pack the model directly and store
  orig.network<-enaR::pack(flow = QQ,
                           input = Import,
                           export = Export,
                           living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                           respiration = Resp,
                           storage = Biomass)
}