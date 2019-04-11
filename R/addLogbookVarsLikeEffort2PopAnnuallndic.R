

#' Link annual indic to effort
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' \dontrun{
#' general <- setGeneralOverallVariable(main_path_outputs =file.path("C:","DISPLACE_outputs"),
#'                                       case_study="DanishFleet",
#'                                       igraph=41,
#'                                       a.year="2015",
#'                                       a.country="DEN",
#'                                       nbpops=39,
#'                                       nbszgroup=14,
#'                                       namefolderinput="DanishFleet",
#'                                       the_scenarios= c("svana_baseline",
#'                                                       "svana_sub1mx20",
#'                                                       "svana_sub4mx20",
#'                                                       "svana_sub4mx5ns20bt",
#'                                                       "svana_sub4mx20ns5bt",
#'                                                       "svana_sub4mx5ns5bt" ),
#'                                       nbsimus=20
#'                                       )
#'
#'  
#'  
#' loadLoglikeFiles (general, use_port_info=FALSE)
#'
#'
#'  
#'
#' displace_annual_indic_per_pop_year <- addLogbookVarsLikeEffort2PopAnnualIndic(general)
#'
#'
#'  
#'
#'  }






addLogbookVarsLikeEffort2PopAnnualIndic <- function(general, a_variable="effort", nby=6)
{

  mat_allsce_y <- NULL
  for (sce in c(general$namefolderoutput)){

   
     obj        <- get(paste("lst_loglike_agg_weight", "_all_", sce, sep=''), env=.GlobalEnv) 

     # caution: complete missing records if 0s in given year.month
     complete_all_year_month <- function (x, years_span=2015:(2015+nby-1)){ 
      allcombi              <- expand.grid(month=sprintf("%02d", 1:12), year=years_span)
      allcombi$year.month   <- paste0(allcombi$year,".",allcombi$month)
      allcombi              <- cbind.data.frame(year.month=allcombi$year.month,  matrix(0, ncol=ncol(x)-1))
      colnames(allcombi)    <- colnames(x)
      allmissingcombi       <- allcombi[!allcombi$year.month %in% x$year.month,]
      dd <- rbind.data.frame(x, allmissingcombi)
      rownames(dd) <- dd$year.month
      dd <- dd[as.character(allcombi$year.month),] # get the right order...
     return(dd)
    } 
    obj  <- lapply(obj, complete_all_year_month)


   # per month
   simu_names <-  names(obj)    
   mat_sce <- matrix(NA, nrow=length(simu_names),  ncol=nby*12) 
   rownames(mat_sce) <- simu_names
   colnames(mat_sce) <- paste0(rep(paste0("y",1:nby), each=12), "_", 1:12)
   for (sim in simu_names){
    mat_sce[sim, ] <- as.numeric(obj[[sim]][,a_variable]) [1:dim(mat_sce)[2]]
    }
    

   # per year
   if(nby==5) mat_sce_y <- cbind.data.frame(
      y1 =apply(mat_sce[,c(1:12)], 1, sum),  
      y2 =apply(mat_sce[,c(13:24)], 1, sum),  
      y3 =apply(mat_sce[,c(25:36)], 1, sum),  
      y4 =apply(mat_sce[,c(37:48)], 1, sum),  
      y5 =apply(mat_sce[,c(49:60)], 1, sum)  
   )
   if(nby==6) mat_sce_y <- cbind.data.frame(
      y1 =apply(mat_sce[,c(1:12)], 1, sum),  
      y2 =apply(mat_sce[,c(13:24)], 1, sum),  
      y3 =apply(mat_sce[,c(25:36)], 1, sum),  
      y4 =apply(mat_sce[,c(37:48)], 1, sum),  
      y5 =apply(mat_sce[,c(49:60)], 1, sum),  
      y6 =apply(mat_sce[,c(61:72)], 1, sum)  
   )

   # reshape in long
   mat_sce_y <- reshape(mat_sce_y, direction="long", varying=list(names(mat_sce_y)), v.names="effort", idvar="simu", timevar="year", times=1:6)
   mat_sce_y$simu <- paste0("simu",mat_sce_y$simu)        

   # bind sce
   mat_allsce_y <- rbind.data.frame(
                                       mat_allsce_y,
                                       cbind.data.frame(mat_sce_y, sce=sce)
                                       )  

} # end sce
 

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# ON THE OTHER SIDE, get the pop variables outcomes
mat_allsce_pop_y <- NULL
for (sce in c(general$namefolderoutput)){

  for(sim in simu_names) {
    print(paste(sim))
    # merge all infos
               annual_indics              <-  read.table (file=file.path(general$main.path, 
                                                 general$namefolderinput, sce, paste('popdyn_annual_indic_',sim,".dat", sep='')))
               colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg",
                                                 "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))

               mat_allsce_pop_y <- rbind (mat_allsce_pop_y, cbind(annual_indics, sce=sce, simu=sim))
     }
   } # end sce

# add a y code
mat_allsce_pop_y$year <- factor(mat_allsce_pop_y$tstep) # init
levels(mat_allsce_pop_y$year) <- 1:nby




  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# bind both data for this sce
displace_annual_indic_per_pop_year <- merge( mat_allsce_pop_y, mat_allsce_y)

# export
write.table(displace_annual_indic_per_pop_year, 
                                 file=file.path(general$main.path, general$namefolderinput, 
                                                "displace_annual_indic_per_pop_year.txt"), sep=";", row.names=FALSE, col.names=TRUE, quote=FALSE)
  



return(displace_annual_indic_per_pop_year)
}  



