#' Compares the simulated fire size distribution with the historical fire size distribution
#' @description Combines and calculates the RMSE and pearson correlation between all the durations simulated and the historical fire size distribution.
#'
#' @param Folder.Outputs Path to the folder containing the outputs of the FConstMTT runs.
#' @param intervals Numerical vector with the intervals of fire size to be considered in the comparison between simulated and the historical fire size distribution.
#' @param all.dist Logical. If true, then all the fire size distribution is used to calculate the RMSE and the correlation. If false, then the fire size distribution will only be considered starting from the first numeric value identified in intervals.
#' @param hist.fire.sizes Text file (csv) containing the historical fire size. If the function get_fire_weather was used in the process, then the file to be used should be “summary fire size.csv”.
#' @param freqs.durclass Text file (csv) with the relative frequency of each meteorological and fuel map scenario used. If the function Gen_ign was used in the process, then the file to be used here should be “clusters_freqs_final.csv”, which is located in the ignition folder.
#'
#' @return Returns figures showing the historical and simulated fire size distribution for all the combinations. Also saves a csv file containing the RMSE and person correlation of each combination of durations and the historical fire size.
#' @export
#' @import Metrics ie2misc forestmangr
#'
#' @examples
#'\dontrun{evaluate_fire_size(Folder.Outputs="C:/user/fconstmtt/Outputs",
#'intervals=c(100,250,500,750,1000,2500,5000,10000),all.dist=FALSE,
#'hist.fire.sizes="C:/user/summary fire size.csv",
#'freqs.durclass="C:/user/results/ignitions/clusters_freqs_final.csv")}
#'
evaluate_fire_size <-  function (Folder.Outputs,
                                 intervals,all.dist,
                                 hist.fire.sizes,freqs.durclass,
                                 plot.all){

  setwd(Folder.Outputs)
  my_files <- list.files(pattern = "\\.FireList$")


  datalist <- list()
  results <- data.frame(matrix(NA, nrow = 1000000, ncol = 2))

  for (i in 1:length(my_files)) {
    t <- read.table(my_files[i], sep=",", header = T)
    name_file <- as.data.frame(my_files[i])
    colnames(name_file) <- "name"
    combo_id_temp <- sub("_cluster.*", "", name_file$name)
    combo_id <- sub(".*cluster_", "", name_file$name)
    combo_id2 <- sub("_land_.*", "", combo_id)
    combo_id3 <- stringr::str_sub(combo_id2, start = 3, end=12)
    combo_id4 <- sub(".*_durclass_", "", combo_id2)
    combo_id5 <- sub("_.*", "", combo_id4)
    combo_id6 <- paste("durclass_",combo_id5,sep="")

    combo_use <- paste(combo_id_temp,combo_id6,sep="_")

    if (nrow(t)>0){
      size <- as.data.frame(t [,4])/2.471 #from acres to hectares
    } else {
      size <- 0
    }

    row_numbers <- nrow(t)
    datalist[i] <- (nrow(t))
    NonNAindex <- which(is.na(results))
    start_row <- min(NonNAindex)
    end_row <- start_row + row_numbers - 1

    if(end_row < start_row){
      end_row <- start_row
    }

    results[start_row:end_row,1]<- size
    results[start_row:end_row,2]<- combo_use
  }


  colnames(results) <- c("size","combo")

  results<- results[complete.cases(results), ]


  results_inter <- data.frame(matrix(NA, nrow = 1000, ncol = 5))

  n_unique<-unique(results$combo)


  if (all.dist==TRUE){

    for (j in 1:length(n_unique)){
      name <- n_unique[j]
      durval_name <- sub("_durclass_.*", "", name)
      durclass_name <- substrRight(name, 10)

      results_i <- subset(results, combo==n_unique[j])


      if (is.na(intervals[1])) {
        int_1 <- NA } else {
          int_1 <- sum(results_i$size < intervals[1])/nrow(results_i)
        }


      if (is.na(intervals[2])) {
        int_2 <- NA } else {
          int_2 <- sum(results_i$size >= intervals[1] & results_i$size < intervals[2])/nrow(results_i)
        }


      if (is.na(intervals[3])) {
        int_3 <- NA } else {
          int_3 <- sum(results_i$size >= intervals[2] & results_i$size < intervals[3])/nrow(results_i)
        }

      if (is.na(intervals[4])) {
        int_4 <- NA } else {
          int_4 <-sum(results_i$size >= intervals[3] & results_i$size < intervals[4])/nrow(results_i)
        }


      if (is.na(intervals[5])) {
        int_5 <- NA } else {
          int_5 <-sum(results_i$size >= intervals[4] & results_i$size < intervals[5])/nrow(results_i)
        }


      if (is.na(intervals[6])) {
        int_6 <- NA } else {
          int_6 <-sum(results_i$size >= intervals[5] & results_i$size < intervals[6])/nrow(results_i)
        }


      if (is.na(intervals[7])) {
        int_7 <- NA } else {
          int_7 <-sum(results_i$size >= intervals[6] & results_i$size < intervals[7])/nrow(results_i)
        }


      if (is.na(intervals[8])) {
        int_8 <- NA } else {
          int_8 <-sum(results_i$size >= intervals[7] & results_i$size < intervals[8])/nrow(results_i)
        }



      if (is.na(intervals[9])) {
        int_9 <- NA } else {
          int_9 <-sum(results_i$size >= intervals[8] & results_i$size < intervals[9])/nrow(results_i)
        }




      if (is.na(intervals[10])) {
        int_10 <- NA } else {
          int_10 <-sum(results_i$size >= intervals[9] & results_i$size < intervals[10])/nrow(results_i)
        }




      if (is.na(intervals[11])) {
        int_11 <- NA } else {
          int_11 <-sum(results_i$size >= intervals[10] & results_i$size < intervals[11])/nrow(results_i)
        }




      if (is.na(intervals[12])) {
        int_12 <- NA } else {
          int_12 <-sum(results_i$size >= intervals[11] & results_i$size < intervals[12])/nrow(results_i)
        }



      if (is.na(intervals[13])) {
        int_13 <- NA } else {
          int_13 <-sum(results_i$size >= intervals[12] & results_i$size < intervals[13])/nrow(results_i)
        }


      if (is.na(intervals[14])) {
        int_14 <- NA } else {
          int_14 <-sum(results_i$size >= intervals[13] & results_i$size < intervals[14])/nrow(results_i)
        }


      if (is.na(intervals[15])) {
        int_15 <- NA } else {
          int_15 <-sum(results_i$size >= intervals[14] & results_i$size < intervals[15])/nrow(results_i)
        }

      if (is.na(length(intervals))) {
        int_last <- NA } else {
          int_last <-sum(results_i$size >= intervals[length(intervals)])/nrow(results_i)
        }



      results_i_final <- as.data.frame(rbind(int_1,int_2,int_3,int_4,int_5,
                                             int_6,int_7,int_8,int_9,int_10,
                                             int_11,int_12,int_13,int_14,int_15,
                                             int_last))
      results_i_final$combo <- j
      results_i_final$durclass_name <- durclass_name
      results_i_final$durval_name <- durval_name

      results_i_final<- results_i_final[complete.cases(results_i_final), ]
      results_i_final$x_axis <- 1:nrow(results_i_final)

      row_numbers <- nrow(results_i_final)
      NonNAindex <- which(is.na(results_inter))
      start_row <- min(NonNAindex)
      end_row <- start_row + row_numbers - 1
      results_inter[start_row:end_row,]<- results_i_final
    }
  }



  if (all.dist==FALSE){

    results_use <- subset(results,size>=intervals[1])

    for (j in 1:length(n_unique)){
      name <- n_unique[j]
      durval_name <- sub("_durclass_.*", "", name)
      durclass_name <- substrRight(name, 10)

      results_i <- subset(results_use, combo==n_unique[j])


      if (is.na(intervals[1])) {
        int_1 <- NA } else {
          int_1 <- sum(results_i$size < intervals[1])/nrow(results_i)
        }


      if (is.na(intervals[2])) {
        int_2 <- NA } else {
          int_2 <- sum(results_i$size >= intervals[1] & results_i$size < intervals[2])/nrow(results_i)
        }


      if (is.na(intervals[3])) {
        int_3 <- NA } else {
          int_3 <- sum(results_i$size >= intervals[2] & results_i$size < intervals[3])/nrow(results_i)
        }

      if (is.na(intervals[4])) {
        int_4 <- NA } else {
          int_4 <-sum(results_i$size >= intervals[3] & results_i$size < intervals[4])/nrow(results_i)
        }


      if (is.na(intervals[5])) {
        int_5 <- NA } else {
          int_5 <-sum(results_i$size >= intervals[4] & results_i$size < intervals[5])/nrow(results_i)
        }


      if (is.na(intervals[6])) {
        int_6 <- NA } else {
          int_6 <-sum(results_i$size >= intervals[5] & results_i$size < intervals[6])/nrow(results_i)
        }


      if (is.na(intervals[7])) {
        int_7 <- NA } else {
          int_7 <-sum(results_i$size >= intervals[6] & results_i$size < intervals[7])/nrow(results_i)
        }


      if (is.na(intervals[8])) {
        int_8 <- NA } else {
          int_8 <-sum(results_i$size >= intervals[7] & results_i$size < intervals[8])/nrow(results_i)
        }



      if (is.na(intervals[9])) {
        int_9 <- NA } else {
          int_9 <-sum(results_i$size >= intervals[8] & results_i$size < intervals[9])/nrow(results_i)
        }




      if (is.na(intervals[10])) {
        int_10 <- NA } else {
          int_10 <-sum(results_i$size >= intervals[9] & results_i$size < intervals[10])/nrow(results_i)
        }




      if (is.na(intervals[11])) {
        int_11 <- NA } else {
          int_11 <-sum(results_i$size >= intervals[10] & results_i$size < intervals[11])/nrow(results_i)
        }




      if (is.na(intervals[12])) {
        int_12 <- NA } else {
          int_12 <-sum(results_i$size >= intervals[11] & results_i$size < intervals[12])/nrow(results_i)
        }



      if (is.na(intervals[13])) {
        int_13 <- NA } else {
          int_13 <-sum(results_i$size >= intervals[12] & results_i$size < intervals[13])/nrow(results_i)
        }


      if (is.na(intervals[14])) {
        int_14 <- NA } else {
          int_14 <-sum(results_i$size >= intervals[13] & results_i$size < intervals[14])/nrow(results_i)
        }


      if (is.na(intervals[15])) {
        int_15 <- NA } else {
          int_15 <-sum(results_i$size >= intervals[14] & results_i$size < intervals[15])/nrow(results_i)
        }

      if (is.na(length(intervals))) {
        int_last <- NA } else {
          int_last <-sum(results_i$size >= intervals[length(intervals)])/nrow(results_i)
        }



      results_i_final <- as.data.frame(rbind(int_2,int_3,int_4,int_5,
                                             int_6,int_7,int_8,int_9,int_10,
                                             int_11,int_12,int_13,int_14,int_15,
                                             int_last))
      results_i_final$combo <- j
      results_i_final$durclass_name <- durclass_name
      results_i_final$durval_name <- durval_name

      results_i_final<- results_i_final[complete.cases(results_i_final), ]
      results_i_final$x_axis <- 1:nrow(results_i_final)

      row_numbers <- nrow(results_i_final)
      NonNAindex <- which(is.na(results_inter))
      start_row <- min(NonNAindex)
      end_row <- start_row + row_numbers - 1
      results_inter[start_row:end_row,]<- results_i_final
    }
  }



  results_inter<- results_inter[complete.cases(results_inter), ]



  unique_all <- paste(results_inter$X4,results_inter$X3,sep="_")
  unique_all <- as.data.frame(unique(unique_all))
  colnames(unique_all) <- "durval"


  unique_all <- str_split_fixed(unique_all$durval, "_durclass_", 2)
  unique_all <- as.data.frame(unique_all)
  colnames(unique_all) <- c("durval","durclass")

  results_inter_expanded <- expand.grid(split(unique_all$durval, unique_all$durclass))
  names(results_inter_expanded)[names(results_inter_expanded) == '1'] <- 'durclass_1'
  names(results_inter_expanded)[names(results_inter_expanded) == '2'] <- 'durclass_2'
  names(results_inter_expanded)[names(results_inter_expanded) == '3'] <- 'durclass_3'
  names(results_inter_expanded)[names(results_inter_expanded) == '4'] <- 'durclass_4'
  names(results_inter_expanded)[names(results_inter_expanded) == '5'] <- 'durclass_5'



  my_observed_fire <- read.csv (hist.fire.sizes)



  if (is.na(intervals[1])) {
    int_1 <- NA } else {
      int_1 <- sum(my_observed_fire$Fire.size < intervals[1])/nrow(my_observed_fire)
    }


  if (is.na(intervals[2])) {
    int_2 <- NA } else {
      int_2 <- sum(my_observed_fire$Fire.size >= intervals[1] & my_observed_fire$Fire.size < intervals[2])/nrow(my_observed_fire)
    }


  if (is.na(intervals[3])) {
    int_3 <- NA } else {
      int_3 <- sum(my_observed_fire$Fire.size >= intervals[2] & my_observed_fire$Fire.size < intervals[3])/nrow(my_observed_fire)
    }

  if (is.na(intervals[4])) {
    int_4 <- NA } else {
      int_4 <-sum(my_observed_fire$Fire.size >= intervals[3] & my_observed_fire$Fire.size < intervals[4])/nrow(my_observed_fire)
    }


  if (is.na(intervals[5])) {
    int_5 <- NA } else {
      int_5 <-sum(my_observed_fire$Fire.size >= intervals[4] & my_observed_fire$Fire.size < intervals[5])/nrow(my_observed_fire)
    }


  if (is.na(intervals[6])) {
    int_6 <- NA } else {
      int_6 <-sum(my_observed_fire$Fire.size >= intervals[5] & my_observed_fire$Fire.size < intervals[6])/nrow(my_observed_fire)
    }


  if (is.na(intervals[7])) {
    int_7 <- NA } else {
      int_7 <-sum(my_observed_fire$Fire.size >= intervals[6] & my_observed_fire$Fire.size < intervals[7])/nrow(my_observed_fire)
    }


  if (is.na(intervals[8])) {
    int_8 <- NA } else {
      int_8 <-sum(my_observed_fire$Fire.size >= intervals[7] & my_observed_fire$Fire.size < intervals[8])/nrow(my_observed_fire)
    }



  if (is.na(intervals[9])) {
    int_9 <- NA } else {
      int_9 <-sum(my_observed_fire$Fire.size >= intervals[8] & my_observed_fire$Fire.size < intervals[9])/nrow(my_observed_fire)
    }




  if (is.na(intervals[10])) {
    int_10 <- NA } else {
      int_10 <-sum(my_observed_fire$Fire.size >= intervals[9] & my_observed_fire$Fire.size < intervals[10])/nrow(my_observed_fire)
    }




  if (is.na(intervals[11])) {
    int_11 <- NA } else {
      int_11 <-sum(my_observed_fire$Fire.size >= intervals[10] & my_observed_fire$Fire.size < intervals[11])/nrow(my_observed_fire)
    }




  if (is.na(intervals[12])) {
    int_12 <- NA } else {
      int_12 <-sum(my_observed_fire$Fire.size >= intervals[11] & my_observed_fire$Fire.size < intervals[12])/nrow(my_observed_fire)
    }



  if (is.na(intervals[13])) {
    int_13 <- NA } else {
      int_13 <-sum(my_observed_fire$Fire.size >= intervals[12] & my_observed_fire$Fire.size < intervals[13])/nrow(my_observed_fire)
    }


  if (is.na(intervals[14])) {
    int_14 <- NA } else {
      int_14 <-sum(my_observed_fire$Fire.size >= intervals[13] & my_observed_fire$Fire.size < intervals[14])/nrow(my_observed_fire)
    }


  if (is.na(intervals[15])) {
    int_15 <- NA } else {
      int_15 <-sum(my_observed_fire$Fire.size >= intervals[14] & my_observed_fire$Fire.size < intervals[15])/nrow(my_observed_fire)
    }


  if (is.na(length(intervals))) {
    int_last <- NA } else {
      int_last <-sum(my_observed_fire$Fire.size >= intervals[length(intervals)])/nrow(my_observed_fire)
    }


  if (all.dist==TRUE){
    my_observed_fire_final <- as.data.frame(rbind(int_1,int_2,int_3,int_4,int_5,
                                                  int_6,int_7,int_8,int_9,int_10,
                                                  int_11,int_12,int_13,int_14,int_15,
                                                  int_last))
  }

  if(all.dist==FALSE){
    my_observed_fire_final <- as.data.frame(rbind(int_2,int_3,int_4,int_5,
                                                  int_6,int_7,int_8,int_9,int_10,
                                                  int_11,int_12,int_13,int_14,int_15,
                                                  int_last))
  }


  my_observed_fire_final$combo <- 0

  my_observed_fire_final<- my_observed_fire_final[complete.cases(my_observed_fire_final), ]
  my_observed_fire_final$x_axis <- 1:nrow(my_observed_fire_final)


  colnames(results_inter) <- c("V1","combo","durclass","durval","x_axis")


  freqs.durclass_use <- read.csv(freqs.durclass)

  freq_dur1 <- sum(freqs.durclass_use$duration_1)
  freq_dur2 <- sum(freqs.durclass_use$duration_2)
  freq_dur3 <- sum(freqs.durclass_use$duration_3)
  freq_dur4 <- sum(freqs.durclass_use$duration_4)
  freq_dur5 <- sum(freqs.durclass_use$duration_5)



  df <- results_inter %>% mutate(V1 = ifelse(durclass == "durclass_1", V1*freq_dur1, V1),
                                 V1 = ifelse(durclass == "durclass_2", V1*freq_dur2, V1),
                                 V1 = ifelse(durclass == "durclass_3", V1*freq_dur3, V1),
                                 V1 = ifelse(durclass == "durclass_4", V1*freq_dur4, V1),
                                 V1 = ifelse(durclass == "durclass_5", V1*freq_dur5, V1))


  results <- matrix(ncol=3, nrow= 1000000)
  nrow(results)


  results_rmse <- matrix(ncol=12, nrow= nrow(results_inter_expanded))
  nrow(results_rmse)

  for (t in 1:nrow(results_inter_expanded)){
    results_inter_expanded_loop <- as.data.frame(results_inter_expanded[t,])
    results_inter_expanded_loop <- cbind(as.list(colnames(results_inter_expanded_loop)),results_inter_expanded_loop)
    results_inter_expanded_loop$`"durclass_1"`

    dur1_combo1_use <- subset(df,durclass==results_inter_expanded_loop$`"durclass_1"` &
                                durval == results_inter_expanded_loop$durclass_1)

    dur2_combo1_use <- subset(df,durclass==results_inter_expanded_loop$`"durclass_2"` &
                                durval == results_inter_expanded_loop$durclass_2)

    dur3_combo1_use <- subset(df,durclass==results_inter_expanded_loop$`"durclass_3"` &
                                durval == results_inter_expanded_loop$durclass_3)

    dur4_combo1_use <- subset(df,durclass==results_inter_expanded_loop$`"durclass_4"` &
                                durval == results_inter_expanded_loop$durclass_4)

    dur5_combo1_use <- subset(df,durclass==results_inter_expanded_loop$`"durclass_5"` &
                                durval == results_inter_expanded_loop$durclass_5)


    x <- c("V1", "combo", "durclass","durval","x_axis")


    if(nrow(dur2_combo1_use)<1){

      dur2_combo1_use <-as.data.frame(matrix(ncol=5, nrow= nrow(dur1_combo1_use)))
      colnames(dur2_combo1_use) <- x

      dur2_combo1_use$V1 <- 0
      dur2_combo1_use$combo <- 0
      dur2_combo1_use$durclass <- 0
      dur2_combo1_use$durval <- 0
      dur2_combo1_use$x_axis <- 0
    }


    if(nrow(dur3_combo1_use)<1){

      dur3_combo1_use <-as.data.frame(matrix(ncol=5, nrow= nrow(dur1_combo1_use)))
      colnames(dur3_combo1_use) <- x

      dur3_combo1_use$V1 <- 0
      dur3_combo1_use$combo <- 0
      dur3_combo1_use$durclass <- 0
      dur3_combo1_use$durval <- 0
      dur3_combo1_use$x_axis <- 0
    }



    if(nrow(dur4_combo1_use)<1){

      dur4_combo1_use <-as.data.frame(matrix(ncol=5, nrow= nrow(dur1_combo1_use)))
      colnames(dur4_combo1_use) <- x

      dur4_combo1_use$V1 <- 0
      dur4_combo1_use$combo <- 0
      dur4_combo1_use$durclass <- 0
      dur4_combo1_use$durval <- 0
      dur4_combo1_use$x_axis <- 0
    }




    if(nrow(dur5_combo1_use)<1){

      dur5_combo1_use <-as.data.frame(matrix(ncol=5, nrow= nrow(dur1_combo1_use)))
      colnames(dur5_combo1_use) <- x

      dur5_combo1_use$V1 <- 0
      dur5_combo1_use$combo <- 0
      dur5_combo1_use$durclass <- 0
      dur5_combo1_use$durval <- 0
      dur5_combo1_use$x_axis <- 0
    }




    all_dur_loop <- as.data.frame(dur1_combo1_use$V1 +dur2_combo1_use$V1 +
                                    dur3_combo1_use$V1 + dur4_combo1_use$V1+ dur5_combo1_use$V1)

    all_dur_loop$durval_1 <- dur1_combo1_use$durval
    all_dur_loop$durval_2 <- dur2_combo1_use$durval
    all_dur_loop$durval_3 <- dur3_combo1_use$durval
    all_dur_loop$durval_4 <- dur4_combo1_use$durval
    all_dur_loop$durval_5 <- dur5_combo1_use$durval
    all_dur_loop$x_axis <- dur1_combo1_use$x_axis
    all_dur_loop$combo <- t

    old_name <- colnames(all_dur_loop[1])

    names(all_dur_loop)[names(all_dur_loop) == old_name] <- "V1"

    all_dur_loop_bind <-all_dur_loop

    all_dur_loop_bind <- all_dur_loop_bind[, -which(names(all_dur_loop_bind) %in% c("durval_1","durval_2","durval_3","durval_4","durval_5"))]



    #RMSE

    test_obs <- my_observed_fire_final
    test_i <- all_dur_loop_bind


    test_obs$V1_sim <- test_i$V1

    rmse_i <- sqrt(mean((test_obs$V1 - test_obs$V1_sim)^2))

    cor_i <- cor(test_obs$V1,test_obs$V1_sim)

    per_nrmse_i <- rmse_per(test_obs, y="V1",yhat="V1_sim")


    mae_i <- mae(test_obs$V1,test_obs$V1_sim)
    rae_i <- rae(test_obs$V1,test_obs$V1_sim)

    nse_i <- vnse(test_obs$V1_sim,test_obs$V1)



    NAindex <- which(is.na(results))
    firstNA <- min(NAindex)


    start_results <- firstNA
    end_results <- firstNA+nrow(all_dur_loop_bind)-1





    NAindex_rmse <- which(is.na(results_rmse))
    firstNA_rmse <- min(NAindex_rmse)


    start_results_rmse <- firstNA_rmse
    end_results_rmse <- firstNA_rmse


    results[start_results:end_results,1]<- all_dur_loop_bind$V1
    results[start_results:end_results,2]<- all_dur_loop_bind$combo
    results[start_results:end_results,3]<- all_dur_loop_bind$x_axis


    results_rmse[start_results_rmse:end_results_rmse,1]<- as.numeric(all_dur_loop_bind$combo[1])
    results_rmse[start_results_rmse:end_results_rmse,2]<- as.numeric(gsub("durval_","",all_dur_loop$durval_1[1]))
    results_rmse[start_results_rmse:end_results_rmse,3]<- as.numeric(gsub("durval_","",all_dur_loop$durval_2[1]))
    results_rmse[start_results_rmse:end_results_rmse,4]<- as.numeric(gsub("durval_","",all_dur_loop$durval_3[1]))
    results_rmse[start_results_rmse:end_results_rmse,5]<- as.numeric(gsub("durval_","",all_dur_loop$durval_4[1]))
    results_rmse[start_results_rmse:end_results_rmse,6]<- as.numeric(gsub("durval_","",all_dur_loop$durval_5[1]))
    results_rmse[start_results_rmse:end_results_rmse,7]<- as.numeric(rmse_i)
    results_rmse[start_results_rmse:end_results_rmse,8]<- as.numeric(cor_i)
    results_rmse[start_results_rmse:end_results_rmse,9]<- as.numeric(per_nrmse_i)
    results_rmse[start_results_rmse:end_results_rmse,10]<- as.numeric(mae_i)
    results_rmse[start_results_rmse:end_results_rmse,11]<- as.numeric(rae_i)
    results_rmse[start_results_rmse:end_results_rmse,12]<- as.numeric(nse_i)

  }

  results <- as.data.frame(results)
  results <- na.omit(results)
  colnames(results) <- c("V1", "combo", "x_axis")


  results_rmse <- as.data.frame(results_rmse)
  results_rmse <- na.omit(results_rmse)
  colnames(results_rmse) <- c("combo", "durval1","durval2","durval3","durval4","durval5","RMSE","Correlation","percentage NRMSE","MAE","RAE","NSE")

  results_rmse<-results_rmse[, colSums(abs(results_rmse)) > 0]

  results_rmse <- results_rmse[order(results_rmse$RMSE),]

  write.csv(results_rmse,file=paste(Folder.Outputs,"/rmse_combos.csv",sep=""), row.names = FALSE)




  all_for_plot <- rbind(my_observed_fire_final,results)

  all_for_plot_diff <- my_observed_fire_final$V1-results$V1



  nbreaks <- nrow(dur1_combo1_use)

  total_combos <- max(all_for_plot$combo)





  if (plot.all==TRUE & all.dist==TRUE){

    if (total_combos > 11) {
      total_figures <- ceiling(total_combos/11)

      for(b in 1:total_figures) {
        historical_use_always <- subset(all_for_plot,combo==0)

        final_val <- 11*b
        start_value <- final_val-10

        plot_partially <- all_for_plot[all_for_plot$combo %in% start_value:final_val, ]

        plot_partially_use <- rbind(historical_use_always,plot_partially)





        automatic_lables_final <- numeric()

        first_interval <- paste(0,intervals[1],sep="-")

        for (i in 1:(length(intervals)-1)){
          automatic_lables <- paste(intervals[i],intervals[i+1],sep="-")
          automatic_lables_final <- c(automatic_lables_final,automatic_lables)
        }

        automatic_lables_final <- c(first_interval,automatic_lables_final)

        automatic_lables_final <- gsub("-","-\n",automatic_lables_final)




        temp_plot <- ggplot(plot_partially_use, aes(x=x_axis, y=V1, group=factor(combo),fill = factor(combo))) +


          geom_bar(data = filter(plot_partially_use, combo  == 0), aes(fill="Historical"),col="black",stat = "identity") +


          scale_fill_manual("",values=c("Historical" = "grey80"))+

          geom_line(data = filter(plot_partially_use, combo  != 0), aes(col = factor(combo) , group = factor(combo)),size=1)+
          scale_color_brewer(palette="Spectral",
                             name="Combination")+

          scale_x_continuous(breaks= c(1:nrow(historical_use_always)),
                             labels= c(automatic_lables_final,paste(">",intervals[length(intervals)],sep="")))+


          theme_tq()+
          theme(panel.grid.minor = element_blank(), axis.title=element_text(size=12),
                panel.grid.major.x = element_blank(), axis.text = element_text(size = 8),
                plot.title = element_text(size = 16))+
          ylab("Relative frequency") + xlab("Fire size class (ha)")

        ggsave(temp_plot, file=paste(Folder.Outputs,"/fire size distribution part",b,".png",sep=""), width = 17, height = 10, units = "cm")



      }

      all_for_plot_for_saving <- all_for_plot

      all_for_plot_for_saving$label <- c(automatic_lables_final,paste(">",intervals[length(intervals)],sep=""))
      all_for_plot_for_saving$label <- gsub("\n"," ",all_for_plot_for_saving$label)
      all_for_plot_for_saving$label <- paste0(" ", all_for_plot_for_saving$label)

      colnames(all_for_plot_for_saving)<- c("relative frequency", "combo", "class","area")

      all_for_plot_for_saving <- as.data.frame(cbind(all_for_plot_for_saving$class,all_for_plot_for_saving$area,all_for_plot_for_saving$combo,all_for_plot_for_saving$`relative frequency`))
      colnames(all_for_plot_for_saving)<- c("class","area","combo","relative frequency")

      write.csv(all_for_plot_for_saving,paste(Folder.Outputs,"/simulated_frequencies_fire_size.csv",sep=""),row.names = FALSE)


    } else {
      total_figures <- 1

      for(b in 1:total_figures) {
        historical_use_always <- subset(all_for_plot,combo==0)

        final_val <- 11*b
        start_value <- final_val-10

        plot_partially <- all_for_plot[all_for_plot$combo %in% start_value:final_val, ]

        plot_partially_use <- rbind(historical_use_always,plot_partially)


        automatic_lables_final <- numeric()

        first_interval <- paste(0,intervals[1],sep="-")

        for (i in 1:(length(intervals)-1)){
          automatic_lables <- paste(intervals[i],intervals[i+1],sep="-")
          automatic_lables_final <- c(automatic_lables_final,automatic_lables)
        }

        automatic_lables_final <- c(first_interval,automatic_lables_final)

        automatic_lables_final <- gsub("-","-\n",automatic_lables_final)


        temp_plot <- ggplot(plot_partially_use, aes(x=x_axis, y=V1, group=factor(combo),fill = factor(combo))) +


          geom_bar(data = filter(plot_partially_use, combo  == 0), aes(fill="Historical"),col="black",stat = "identity") +

          scale_fill_manual("",values=c("Historical" = "grey80"))+

          geom_line(data = filter(plot_partially_use, combo  != 0), aes(col = factor(combo) , group = factor(combo)),size=1)+
          scale_color_brewer(palette="Spectral",
                             name="Combination")+

          scale_x_continuous(breaks= c(1:nrow(historical_use_always)),
                             labels= c(automatic_lables_final,paste(">",intervals[length(intervals)],sep="")))+


          theme_tq()+
          theme(panel.grid.minor = element_blank(), axis.title=element_text(size=12),
                panel.grid.major.x = element_blank(), axis.text = element_text(size = 8),
                plot.title = element_text(size = 16))+
          ylab("Relative frequency") + xlab("Fire size class (ha)")

        ggsave(temp_plot, file=paste(Folder.Outputs,"/fire size distribution part",b,".png",sep=""), width = 17, height = 10, units = "cm")



      }
      all_for_plot_for_saving <- all_for_plot

      all_for_plot_for_saving$label <- c(automatic_lables_final,paste(">",intervals[length(intervals)],sep=""))
      all_for_plot_for_saving$label <- gsub("\n"," ",all_for_plot_for_saving$label)
      all_for_plot_for_saving$label <- paste0(" ", all_for_plot_for_saving$label)

      colnames(all_for_plot_for_saving)<- c("relative frequency", "combo", "class","area")

      all_for_plot_for_saving <- as.data.frame(cbind(all_for_plot_for_saving$class,all_for_plot_for_saving$area,all_for_plot_for_saving$combo,all_for_plot_for_saving$`relative frequency`))
      colnames(all_for_plot_for_saving)<- c("class","area","combo","relative frequency")

      write.csv(all_for_plot_for_saving,paste(Folder.Outputs,"/simulated_frequencies_fire_size.csv",sep=""),row.names = FALSE)


    }}






  if (plot.all==TRUE & all.dist==FALSE){

    if (total_combos > 11) {
      total_figures <- ceiling(total_combos/11)

      for(b in 1:total_figures) {
        historical_use_always <- subset(all_for_plot,combo==0)

        final_val <- 11*b
        start_value <- final_val-10

        plot_partially <- all_for_plot[all_for_plot$combo %in% start_value:final_val, ]

        plot_partially_use <- rbind(historical_use_always,plot_partially)





        automatic_lables_final <- numeric()

        first_interval <- paste(0,intervals[1],sep="-")

        for (i in 1:(length(intervals)-1)){
          automatic_lables <- paste(intervals[i],intervals[i+1],sep="-")
          automatic_lables_final <- c(automatic_lables_final,automatic_lables)
        }

        automatic_lables_final <- gsub("-","-\n",automatic_lables_final)




        temp_plot <- ggplot(plot_partially_use, aes(x=x_axis, y=V1, group=factor(combo),fill = factor(combo))) +


          geom_bar(data = filter(plot_partially_use, combo  == 0), aes(fill="Historical"),col="black",stat = "identity") +

          scale_fill_manual("",values=c("Historical" = "grey80"))+

          geom_line(data = filter(plot_partially_use, combo  != 0), aes(col = factor(combo) , group = factor(combo)),size=1)+
          scale_color_brewer(palette="Spectral",
                             name="Combination")+

          scale_x_continuous(breaks= c(1:nrow(historical_use_always)),
                             labels= c(automatic_lables_final,paste(">",intervals[length(intervals)],sep="")))+


          theme_tq()+
          theme(panel.grid.minor = element_blank(), axis.title=element_text(size=12),
                panel.grid.major.x = element_blank(), axis.text = element_text(size = 8),
                plot.title = element_text(size = 16))+
          ylab("Relative frequency") + xlab("Fire size class (ha)")

        ggsave(temp_plot, file=paste(Folder.Outputs,"/fire size distribution part",b,".png",sep=""), width = 17, height = 10, units = "cm")



      }
      all_for_plot_for_saving <- all_for_plot

      all_for_plot_for_saving$label <- c(automatic_lables_final,paste(">",intervals[length(intervals)],sep=""))
      all_for_plot_for_saving$label <- gsub("\n"," ",all_for_plot_for_saving$label)
      all_for_plot_for_saving$label <- paste0(" ", all_for_plot_for_saving$label)

      colnames(all_for_plot_for_saving)<- c("relative frequency", "combo", "class","area")

      all_for_plot_for_saving <- as.data.frame(cbind(all_for_plot_for_saving$class,all_for_plot_for_saving$area,all_for_plot_for_saving$combo,all_for_plot_for_saving$`relative frequency`))
      colnames(all_for_plot_for_saving)<- c("class","area","combo","relative frequency")

      write.csv(all_for_plot_for_saving,paste(Folder.Outputs,"/simulated_frequencies_fire_size.csv",sep=""),row.names = FALSE)

    } else {
      total_figures <- 1

      for(b in 1:total_figures) {
        historical_use_always <- subset(all_for_plot,combo==0)

        final_val <- 11*b
        start_value <- final_val-10

        plot_partially <- all_for_plot[all_for_plot$combo %in% start_value:final_val, ]

        plot_partially_use <- rbind(historical_use_always,plot_partially)


        automatic_lables_final <- numeric()

        first_interval <- paste(0,intervals[1],sep="-")

        for (i in 1:(length(intervals)-1)){
          automatic_lables <- paste(intervals[i],intervals[i+1],sep="-")
          automatic_lables_final <- c(automatic_lables_final,automatic_lables)
        }

        automatic_lables_final <- gsub("-","-\n",automatic_lables_final)


        temp_plot <- ggplot(plot_partially_use, aes(x=x_axis, y=V1, group=factor(combo),fill = factor(combo))) +


          geom_bar(data = filter(plot_partially_use, combo  == 0), aes(fill="Historical"),col="black",stat = "identity") +

          scale_fill_manual("",values=c("Historical" = "grey80"))+

          geom_line(data = filter(plot_partially_use, combo  != 0), aes(col = factor(combo) , group = factor(combo)),size=1)+
          scale_color_brewer(palette="Spectral",
                             name="Combination")+

          scale_x_continuous(breaks= c(1:nrow(historical_use_always)),
                             labels= c(automatic_lables_final,paste(">",intervals[length(intervals)],sep="")))+


          theme_tq()+
          theme(panel.grid.minor = element_blank(), axis.title=element_text(size=12),
                panel.grid.major.x = element_blank(), axis.text = element_text(size = 8),
                plot.title = element_text(size = 16))+
          ylab("Relative frequency") + xlab("Fire size class (ha)")

        ggsave(temp_plot, file=paste(Folder.Outputs,"/fire size distribution part",b,".png",sep=""), width = 17, height = 10, units = "cm")



      }

      all_for_plot_for_saving <- all_for_plot

      all_for_plot_for_saving$label <- c(automatic_lables_final,paste(">",intervals[length(intervals)],sep=""))
      all_for_plot_for_saving$label <- gsub("\n"," ",all_for_plot_for_saving$label)
      all_for_plot_for_saving$label <- paste0(" ", all_for_plot_for_saving$label)

      colnames(all_for_plot_for_saving)<- c("relative frequency", "combo", "class","area")

      all_for_plot_for_saving <- as.data.frame(cbind(all_for_plot_for_saving$class,all_for_plot_for_saving$area,all_for_plot_for_saving$combo,all_for_plot_for_saving$`relative frequency`))
      colnames(all_for_plot_for_saving)<- c("class","area","combo","relative frequency")

      write.csv(all_for_plot_for_saving,paste(Folder.Outputs,"/simulated_frequencies_fire_size.csv",sep=""),row.names = FALSE)

    }}




  if (plot.all==FALSE & all.dist==TRUE){

    total_figures <- 1

    for(b in 1:total_figures) {
      historical_use_always <- subset(all_for_plot,combo==0)

      #final_val <- 11*b
      #start_value <- final_val-10

      plot_partially <- all_for_plot[all_for_plot$combo %in% results_rmse[1,1], ]

      plot_partially_use <- rbind(historical_use_always,plot_partially)





      automatic_lables_final <- numeric()

      first_interval <- paste(0,intervals[1],sep="-")

      for (i in 1:(length(intervals)-1)){
        automatic_lables <- paste(intervals[i],intervals[i+1],sep="-")
        automatic_lables_final <- c(automatic_lables_final,automatic_lables)
      }

      automatic_lables_final <- c(first_interval,automatic_lables_final)

      automatic_lables_final <- gsub("-","-\n",automatic_lables_final)




      temp_plot <- ggplot(plot_partially_use, aes(x=x_axis, y=V1, group=factor(combo),fill = factor(combo))) +


        geom_bar(data = filter(plot_partially_use, combo  == 0), aes(fill="Historical"),col="black",stat = "identity") +


        scale_fill_manual("",values=c("Historical" = "grey80"))+

        geom_line(data = filter(plot_partially_use, combo  != 0), aes(col = factor(combo) , group = factor(combo)),size=1)+
        scale_color_brewer(palette="Spectral",
                           name="Combination")+

        scale_x_continuous(breaks= c(1:nrow(historical_use_always)),
                           labels= c(automatic_lables_final,paste(">",intervals[length(intervals)],sep="")))+


        theme_tq()+
        theme(panel.grid.minor = element_blank(), axis.title=element_text(size=12),
              panel.grid.major.x = element_blank(), axis.text = element_text(size = 8),
              plot.title = element_text(size = 16))+
        ylab("Relative frequency") + xlab("Fire size class (ha)")

      ggsave(temp_plot, file=paste(Folder.Outputs,"/fire size distribution part",b,".png",sep=""), width = 17, height = 10, units = "cm")



    }

    all_for_plot_for_saving <- all_for_plot

    all_for_plot_for_saving$label <- c(automatic_lables_final,paste(">",intervals[length(intervals)],sep=""))
    all_for_plot_for_saving$label <- gsub("\n"," ",all_for_plot_for_saving$label)
    all_for_plot_for_saving$label <- paste0(" ", all_for_plot_for_saving$label)

    colnames(all_for_plot_for_saving)<- c("relative frequency", "combo", "class","area")

    all_for_plot_for_saving <- as.data.frame(cbind(all_for_plot_for_saving$class,all_for_plot_for_saving$area,all_for_plot_for_saving$combo,all_for_plot_for_saving$`relative frequency`))
    colnames(all_for_plot_for_saving)<- c("class","area","combo","relative frequency")

    write.csv(all_for_plot_for_saving,paste(Folder.Outputs,"/simulated_frequencies_fire_size.csv",sep=""),row.names = FALSE)


  }






  if (plot.all==FALSE & all.dist==FALSE){

    for(b in 1:total_figures) {
      historical_use_always <- subset(all_for_plot,combo==0)

      #final_val <- 11*b
      #start_value <- final_val-10

      plot_partially <- all_for_plot[all_for_plot$combo %in% results_rmse[1,1], ]

      plot_partially_use <- rbind(historical_use_always,plot_partially)




      automatic_lables_final <- numeric()

      first_interval <- paste(0,intervals[1],sep="-")

      for (i in 1:(length(intervals)-1)){
        automatic_lables <- paste(intervals[i],intervals[i+1],sep="-")
        automatic_lables_final <- c(automatic_lables_final,automatic_lables)
      }

      automatic_lables_final <- gsub("-","-\n",automatic_lables_final)




      temp_plot <- ggplot(plot_partially_use, aes(x=x_axis, y=V1, group=factor(combo),fill = factor(combo))) +


        geom_bar(data = filter(plot_partially_use, combo  == 0), aes(fill="Historical"),col="black",stat = "identity") +

        scale_fill_manual("",values=c("Historical" = "grey80"))+

        geom_line(data = filter(plot_partially_use, combo  != 0), aes(col = factor(combo) , group = factor(combo)),size=1)+
        scale_color_brewer(palette="Spectral",
                           name="Combination")+

        scale_x_continuous(breaks= c(1:nrow(historical_use_always)),
                           labels= c(automatic_lables_final,paste(">",intervals[length(intervals)],sep="")))+


        theme_tq()+
        theme(panel.grid.minor = element_blank(), axis.title=element_text(size=12),
              panel.grid.major.x = element_blank(), axis.text = element_text(size = 8),
              plot.title = element_text(size = 16))+
        ylab("Relative frequency") + xlab("Fire size class (ha)")

      ggsave(temp_plot, file=paste(Folder.Outputs,"/fire size distribution part",b,".png",sep=""), width = 17, height = 10, units = "cm")



    }
    all_for_plot_for_saving <- all_for_plot

    all_for_plot_for_saving$label <- c(automatic_lables_final,paste(">",intervals[length(intervals)],sep=""))
    all_for_plot_for_saving$label <- gsub("\n"," ",all_for_plot_for_saving$label)
    all_for_plot_for_saving$label <- paste0(" ", all_for_plot_for_saving$label)

    colnames(all_for_plot_for_saving)<- c("relative frequency", "combo", "class","area")

    all_for_plot_for_saving <- as.data.frame(cbind(all_for_plot_for_saving$class,all_for_plot_for_saving$area,all_for_plot_for_saving$combo,all_for_plot_for_saving$`relative frequency`))
    colnames(all_for_plot_for_saving)<- c("class","area","combo","relative frequency")

    write.csv(all_for_plot_for_saving,paste(Folder.Outputs,"/simulated_frequencies_fire_size.csv",sep=""),row.names = FALSE)

  }



}
