#' Compares the estimated burn probability with the historical number of times burned.
#' @description Combines and calculates the pearson correlation between the estimated burn probability that result from multiple duration combinations and the historical number of times burned.
#' @param Folder.Outputs Path to the folder containing the outputs of the FConstMTT runs.
#' @param freq.scenario Text file (csv) with the relative frequency of each meteorological and fuel map scenario used. If the function Gen_ign was used in the process, then the file to be used here should be “clusters_freqs_final.csv”, which is located in the ignition folder.
#' @param choose.combos A text file (csv) with the numeric identification of the different combinations. If check_fire_size was run, then this file was stored as “rmse_combos.csv”.
#' @param combos.file Numerical vector. Specify the combinations to be tested. The numeric identification of the combinations is located in the combos.file. In alternative, use “all” to use all duration combinations.
#' @param obs.nxburned Raster file with the historical number of times burned. This raster must have the same spatial resolution and alignment that the simulated burn probability rasters.
#' @param export.plots Binary. If 1, then a boxplot showing the correlation between the estimated burn probability and the number of times burned is saved. If 0, no plot is saved.
#'
#' @return Returns a raster file with the simulated burn probability and a text file (csv) showing the pearson correlation between the simulated burn probability for each combination and the historical number of times burned. Optionally, it can also return a boxplot showing the same correlation.
#' @export
#'
#' @examples
#' \dontrun{check_BP_nxburned(Folder.Outputs="C:/user/fconstmtt/Outputs",
#' freq.scenario="C:/user/results/ignitions/clusters_freqs_final.csv",
#' choose.combos=c(1,2),combos.file="C:/user/fconstmtt/Outputs/rmse_combos.csv",
#' obs.nxburned="C:/user/number_of_times_burned.tif",
#' export.plots=1)}
#'
check_BP_nxburned <- function (Folder.Outputs,
                               freq.scenario,choose.combos,combos.file,obs.nxburned,
                               export.plots){

  setwd(Folder.Outputs)
  my_files <- list.files(pattern = "BP.asc")
  #library (raster)
  #library(ggplot2)

  obs.nxburned_use <- raster(obs.nxburned)

  my_freqs <- read.csv(freq.scenario)

  my_combos_file <- read.csv(combos.file)

  my_combos_file_order <- my_combos_file[order(my_combos_file$RMSE),]

  # if (exists("choose.combos")==TRUE){
  #   nCombos <- choose.combos
  # } else {
  #   #nCombos <- 1:nCombos
  # #} else {
  #   nCombos <- my_combos_file_order[1,1]
  # }


  if (missing(choose.combos)) {
    nCombos <- my_combos_file_order[1,1]} else {
      nCombos <- choose.combos
    }


  if (choose.combos=="all") {
    nCombos <- my_combos_file_order[,1]}



  #
  # my_freqs$direction_val <- 0
  # my_freqs$direction_val[my_freqs$direction=="N"] <- 0
  # my_freqs$direction_val[my_freqs$direction=="NE"] <- 45
  # my_freqs$direction_val[my_freqs$direction=="E"] <- 90
  # my_freqs$direction_val[my_freqs$direction=="SE"] <- 135
  # my_freqs$direction_val[my_freqs$direction=="S"] <- 180
  # my_freqs$direction_val[my_freqs$direction=="SW"] <- 225
  # my_freqs$direction_val[my_freqs$direction=="W"] <- 270
  # my_freqs$direction_val[my_freqs$direction=="NW"] <- 315
  #
  #
  # my_freqs_sort <- my_freqs[
  #   with(my_freqs, order(cluster,direction_val)),
  # ]



  names(my_freqs)[names(my_freqs)==names(my_freqs)[1]] <- 'cluster'

  my_freqs_sort <- my_freqs[
    with(my_freqs, order(cluster,WD_use)),
  ]

  #datalist <- list()
  #results <- data.frame(matrix(NA, nrow = ((nCombos * nIgn.per.combo)+5000), ncol = 2))

  #library(tidyr)

  correlation_df <- matrix(ncol=2,nrow=length(nCombos))

  for (k in 1:length(nCombos)){
    i <- nCombos[k]

    my_combos_file_loop <- subset(my_combos_file,combo==i)

    my_combos_file_loop_dur1 <- my_combos_file_loop$durval1
    my_combos_file_loop_dur2 <- my_combos_file_loop$durval2
    my_combos_file_loop_dur3 <- my_combos_file_loop$durval3
    my_combos_file_loop_dur4 <- my_combos_file_loop$durval4
    my_combos_file_loop_dur5 <- my_combos_file_loop$durval5

    if (is.integer(my_combos_file_loop_dur1))
    {my_combos_file_loop_dur1 <- my_combos_file_loop$durval1}else{
      my_combos_file_loop_dur1 <- 0
    }


    if (is.integer(my_combos_file_loop_dur2))
    {my_combos_file_loop_dur2 <- my_combos_file_loop$durval2}else{
      my_combos_file_loop_dur2 <- 0
    }


    if (is.integer(my_combos_file_loop_dur3))
    {my_combos_file_loop_dur3 <- my_combos_file_loop$durval3}else{
      my_combos_file_loop_dur3 <- 0
    }


    if (is.integer(my_combos_file_loop_dur4))
    {my_combos_file_loop_dur4 <- my_combos_file_loop$durval4}else{
      my_combos_file_loop_dur4 <- 0
    }


    if (is.integer(my_combos_file_loop_dur5))
    {my_combos_file_loop_dur5 <- my_combos_file_loop$durval5}else{
      my_combos_file_loop_dur5 <- 0
    }

    setwd(Folder.Outputs)
    my_data_dur_1 <- intersect(list.files(pattern = "\\.asc$"), list.files(pattern = paste("durval_",my_combos_file_loop_dur1,sep="")))
    my_data_dur_2 <- intersect(list.files(pattern = "\\.asc$"), list.files(pattern = paste("durval_",my_combos_file_loop_dur2,sep="")))
    my_data_dur_3 <- intersect(list.files(pattern = "\\.asc$"), list.files(pattern = paste("durval_",my_combos_file_loop_dur3,sep="")))
    my_data_dur_4 <- intersect(list.files(pattern = "\\.asc$"), list.files(pattern = paste("durval_",my_combos_file_loop_dur4,sep="")))
    my_data_dur_5 <- intersect(list.files(pattern = "\\.asc$"), list.files(pattern = paste("durval_",my_combos_file_loop_dur5,sep="")))

    #armazenar cada uma das durações em tabelas diferentes


    my_data_i_dur1 <- grep("durclass_1", my_data_dur_1, value = TRUE)
    my_data_i_dur2 <- grep("durclass_2", my_data_dur_2, value = TRUE)
    my_data_i_dur3 <- grep("durclass_3", my_data_dur_3, value = TRUE)
    my_data_i_dur4 <- grep("durclass_4", my_data_dur_4, value = TRUE)
    my_data_i_dur5 <- grep("durclass_5", my_data_dur_5, value = TRUE)





    #1_1

    my_data_i_dur1_cluster1 <- as.data.frame(grep("cluster_1", my_data_i_dur1, value = TRUE))
    colnames(my_data_i_dur1_cluster1)<- "name"


    ##############
    my_data_i_dur1 <- as.data.frame(my_data_i_dur1)
    colnames(my_data_i_dur1)<- "name"
    cluster_id_temp <- sub("_durclass_.*", "", my_data_i_dur1$name)
    cluster_id <- as.numeric(sub(".*_cluster_", "", cluster_id_temp))

    my_data_i_dur1$cluster_id <- cluster_id


    wd_id_temp <- sub("_land_.*", "", my_data_i_dur1$name)
    wd_id <- as.numeric(sub(".*durclass_1_", "", wd_id_temp))
    my_data_i_dur1$wd_id <- wd_id



    my_data_i_dur1_sort <- my_data_i_dur1[
      with(my_data_i_dur1, order(cluster_id,wd_id)),
    ]







    # my_data_i_dur1_cluster1$wd_id <- combo_id
    #
    #
    #
    # my_data_i_dur1_cluster1_sort <- my_data_i_dur1_cluster1[
    #   with(my_data_i_dur1_cluster1, order(wd_id)),
    # ]
    #
    #
    # my_freqs_sort_cluster1 <- subset(my_freqs_sort,cluster==1)
    #
    # library(raster)

    s1_1 <- stack()

    for (j in 1:nrow(my_data_i_dur1_sort)){
      raster_i <- raster(paste(my_data_i_dur1_sort[j,"name"]))
      multiply_my_i <- my_freqs_sort [j,"duration_1"]

      raster_i_multiplied <- raster_i*multiply_my_i
      #assign(paste("raster_final_",j,sep=""),raster_i_multiplied)

      name_i <- paste("raster_final_",j,sep="")

      #datalist[j]<-name_i
      #results[j,]<-name_i

      s1_1 <- stack(s1_1,raster_i_multiplied)

    }




    #2_2

    #my_data_i_dur2_cluster1 <- as.data.frame(grep("cluster_1", my_data_i_dur2, value = TRUE))
    #colnames(my_data_i_dur2_cluster1)<- "name"


    ##############
    my_data_i_dur2 <- as.data.frame(my_data_i_dur2)
    colnames(my_data_i_dur2)<- "name"
    cluster_id_temp <- sub("_durclass_.*", "", my_data_i_dur2$name)
    cluster_id <- as.numeric(sub(".*_cluster_", "", cluster_id_temp))

    my_data_i_dur2$cluster_id <- cluster_id


    wd_id_temp <- sub("_land_.*", "", my_data_i_dur2$name)
    wd_id <- as.numeric(sub(".*durclass_2_", "", wd_id_temp))
    my_data_i_dur2$wd_id <- wd_id



    my_data_i_dur2_sort <- my_data_i_dur2[
      with(my_data_i_dur2, order(cluster_id,wd_id)),
    ]







    # my_data_i_dur2_cluster2$wd_id <- combo_id
    #
    #
    #
    # my_data_i_dur2_cluster2_sort <- my_data_i_dur2_cluster2[
    #   with(my_data_i_dur2_cluster2, order(wd_id)),
    # ]
    #
    #
    # my_freqs_sort_cluster2 <- subset(my_freqs_sort,cluster==2)
    #
    # library(raster)

    s2_1 <- stack()

    if(nrow(my_data_i_dur2_sort)>=1){
      for (j in 1:nrow(my_data_i_dur2_sort)){
        raster_i <- raster(paste(my_data_i_dur2_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_2"]

        raster_i_multiplied <- raster_i*multiply_my_i
        #assign(paste("raster_final_",j,sep=""),raster_i_multiplied)

        name_i <- paste("raster_final_",j,sep="")

        #datalist[j]<-name_i
        #results[j,]<-name_i

        s2_1 <- stack(s2_1,raster_i_multiplied)

      }
    }else{
      s2_1 <- s1_1
      s2_1 <- reclassify(s2_1,c(-Inf,Inf,0))
    }



    #3_3

    #my_data_i_dur3_cluster1 <- as.data.frame(grep("cluster_1", my_data_i_dur3, value = TRUE))
    #colnames(my_data_i_dur3_cluster1)<- "name"


    ##############
    my_data_i_dur3 <- as.data.frame(my_data_i_dur3)
    colnames(my_data_i_dur3)<- "name"
    cluster_id_temp <- sub("_durclass_.*", "", my_data_i_dur3$name)
    cluster_id <- as.numeric(sub(".*_cluster_", "", cluster_id_temp))

    my_data_i_dur3$cluster_id <- cluster_id


    wd_id_temp <- sub("_land_.*", "", my_data_i_dur3$name)
    wd_id <- as.numeric(sub(".*durclass_3_", "", wd_id_temp))
    my_data_i_dur3$wd_id <- wd_id



    my_data_i_dur3_sort <- my_data_i_dur3[
      with(my_data_i_dur3, order(cluster_id,wd_id)),
    ]







    # my_data_i_dur3_cluster3$wd_id <- combo_id
    #
    #
    #
    # my_data_i_dur3_cluster3_sort <- my_data_i_dur3_cluster3[
    #   with(my_data_i_dur3_cluster3, order(wd_id)),
    # ]
    #
    #
    # my_freqs_sort_cluster3 <- subset(my_freqs_sort,cluster==3)
    #
    # library(raster)

    s3_1 <- stack()

    if(nrow(my_data_i_dur3_sort)>=1){
      for (j in 1:nrow(my_data_i_dur3_sort)){
        raster_i <- raster(paste(my_data_i_dur3_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_3"]

        raster_i_multiplied <- raster_i*multiply_my_i
        #assign(paste("raster_final_",j,sep=""),raster_i_multiplied)

        name_i <- paste("raster_final_",j,sep="")

        #datalist[j]<-name_i
        #results[j,]<-name_i

        s3_1 <- stack(s3_1,raster_i_multiplied)

      }
    }else{
      s3_1 <- s1_1
      s3_1 <- reclassify(s3_1,c(-Inf,Inf,0))
    }





    #4_4

    #my_data_i_dur4_cluster1 <- as.data.frame(grep("cluster_1", my_data_i_dur4, value = TRUE))
    #colnames(my_data_i_dur4_cluster1)<- "name"


    ##############
    my_data_i_dur4 <- as.data.frame(my_data_i_dur4)
    colnames(my_data_i_dur4)<- "name"
    cluster_id_temp <- sub("_durclass_.*", "", my_data_i_dur4$name)
    cluster_id <- as.numeric(sub(".*_cluster_", "", cluster_id_temp))

    my_data_i_dur4$cluster_id <- cluster_id


    wd_id_temp <- sub("_land_.*", "", my_data_i_dur4$name)
    wd_id <- as.numeric(sub(".*durclass_4_", "", wd_id_temp))
    my_data_i_dur4$wd_id <- wd_id



    my_data_i_dur4_sort <- my_data_i_dur4[
      with(my_data_i_dur4, order(cluster_id,wd_id)),
    ]







    # my_data_i_dur4_cluster4$wd_id <- combo_id
    #
    #
    #
    # my_data_i_dur4_cluster4_sort <- my_data_i_dur4_cluster4[
    #   with(my_data_i_dur4_cluster4, order(wd_id)),
    # ]
    #
    #
    # my_freqs_sort_cluster4 <- subset(my_freqs_sort,cluster==4)
    #
    # library(raster)

    s4_1 <- stack()

    if(nrow(my_data_i_dur4_sort)>=1){
      for (j in 1:nrow(my_data_i_dur4_sort)){
        raster_i <- raster(paste(my_data_i_dur4_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_4"]

        raster_i_multiplied <- raster_i*multiply_my_i
        #assign(paste("raster_final_",j,sep=""),raster_i_multiplied)

        name_i <- paste("raster_final_",j,sep="")

        #datalist[j]<-name_i
        #results[j,]<-name_i

        s4_1 <- stack(s4_1,raster_i_multiplied)

      }
    }else{
      s4_1 <- s1_1
      s4_1 <- reclassify(s4_1,c(-Inf,Inf,0))
    }





    #5_5

    #my_data_i_dur5_cluster1 <- as.data.frame(grep("cluster_1", my_data_i_dur5, value = TRUE))
    #colnames(my_data_i_dur5_cluster1)<- "name"


    ##############
    my_data_i_dur5 <- as.data.frame(my_data_i_dur5)
    colnames(my_data_i_dur5)<- "name"
    cluster_id_temp <- sub("_durclass_.*", "", my_data_i_dur5$name)
    cluster_id <- as.numeric(sub(".*_cluster_", "", cluster_id_temp))

    my_data_i_dur5$cluster_id <- cluster_id


    wd_id_temp <- sub("_land_.*", "", my_data_i_dur5$name)
    wd_id <- as.numeric(sub(".*durclass_5_", "", wd_id_temp))
    my_data_i_dur5$wd_id <- wd_id



    my_data_i_dur5_sort <- my_data_i_dur5[
      with(my_data_i_dur5, order(cluster_id,wd_id)),
    ]







    # my_data_i_dur5_cluster5$wd_id <- combo_id
    #
    #
    #
    # my_data_i_dur5_cluster5_sort <- my_data_i_dur5_cluster5[
    #   with(my_data_i_dur5_cluster5, order(wd_id)),
    # ]
    #
    #
    # my_freqs_sort_cluster5 <- subset(my_freqs_sort,cluster==5)
    #
    # library(raster)

    s5_1 <- stack()


    if(nrow(my_data_i_dur5_sort)>=1){
      for (j in 1:nrow(my_data_i_dur5_sort)){
        raster_i <- raster(paste(my_data_i_dur5_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_5"]

        raster_i_multiplied <- raster_i*multiply_my_i
        #assign(paste("raster_final_",j,sep=""),raster_i_multiplied)

        name_i <- paste("raster_final_",j,sep="")

        #datalist[j]<-name_i
        #results[j,]<-name_i

        s5_1 <- stack(s5_1,raster_i_multiplied)

      }
    }else{
      s5_1 <- s1_1
      s5_1 <- reclassify(s5_1,c(-Inf,Inf,0))
    }






    rs1 <- raster::calc(s1_1, sum, na.rm=TRUE)
    rs2 <- raster::calc(s2_1, sum, na.rm=TRUE)
    rs3 <- raster::calc(s3_1, sum, na.rm=TRUE)
    rs4 <- raster::calc(s4_1, sum, na.rm=TRUE)
    rs5 <- raster::calc(s5_1, sum, na.rm=TRUE)


    all_rasters_sum <- sum(rs1,rs2,rs3,rs4,rs5)


    dir.create (paste(Folder.Outputs,"/correlation_BP_NxBurned",sep=""))

    setwd(paste(Folder.Outputs,"/correlation_BP_NxBurned",sep=""))

    writeRaster(all_rasters_sum,paste("BP_combo_",i,".asc",sep=""))


    #crop the Nx burned raster file


    r3 <- crop(obs.nxburned_use,extent(all_rasters_sum))

    r3[is.na(r3[])] <- 0

    #plot(r3)
    #plot(all_rasters_sum)
    #plot(obs.nxburned)


    sim_vals <- getValues(all_rasters_sum)
    hist_vals <- getValues(r3)


    data_for_ggplot <- as.data.frame(cbind(hist_vals,sim_vals))

    store_cor <- cor(data_for_ggplot$hist_vals,data_for_ggplot$sim_vals)


    boxplot_bp <- ggplot(data_for_ggplot, aes(x=as.factor(hist_vals), y=as.numeric(sim_vals))) +
      geom_boxplot(fill="grey")+
      scale_color_grey() + theme_classic()+
      theme(text = element_text(size = 16))+
      xlab("Historical number of times burned")+
      ylab("Simulated Burn Probability")

    if(export.plots==1){
      ggsave(filename=paste(Folder.Outputs,"/correlation_BP_NxBurned","/BP_nxburned_combo_",i,".jpg",sep=""), plot=boxplot_bp, width = 6, height = 5)
      #png(paste(Folder.Outputs,"/BP_nxburned_combo_",i,".png",sep=""), width = 500, height = 400,)
      #boxplot(all_rasters_sum,r3,main=paste("Combo",i), ylab='Simulated Burn Probability', xlab="Historical number of times burned")
      #dev.off()
    }


    correlation_df[k,1] <- i
    correlation_df[k,2]<-store_cor


  }


  colnames(correlation_df) <- c("combo","correlation burn probability - number of times burned")
  write.csv(correlation_df,"correlation_BP_NxBurned.csv",row.names = FALSE)




}
