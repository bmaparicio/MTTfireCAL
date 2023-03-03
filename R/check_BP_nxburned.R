#' Compares the estimated burn probability with the historical number of times burned.
#' @description Combines and calculates the pearson correlation between the estimated burn probability that result from multiple duration combinations and the historical number of times burned.
#' @param Folder.Outputs Path to the folder containing the outputs of the FConstMTT runs.
#' @param freq.scenario Text file (csv) with the relative frequency of each meteorological and fuel map scenario used. If the function Gen_ign was used in the process, then the file to be used here should be “clusters_freqs_final.csv”, which is located in the ignition folder.
#' @param choose.combos Numerical vector. Specify the combinations to be tested. The numeric identification of the combinations is located in the combos.file. In alternative, select “all” to use all duration combinations or "best" to use only the combination with the lowest RMSE.
#' @param combos.file A text file (csv) with the numeric identification of the different combinations. If check_fire_size was run, then this file was stored as “rmse_combos.csv”.
#' @param obs.nxburned Raster file with the historical number of times burned. This raster must have the same spatial resolution and alignment that the simulated burn probability rasters.
#' @param group.nxburned.from A numeric value representing the maximum value of number of times burned that should be considered in the correlation analysis. A value of five means that cells with five or more number of times burned will be grouped together. Optional
#' @param export.plots Binary. If 1, then a boxplot showing the correlation between the estimated burn probability and the number of times burned is saved. If 0, no plot is saved.
#'
#' @return Returns a raster file with the simulated burn probability, a text file (csv) showing the pearson correlation between the simulated burn probability for each combination and the historical number of times burned, and a text file with the relative frequency per class of number of times burned. Optionally, it can also return a boxplot showing the same correlation.
#' @export
#'
#' @examples
#' \dontrun{evaluate_BP_nxburned(Folder.Outputs="C:/user/fconstmtt/Outputs",
#' freq.scenario="C:/user/results/ignitions/clusters_freqs_final.csv",
#' choose.combos=c(1,2),combos.file="C:/user/fconstmtt/Outputs/rmse_combos.csv",
#' group.nxburned.from = 5,
#' obs.nxburned="C:/user/number_of_times_burned.tif",
#' export.plots=1)}
#'
evaluate_BP_nxburned <- function (Folder.Outputs,
                                  freq.scenario,choose.combos,combos.file,obs.nxburned,
                                  group.nxburned.from,
                                  export.plots){

  setwd(Folder.Outputs)
  my_files <- list.files(pattern = "BP.asc")

  obs.nxburned_use <- raster(obs.nxburned)

  my_freqs <- read.csv(freq.scenario)

  my_combos_file <- read.csv(combos.file)

  my_combos_file_order <- my_combos_file[order(my_combos_file$RMSE),]



  if (missing(choose.combos)) {
    nCombos <- my_combos_file_order[1,1]} else {
      nCombos <- choose.combos
    }


  if (choose.combos=="all") {
    nCombos <- my_combos_file_order[,1]}

  if (choose.combos=="best") {
    nCombos <- my_combos_file_order[1,1]}



  names(my_freqs)[names(my_freqs)==names(my_freqs)[1]] <- 'cluster'

  my_freqs_sort <- my_freqs[
    with(my_freqs, order(cluster,WD_use)),
  ]


  correlation_df <- matrix(ncol=3,nrow=length(nCombos))

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


    my_data_i_dur1 <- grep("durclass_1", my_data_dur_1, value = TRUE)
    my_data_i_dur2 <- grep("durclass_2", my_data_dur_2, value = TRUE)
    my_data_i_dur3 <- grep("durclass_3", my_data_dur_3, value = TRUE)
    my_data_i_dur4 <- grep("durclass_4", my_data_dur_4, value = TRUE)
    my_data_i_dur5 <- grep("durclass_5", my_data_dur_5, value = TRUE)





    my_data_i_dur1_cluster1 <- as.data.frame(grep("cluster_1", my_data_i_dur1, value = TRUE))
    colnames(my_data_i_dur1_cluster1)<- "name"


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





    s1_1 <- stack()

    for (j in 1:nrow(my_data_i_dur1_sort)){
      raster_i <- raster(paste(my_data_i_dur1_sort[j,"name"]))
      multiply_my_i <- my_freqs_sort [j,"duration_1"]

      raster_i_multiplied <- raster_i*multiply_my_i

      name_i <- paste("raster_final_",j,sep="")


      s1_1 <- stack(s1_1,raster_i_multiplied)

    }





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





    s2_1 <- stack()

    if(nrow(my_data_i_dur2_sort)>=1){
      for (j in 1:nrow(my_data_i_dur2_sort)){
        raster_i <- raster(paste(my_data_i_dur2_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_2"]

        raster_i_multiplied <- raster_i*multiply_my_i

        name_i <- paste("raster_final_",j,sep="")


        s2_1 <- stack(s2_1,raster_i_multiplied)

      }
    }else{
      s2_1 <- s1_1
      s2_1 <- reclassify(s2_1,c(-Inf,Inf,0))
    }



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





    s3_1 <- stack()

    if(nrow(my_data_i_dur3_sort)>=1){
      for (j in 1:nrow(my_data_i_dur3_sort)){
        raster_i <- raster(paste(my_data_i_dur3_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_3"]

        raster_i_multiplied <- raster_i*multiply_my_i

        name_i <- paste("raster_final_",j,sep="")


        s3_1 <- stack(s3_1,raster_i_multiplied)

      }
    }else{
      s3_1 <- s1_1
      s3_1 <- reclassify(s3_1,c(-Inf,Inf,0))
    }




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




    s4_1 <- stack()

    if(nrow(my_data_i_dur4_sort)>=1){
      for (j in 1:nrow(my_data_i_dur4_sort)){
        raster_i <- raster(paste(my_data_i_dur4_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_4"]

        raster_i_multiplied <- raster_i*multiply_my_i

        name_i <- paste("raster_final_",j,sep="")

        s4_1 <- stack(s4_1,raster_i_multiplied)

      }
    }else{
      s4_1 <- s1_1
      s4_1 <- reclassify(s4_1,c(-Inf,Inf,0))
    }




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





    s5_1 <- stack()


    if(nrow(my_data_i_dur5_sort)>=1){
      for (j in 1:nrow(my_data_i_dur5_sort)){
        raster_i <- raster(paste(my_data_i_dur5_sort[j,"name"]))
        multiply_my_i <- my_freqs_sort [j,"duration_5"]

        raster_i_multiplied <- raster_i*multiply_my_i

        name_i <- paste("raster_final_",j,sep="")


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


    r3 <- crop(obs.nxburned_use,extent(all_rasters_sum))

    r3[is.na(r3[])] <- 0


    sim_vals <- getValues(all_rasters_sum)
    hist_vals_ori <- getValues(r3)
    hist_vals <- getValues(r3)


    percentage_nxburned <- matrix(ncol=2,nrow=length(unique(hist_vals)))

    for (aa in 0:max(hist_vals)){
      hist_vals_loop <- length(hist_vals[hist_vals==aa])/length(hist_vals)*100
      percentage_nxburned[aa+1,1] <- aa
      percentage_nxburned[aa+1,2] <- hist_vals_loop
    }

    percentage_nxburned<- as.data.frame(percentage_nxburned)
    colnames(percentage_nxburned)<-c("nxburned","percentage")

    setwd(paste(Folder.Outputs,"/correlation_BP_NxBurned",sep=""))
    write.csv(percentage_nxburned,"proportion_nxburned_original.csv",row.names = FALSE)



    if (missing(group.nxburned.from)){
      hist_vals <- hist_vals
    } else {
      hist_vals[hist_vals>group.nxburned.from] <- group.nxburned.from

      percentage_nxburned <- matrix(ncol=2,nrow=length(unique(hist_vals)))

      for (aa in 0:max(hist_vals)){
        hist_vals_loop <- length(hist_vals[hist_vals==aa])/length(hist_vals)*100
        percentage_nxburned[aa+1,1] <- aa
        percentage_nxburned[aa+1,2] <- hist_vals_loop
      }

      percentage_nxburned<- as.data.frame(percentage_nxburned)
      colnames(percentage_nxburned)<-c("nxburned","percentage")

      percentage_nxburned[aa+1,1]<- paste(">=",percentage_nxburned[aa+1,1],sep="")

      setwd(paste(Folder.Outputs,"/correlation_BP_NxBurned",sep=""))
      write.csv(percentage_nxburned,"proportion_nxburned_grouped.csv",row.names = FALSE)

    }




    data_for_ggplot <- as.data.frame(cbind(hist_vals,sim_vals))

    store_cor <- cor(data_for_ggplot$hist_vals,data_for_ggplot$sim_vals)
    store_cor_ori <- cor(hist_vals_ori,sim_vals)


    boxplot_bp <- ggplot(data_for_ggplot, aes(x=as.factor(hist_vals), y=as.numeric(sim_vals))) +
      geom_boxplot(fill="grey")+
      scale_color_grey() + theme_classic()+
      theme(text = element_text(size = 16))+
      xlab("Historical number of times burned")+
      ylab("Simulated Burn Probability")+
      scale_x_discrete(labels=c(percentage_nxburned$nxburned))

    if(export.plots==1){
      ggsave(filename=paste(Folder.Outputs,"/correlation_BP_NxBurned","/correlation_BP_nxburned_combo_",i,".jpg",sep=""), plot=boxplot_bp, width = 6, height = 5)

    }


    correlation_df[k,1] <- i
    correlation_df[k,2]<-store_cor_ori
    correlation_df[k,3]<-store_cor

  }


  colnames(correlation_df) <- c("combo","correlation burn probability - number of times burned using all dataset","correlation burn probability - number of times burned group.nxburned.from")
  write.csv(correlation_df,"correlation_BP_NxBurned.csv",row.names = FALSE)








  #figure
  obs.nxburned_use <- rasterToPoints(obs.nxburned_use)
  obs.nxburned_df <-  data.frame(obs.nxburned_use)
  colnames(obs.nxburned_df) <- c("lon", "lat", "Nburned")


  sim_BP_use <- rasterToPoints(all_rasters_sum)
  sim_BP_df <- data.frame(sim_BP_use)
  colnames(sim_BP_df) <- c("lon", "lat", "BP")


  #set the zeros as NA
  obs.nxburned_df[obs.nxburned_df == 0] <- NA
  sim_BP_df[sim_BP_df == 0] <- NA


  #plot
  Ntimes_burned <- ggplot() +
    geom_raster(data = obs.nxburned_df, aes(lon, lat, fill = factor(Nburned))) +
    scale_fill_brewer(palette="YlOrRd",
                      na.value = "grey80",
                      name = "N times burned")+
    theme_void()+
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))+
    xlab("Longitude") + ylab("Latitude")+
    coord_fixed(xlim = c(extent(all_rasters_sum)[1],extent(all_rasters_sum)[2]),
                ylim = c(extent(all_rasters_sum)[3],extent(all_rasters_sum)[4]))+
    ggtitle("Historical Number of Times Burned")



  mid <- max(sim_BP_df$BP, na.rm = T)/2

  burn_probability <- ggplot() +
    geom_raster(data = sim_BP_df, aes(lon, lat, fill = BP)) +
    scale_fill_gradient2(low = "#FFFFB2",
                         mid = "#FD8D3C",
                         high = "#BD0026",
                         midpoint = mid,
                         na.value = "grey80",
                         name = "Burn Probability")+
    theme_void()+
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))+
    xlab("Longitude") + ylab("Latitude")+
    coord_fixed(xlim = c(extent(all_rasters_sum)[1],extent(all_rasters_sum)[2]),
                ylim = c(extent(all_rasters_sum)[3],extent(all_rasters_sum)[4]))+
    ggtitle("Estimated Burn Probability")


  width_measured <- extent(all_rasters_sum)[2]-extent(all_rasters_sum)[1]
  height_measured <- extent(all_rasters_sum)[4]-extent(all_rasters_sum)[3]

  ratio_use <- width_measured/height_measured

  theme_get()$plot.margin

  #join the two plots
  library(ggpubr)
  final_figure <- ggarrange(Ntimes_burned, burn_probability,
                            ncol = 2, align = "h")


  #save it as a figure
  ggsave(paste(Folder.Outputs,"/correlation_BP_NxBurned","/Nxburned_and_BP.jpeg",sep=""),
         final_figure,
         width = 20*ratio_use, height = 20, dpi = 150, units = "cm")




}
