#' Get meteorological variables from an existing netcdf file
#' @description Get meteorological variables from an existing netcdf file and stores it in a csv file.
#' @param study.area Shapefile with the limits of the study area (polygon). Must not contain more than one polygon.
#' @param my.fires Polygon shapefile containing the dated fire perimeters. The shapefile must contain a field with a unique id per fire perimeter (ID), a field with the burned area per perimeter in hectares (Area_ha), a field with the date of start of the fire (Date_ini) and the end of the fire (Date_end). The fields Date_ini and Date_end must follow the format yyyy-mm-dd.
#' @param nc.folder Path to the folder with the netcdf file.
#' @param output.folder Path to the folder where the outputs should be saved.
#'
#' @return Returns a csv file with the hourly meteorological data per fire perimeter in the study area (fire_weather_study_area.csv)
#' @export
#' @import lubridate sp tidyverse ecmwfr ncdf4 udunits2 sf rgdal raster rgeos stringr zoo lwgeom tibble
#' @examples
#' \dontrun{fire_weather_nc(study.area="C:/user/study_area.shp",
#' my.fires="C:/user/my_fires.shp",nc.folder="C:/user/netdcf_file.nc",
#' output.folder="C:/user/results")}
#'
fire_weather_nc <- function(study.area, my.fires,nc.folder,output.folder) {

  my_fires <- readOGR(my.fires)

  #my_fires$ID <- 1:nrow(my_fires)

  my_fires <- gBuffer(my_fires,width=0,byid=TRUE)

  my_fires_t <- spTransform(my_fires, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  study_area <- readOGR(study.area)
  study_area <- spTransform(study_area, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  my_fires_t <- st_as_sf(my_fires_t)
  study_area <- st_as_sf(study_area)

  my_fires_t$ID_use <- 1:nrow(my_fires_t)


  fire_inside_study_area <- suppressMessages(st_intersects(study_area,my_fires_t))

  fire_inside_study_area_df <- as.data.frame(fire_inside_study_area)


  my_fires_t_inside_study_area <- my_fires_t[my_fires_t$ID_use %in% fire_inside_study_area_df$col.id,]

  names(my_fires_t_inside_study_area)[names(my_fires_t_inside_study_area) == 'Date_ini'] <- 'Data_ini'
  names(my_fires_t_inside_study_area)[names(my_fires_t_inside_study_area) == 'Date_end'] <- 'Data_end'


  my_fires_t_dated <- subset(my_fires_t_inside_study_area, Data_ini!="NaN" | Data_end!="NaN")

  length(my_fires_t_dated)

  #head(my_fires_t_dated@data)
  #str(my_fires_t_dated@data)

  my_fires_t_dated$date_diff <- as.Date(as.character(my_fires_t_dated$Data_end), format="%Y-%m-%d")-
    as.Date(as.character(my_fires_t_dated$Data_ini), format="%Y-%m-%d")

  head(my_fires_t_dated)





  itemizeDates <- function(startDate="2020-12-28", endDate="2020-12-30",
                           format="%Y-%m-%d") {
    startDate <- as.Date(startDate) - as.difftime(1, unit="days") #esta parte ? para corrigir para termos UTC+1 sempre
    out <- seq(as.Date(startDate, format=format),
               as.Date(endDate, format=format), by="days")
    format(out, format)
  }

  itemizeDates(startDate="2020-1-27", endDate="2020-2-21")



  results <- data.frame()


  for (q in 1:nrow(my_fires_t_dated)) {
    days_fire <- itemizeDates(startDate=my_fires_t_dated$Data_ini[q], endDate=my_fires_t_dated$Data_end[q])
    results_pt <- as.data.frame(days_fire)
    results <- rbind(results,results_pt)
  }

  days_fire <- results


  #remove duplicated dates
  days_fire_final <- days_fire[!duplicated(days_fire), ]


  setwd(nc.folder)
  my_data_temp <- nc_open("era5_weather_study_area.nc")

  lon <- ncvar_get(my_data_temp,"longitude")
  lat <- ncvar_get(my_data_temp,"latitude")

  lat <- round(lat,1)
  lon <- round(lon,1)

  #plot(lat~lon)

  t <- ncvar_get(my_data_temp, "time")

  timestamp_use <- as_datetime(c(t*60*60),origin="1900-01-01")
  t2m.array <- ncvar_get(my_data_temp, "t2m")
  d2m.array <- ncvar_get(my_data_temp, "d2m")
  u10.array <- ncvar_get(my_data_temp, "u10")
  v10.array <- ncvar_get(my_data_temp, "v10")



  fillvalue_t2m <-ncatt_get(my_data_temp,"t2m","_FillValue")
  fillvalue_d2m <-ncatt_get(my_data_temp,"d2m","_FillValue")
  fillvalue_u10 <-ncatt_get(my_data_temp,"u10","_FillValue")
  fillvalue_v10 <-ncatt_get(my_data_temp,"v10","_FillValue")

  t2m.array[t2m.array==fillvalue_t2m$value] <- NA
  t2m.array[d2m.array==fillvalue_d2m$value] <- NA
  t2m.array[u10.array==fillvalue_u10$value] <- NA
  t2m.array[v10.array==fillvalue_v10$value] <- NA

  nc_close(my_data_temp)


  #t2m.slice <- t2m.array[,,1] #vou buscar o primeiro dia que saquei


  #calcular RH

  t2m.array <- t2m.array - 273.15
  d2m.array <- d2m.array - 273.15



  RH.array <- 100*(+exp((17.625*d2m.array)/(243.04+d2m.array))/exp((17.625*t2m.array)/(243.04+t2m.array)))


  #RH <- 100*(+exp((17.625*21.11)/(243.04+21.11))/exp((17.625*35)/(243.04+35)))



  WD.array <- 180+(180/pi)*atan2(u10.array,v10.array)
  WS.array <- sqrt ((u10.array)^2+(v10.array)^2) #WS is given in m/s-1

  WS.array <- WS.array*3.6









  #coordinates
  #lat_df <- do.call(rbind.data.frame, lat)
  #lon_df <- do.call(rbind.data.frame, lon)

  lat_df <- as.data.frame (lat)
  lon_df <- as.data.frame (lon)

  #cords_test <- cbind(lat_df,lon_df)

  coordenadas <- expand.grid(lat,lon)
  #plot(cords_test)


  colnames(coordenadas)<- c("lati","long")

  coordinates(coordenadas) <- ~long+lati
  proj4string(coordenadas) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  coordenadas <- st_as_sf(coordenadas,coords = 1:2)

  #plot(coordenadas,add=T)
  #plot(coordenadas)



  #nrow(lon_df)

  #lat_df_use <- lat_df[,1]
  #lon_df_use <- lon_df[1,]

  #test_df$lat <- lat_df_use
  #test_df$lon <- lat_df_use

  #days_fire_final_t <- gsub("\\ .*","",timestamp_use)
  #days_fire_final <- days_fire_final_t[!duplicated(days_fire_final_t)]

  days_fire_final_sorted <- sort(days_fire_final)
  #timestamp_use


  ###ciclo ir buscar meteo#####



  my_fires_t_dated_df <- as.data.frame(my_fires_t_dated)

  results <- matrix(ncol=7, nrow= (length(my_fires_t_dated)+10000))
  result_hours <- matrix(ncol=12, nrow= (length(my_fires_t_dated)+15000*24))
  #nrow(result_hours)
  #nrow(results)

  result_hours_test <- result_hours

  newMatrix <- rbind(result_hours_test, matrix(data=NA, ncol=12, nrow=150000))
  #nrow(newMatrix)
  #head(newMatrix)
  #tail(newMatrix)

  result_hours<- newMatrix


  my_fires_t_dated_2020<- my_fires_t_dated

  my_fires_t_dated_2020$date_diff_t <- ifelse (my_fires_t_dated_2020$date_diff==0,1, my_fires_t_dated_2020$date_diff)

  my_fires_t_dated_2020$Area_ha<-as.numeric(my_fires_t_dated_2020$Area_ha)
  my_fires_t_dated_2020$date_diff_t<-as.numeric(my_fires_t_dated_2020$date_diff_t)

  my_fires_t_dated_2020 <- subset(my_fires_t_dated_2020,!(date_diff_t > 10 & Area_ha < 500))

  library(stringr)
  my_fires_t_dated_2020_separate <- as.data.frame(str_split_fixed(my_fires_t_dated_2020$Data_ini, "-", 3))

  #str(my_fires_t_dated_2020_separate)
  my_fires_t_dated_2020_separate$V2<-as.numeric(my_fires_t_dated_2020_separate$V2)

  #max (my_fires_t_dated_2020_separate$V2)

  #subset(my_fires_t_dated_2020_separate,V2>12)


  #sum(my_fires_t_dated_2020$date_diff_t, na.rm = T)

  #options(warn=-1)




  #my_fires_t_dated_2020 <- my_fires_t_dated

  timestamp_use_loop <- format(as.POSIXct(timestamp_use,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')

  #pb = txtProgressBar(min = 0, max = nrow(my_fires_t_dated_2020), initial = 1)


  for (i in 1:nrow(my_fires_t_dated_2020)){ #length(my_fires_t_dated_2020)

    #my_id_use <- "4896"

    my_id_use <- my_fires_t_dated_2020$ID[i]
    my_fires_t_dated_loop <-  subset(my_fires_t_dated_2020, my_fires_t_dated_2020$ID == my_id_use) #my_id_use "6283" "5985" #"25140" #"29216"
    #head(my_fires_t_dated_loop@data)

    days_fire <- itemizeDates(startDate=my_fires_t_dated_loop$Data_ini, endDate=my_fires_t_dated_loop$Data_end)

    class(timestamp_use_loop)



    results_j <- data.frame()
    for(j in 1:length(days_fire)){

      #use_lon <- unique(seal_coords$lon)
      #use_lat <- unique(seal_coords$lat)



      g <- which(timestamp_use_loop==days_fire[j])
      g <- as.data.frame(g)

      results_j <- rbind(results_j,g)

    }

    posicoes_dias <- results_j

    require(abind)



    if (length(as.numeric(dim(RH.array)))<3){
      RH.array_t <- abind( RH.array, RH.array, along=3 )

      length_use <- as.numeric(dim(RH.array))
      length_use_lon <- nrow(lon_df)
      length_use_lat <- nrow(lat_df)


      RH.array <- array(data = c(unlist(RH.array, lon_df, lat_df)),
                        dim = c(length_use_lon, length_use_lat, length_use[2]))

      WD.array <- array(data = c(unlist(WD.array, lon_df, lat_df)),
                        dim = c(length_use_lon, length_use_lat, length_use[2]))

      WS.array <- array(data = c(unlist(WS.array, lon_df, lat_df)),
                        dim = c(length_use_lon, length_use_lat, length_use[2]))

      t2m.array <- array(data = c(unlist(t2m.array, lon_df, lat_df)),
                         dim = c(length_use_lon, length_use_lat, length_use[2]))

    }

    RH.array_loop <- RH.array[,,posicoes_dias$g]
    WD.array_loop <- WD.array[,,posicoes_dias$g]
    WS.array_loop <- WS.array[,,posicoes_dias$g]
    t2m.array_loop <- t2m.array[,,posicoes_dias$g]





    #look into the meteo files to get the weather conditions of those days
    #days_fire

    #  days_fire_vs1 <- gsub("-","",days_fire)
    #paste0(days_fire_vs1,"00")

    # seq_vs1 <- seq(00,23,by=1)
    #
    # paste0(days_fire_vs1,seq_vs1)
    #
    days_fire_vs2 <- rep(days_fire, each=24)




    seq_vs2 <- sprintf("%02d", 0:23)
    seq_vs3 <- rep(seq_vs2,length(days_fire))

    files_to_get <- paste(days_fire_vs2,seq_vs3,sep="-")

    ###para j?, n?o uso####
    #files_to_get <- files_to_get[24:(length(files_to_get)-1)] #esta parte ? necess?ria para corrigir o utc+1 no ver?o. Como os inc?ndios ocorrem mais de ver?o, estou a assumir que estamos sempre no hor?rio utc +1



    #posicoes_dias










    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }


    #
    #
    # ###files to get rasters####
    # hours_to_get <- substrRight(files_to_get, 2)
    # hours_to_get<- paste0(hours_to_get,"00h")
    #
    # year_to_get <- substr(files_to_get, 1, 4)
    #
    #
    # months_to_get_int <- substring(files_to_get, 5)
    # months_to_get <- substr(months_to_get_int, 1, 2)
    #
    #
    # day_to_get_int <- substring(files_to_get, 7)
    # day_to_get <- substr(day_to_get_int, 1, 2)
    #
    #
    # files_to_get_ws <- paste(day_to_get,months_to_get,year_to_get,hours_to_get,sep="-")
    #
    #
    # days_fire_vs1 <- gsub("-","",days_fire)
    # #paste0(days_fire_vs1,"00")
    #
    # # seq_vs1 <- seq(00,23,by=1)
    # #
    # # paste0(days_fire_vs1,seq_vs1)
    # #
    # days_fire_vs2 <- rep(days_fire_vs1, each=24)
    #
    # seq_vs2 <- sprintf("%02d", 0:23)
    # seq_vs3 <- rep(seq_vs2,length(days_fire))
    #
    # files_to_get <- paste0(days_fire_vs2,seq_vs3)
    #
    # files_to_get <- files_to_get[24:(length(files_to_get)-1)] #esta parte ? necess?ria para corrigir o utc+1 no ver?o. Como os inc?ndios ocorrem mais de ver?o, estou a assumir que estamos sempre no hor?rio utc +1
    #
    # files_to_get_fwi <- days_fire_vs2[25:(length(days_fire_vs2))]
    #
    # #plot(test_r)
    # #plot(my_fires_t_dated_loop,add=T)
    # #plot(my_fires_t_dated_loop)
    #
    # #plot(coordenadas,add=T)
    #
    #
    #
    #
    #
    #

    #se o poligono do fogo tiver pelo menos um ponto dentro dele

    coordenadas_st <- st_as_sf(coordenadas)



    #my_fires_t_dated_loop<-st_buffer(my_fires_t_dated_loop, byid = T, dist = 0)


    my_fires_t_dated_loop_st <- st_as_sf(my_fires_t_dated_loop)


    #st_is_valid(my_fires_t_dated_loop_st)
    #st_is_valid(coordenadas_st)


    out <- suppressMessages(st_intersection(coordenadas_st, my_fires_t_dated_loop_st))

    #out_fwi <- st_intersection(coordenadas_fwi, my_fires_t_dated_loop_st)


    #plot(out,add=T,col="blue")
    #plot(out[4,],add=T,col="yellow")



    rm(seal_coords)
    rm(sacar_col)
    rm(sacar_linha)

    if (dim(out)[1] != 0) {


      #out$geometry

      seal_coords <- do.call(rbind, st_geometry(out)) %>%
        as_tibble() %>% setNames(c("lon","lat"))

      seal_coords <- as.data.frame(seal_coords)

      seal_coords$lat <- round(seal_coords$lat,1)
      seal_coords$lon <- round(seal_coords$lon,1)

      #now we need to get the position of these points in the matrix

      #seal_coords$lon

      #lon$lon
      #which(lon$lon == seal_coords$lon)/71


      sacar_linha <- which(lon %in% seal_coords$lon,arr.ind = TRUE)
      #s? me interessa a coluna

      sacar_col <- which(lat %in% seal_coords$lat,arr.ind = TRUE)
      #s? me interessa a linha




      # if (dim(out_fwi)[1] == 0) {
      #   b <- gBuffer(my_fires_t_dated_loop, byid=TRUE,width=0.2)
      #   #plot(my_fires_t_dated_loop,add=T)
      #   #plot(b)
      #   #plot(coordenadas,add=T)
      #   #plot(coordenadas_fwi,add=T,col="blue")
      #
      #   #coordenadas_st <- st_as_sf(coordenadas)
      #   b_st <- st_as_sf(b)
      #   my_fires_t_dated_loop_st <- st_as_sf(my_fires_t_dated_loop)
      #
      #   b_st <- st_buffer(b_st,0)
      #
      #   #class(b_st)
      #   #class(b)
      #
      #   #st_is_valid(b_st)
      #   #st_is_valid(coordenadas_st)
      #
      #
      #   #out <- st_intersection(coordenadas_st, b_st)
      #   out_fwi <- st_intersection(coordenadas_fwi, b_st)
      #
      #   #plot(out,add=T,col="blue")
      #   #plot(out[4,],add=T,col="yellow")
      #
      #
      #   #gDists <- st_distance(my_fires_t_dated_loop_st, out)
      #   gDists_fwi <- st_distance(my_fires_t_dated_loop_st, out_fwi)
      #
      #   #min(gDists)
      #
      #   #out$geometry
      #
      #   #get_min <- out$geometry[which.min(gDists)]
      #   get_min_fwi <- out_fwi$geometry[which.min(gDists_fwi)]
      #   #class(get_min)
      #
      #
      #   seal_coords_fwi <- do.call(rbind, st_geometry(get_min_fwi)) %>%
      #     as_tibble() %>% setNames(c("lon","lat"))
      #
      #   seal_coords_fwi <- as.data.frame(seal_coords_fwi)
      # } else {
      #
      #   seal_coords_fwi <- do.call(rbind, st_geometry(get_min_fwi)) %>%
      #     as_tibble() %>% setNames(c("lon","lat"))
      #
      #   seal_coords_fwi <- as.data.frame(seal_coords_fwi)
      #
      # }
      #
      #
      #
      # sacar_col_fwi <- which(lon_fwi$ilon == seal_coords_fwi$lon,arr.ind = TRUE)
      # #s? me interessa a coluna
      #
      # sacar_linha_fwi <- which(lat_fwi$ilat == seal_coords_fwi$lat,arr.ind = TRUE)
      # #s? me interessa a linha
      #



      sacar_linha_fin <- as.numeric(unique(sacar_linha))
      sacar_col_fin <- as.numeric(unique(sacar_col))


      seal_coords$posicao_row<-NA
      seal_coords$posicao_col<-NA


      #
      # sacar_linha_fin_fwi <- as.numeric(unique(sacar_linha_fwi[,1]))
      # sacar_col_fin_fwi <- as.numeric(unique(sacar_col_fwi[,2]))
      #
      #
      # seal_coords$posicao_row_fwi<-NA
      # seal_coords$posicao_col_fwi<-NA
      #

      seal_coords$lat <- round(seal_coords$lat,1)
      seal_coords$lon <- round(seal_coords$lon,1)

      for(j in 1:nrow(seal_coords)){
        sacar_col_loop <- which(lat == seal_coords$lat[j],arr.ind = TRUE)

        sacar_linha_loop <- which(lon == seal_coords$lon[j],arr.ind = TRUE)

        #posicao_sacar <- sacar_col_loop[sacar_linha_loop[1,1],]

        seal_coords[j,3] <- as.numeric(sacar_linha_loop[1])
        seal_coords[j,4] <- as.numeric(sacar_col_loop[1])
      }


      #
      # for(j in 1:nrow(seal_coords_fwi)){
      #   sacar_col_loop_fwi <- which(lon_fwi$ilon == seal_coords_fwi$lon[j],arr.ind = TRUE)
      #
      #   sacar_linha_loop_fwi <- which(lat_fwi$ilat == seal_coords_fwi$lat[j],arr.ind = TRUE)
      #
      #   posicao_sacar_fwi <- sacar_col_loop_fwi[sacar_linha_loop_fwi[1,1],]
      #
      #   seal_coords[j,5] <- as.numeric(posicao_sacar_fwi[1])
      #   seal_coords[j,6] <- posicao_sacar_fwi[2]
      #
      # }


      library(zoo)
      seal_coords <- na.locf(seal_coords)

      #my_fires_t_dated_df[i,7] <- sacar_linha_fin
      #my_fires_t_dated_df[i,7] <- sacar_col_fin



      #test$map[sacar_linha_fin,sacar_col_fin]

      NAindex <- which(is.na(results))
      firstNA <- min(NAindex)


      #length(seal_coords)

      start_results <- firstNA
      end_results <- firstNA+nrow(seal_coords)-1


      #use_lon <- unique(seal_coords$lon)
      #use_lat <- unique(seal_coords$lat)



      results[start_results:end_results,1]<- my_fires_t_dated_loop_st$ID
      results[start_results:end_results,2]<- seal_coords$posicao_row
      results[start_results:end_results,3]<- seal_coords$posicao_col
      results[start_results:end_results,4]<- seal_coords$lon
      results[start_results:end_results,5]<- seal_coords$lat
      #results[start_results:end_results,6]<- seal_coords$posicao_row_fwi
      #results[start_results:end_results,7]<- seal_coords$posicao_col_fwi

      #head(results,20)
    }






    #se o poligono do fogo n?o tiver nenhum ponto dentro dele, vou fazer um buffer ? sua volta e selecionar os pontos
    #que caem dentro do buffer. Depois vou aplicar uma medida de dist?ncia e usar apenas o ponto mais pr?ximo


    if (dim(out)[1] == 0) {
      b <- suppressMessages(sf::st_buffer(my_fires_t_dated_loop, dist=10000)) #byid=TRUE,

      #plot(b)
      #plot(my_fires_t_dated_loop,add=T)
      #plot(coordenadas,add=T)
      #plot(coordenadas_fwi,add=T,col="blue")



      coordenadas_st <- st_as_sf(coordenadas)
      b_st <- st_as_sf(b)
      my_fires_t_dated_loop_st <- st_as_sf(my_fires_t_dated_loop)

      b_st <- suppressMessages(st_buffer(b_st,0))

      #class(b_st)
      #class(b)

      #st_is_valid(b_st)
      #st_is_valid(coordenadas_st)


      out <- suppressMessages(st_intersection(coordenadas_st, b_st))

      ###para j?, o fwi est? off####
      #out_fwi <- st_intersection(coordenadas_fwi, b_st)

      #plot(out,add=T,col="blue")
      #plot(out[4,],add=T,col="yellow")


      gDists <- st_distance(my_fires_t_dated_loop_st, out)

      #gDists_fwi <- st_distance(my_fires_t_dated_loop_st, out_fwi)

      #min(gDists)

      #out$geometry

      get_min <- out$geometry[which.min(gDists)]
      #get_min_fwi <- out_fwi$geometry[which.min(gDists_fwi)]
      #class(get_min)


      seal_coords <- do.call(rbind, st_geometry(get_min)) %>%
        as_tibble() %>% setNames(c("lon","lat"))

      seal_coords <- as.data.frame(seal_coords)



      #seal_coords_fwi <- do.call(rbind, st_geometry(get_min_fwi)) %>%
      #  as_tibble() %>% setNames(c("lon","lat"))

      #seal_coords_fwi <- as.data.frame(seal_coords_fwi)


      #now we need to get the position of these points in the matrix

      #seal_coords$lon

      #lon$lon
      #which(lon$lon == seal_coords$lon)/71




      seal_coords$lat <- round(seal_coords$lat,1)
      seal_coords$lon <- round(seal_coords$lon,1)

      sacar_linha <- which(lon %in% seal_coords$lon,arr.ind = TRUE)
      #s? me interessa a coluna

      sacar_col <- which(lat %in% seal_coords$lat,arr.ind = TRUE)
      #s? me interessa a linha



      #sacar_col_fwi <- which(lon_fwi$ilon == seal_coords_fwi$lon,arr.ind = TRUE)
      #s? me interessa a coluna

      #sacar_linha_fwi <- which(lat_fwi$ilat == seal_coords_fwi$lat,arr.ind = TRUE)
      #s? me interessa a linha




      #sacar_linha_fin <- as.numeric(unique(sacar_linha[,1]))
      #sacar_col_fin <- as.numeric(unique(sacar_col[,1]))

      sacar_linha_fin <- as.numeric(unique(sacar_linha))
      sacar_col_fin <- as.numeric(unique(sacar_col))


      seal_coords$posicao_row<-NA
      seal_coords$posicao_col<-NA


      #is_all_na <- round(WS.array[1,1,],2)
      is_all_na <- round(WS.array[sacar_linha_fin,sacar_col_fin,],2)

      if(all(is.na(is_all_na))==TRUE) {

        out$gDists <- as.numeric(gDists)

        gDists_sort <- out[order(gDists),]

        get_min <- gDists_sort[2,]

        #get_min_fwi <- out_fwi$geometry[which.min(gDists_fwi)]
        #class(get_min)


        seal_coords <- do.call(rbind, st_geometry(get_min)) %>%
          as_tibble() %>% setNames(c("lon","lat"))

        seal_coords <- as.data.frame(seal_coords)
      }


      #seal_coords_fwi <- do.call(rbind, st_geometry(get_min_fwi)) %>%
      #  as_tibble() %>% setNames(c("lon","lat"))

      #seal_coords_fwi <- as.data.frame(seal_coords_fwi)


      #now we need to get the position of these points in the matrix

      #seal_coords$lon

      #lon$lon
      #which(lon$lon == seal_coords$lon)/71




      seal_coords$lat <- round(seal_coords$lat,1)
      seal_coords$lon <- round(seal_coords$lon,1)

      sacar_linha <- which(lon %in% seal_coords$lon,arr.ind = TRUE)
      #s? me interessa a coluna

      sacar_col <- which(lat %in% seal_coords$lat,arr.ind = TRUE)
      #s? me interessa a linha



      #sacar_col_fwi <- which(lon_fwi$ilon == seal_coords_fwi$lon,arr.ind = TRUE)
      #s? me interessa a coluna

      #sacar_linha_fwi <- which(lat_fwi$ilat == seal_coords_fwi$lat,arr.ind = TRUE)
      #s? me interessa a linha




      # sacar_linha_fin <- as.numeric(unique(sacar_linha[,1]))
      # sacar_col_fin <- as.numeric(unique(sacar_col[,1]))

      sacar_linha_fin <- as.numeric(unique(sacar_linha))
      sacar_col_fin <- as.numeric(unique(sacar_col))

      seal_coords$posicao_row<-NA
      seal_coords$posicao_col<-NA


      is_all_na <- round(WS.array[sacar_linha_fin,sacar_col_fin,],2)





      #sacar_linha_fin_fwi <- as.numeric(unique(sacar_linha_fwi[,1]))
      #sacar_col_fin_fwi <- as.numeric(unique(sacar_col_fwi[,2]))


      #seal_coords$posicao_row_fwi<-NA
      #seal_coords$posicao_col_fwi<-NA

      seal_coords$lat <- round(seal_coords$lat,1)
      seal_coords$lon <- round(seal_coords$lon,1)

      for(j in 1:nrow(seal_coords)){
        sacar_col_loop <- which(lat == seal_coords$lat[j],arr.ind = TRUE)

        sacar_linha_loop <- which(lon == seal_coords$lon[j],arr.ind = TRUE)

        #posicao_sacar <- sacar_col_loop[sacar_linha_loop[1,1],]

        seal_coords[j,3] <- as.numeric(sacar_linha_loop[1])
        seal_coords[j,4] <- as.numeric(sacar_col_loop[1])




        # sacar_col_loop_fwi <- which(lon_fwi$ilon == seal_coords_fwi$lon[j],arr.ind = TRUE)
        #
        # sacar_linha_loop_fwi <- which(lat_fwi$ilat == seal_coords_fwi$lat[j],arr.ind = TRUE)
        #
        # posicao_sacar_fwi <- sacar_col_loop_fwi[sacar_linha_loop_fwi[1,1],]
        #
        # seal_coords[j,5] <- as.numeric(posicao_sacar_fwi[1])
        # seal_coords[j,6] <- as.numeric(posicao_sacar_fwi[2])

        #}





        #my_fires_t_dated_df[i,7] <- sacar_linha_fin
        #my_fires_t_dated_df[i,7] <- sacar_col_fin



        #test$map[sacar_linha_fin,sacar_col_fin]

        NAindex <- which(is.na(results))
        firstNA <- min(NAindex)


        #length(seal_coords)

        start_results <- firstNA
        end_results <- firstNA+nrow(seal_coords)-1


        #use_lon <- unique(seal_coords$lon)
        #use_lat <- unique(seal_coords$lat)



        results[start_results:end_results,1]<- my_fires_t_dated_loop_st$ID
        results[start_results:end_results,2]<- seal_coords$posicao_row
        results[start_results:end_results,3]<- seal_coords$posicao_col
        results[start_results:end_results,4]<- seal_coords$lon
        results[start_results:end_results,5]<- seal_coords$lat
        #results[start_results:end_results,6]<- seal_coords$posicao_row_fwi
        #results[start_results:end_results,7]<- seal_coords$posicao_col_fwi


        #head(results,20)



      }}









    #
    # setwd("C:/Users/aparicio/Desktop/autopackage/meteo/t2m/era5/3.1")
    #
    # #let us start with the mat files
    #
    # files_to_get_use <- paste0(files_to_get,".mat")
    #
    # files_to_get_ws_use <- paste0(files_to_get_ws,"_wsp.spd")
    #
    # files_to_get_wd_use <- paste0(files_to_get_ws,"_waz.dir")
    #
    # files_to_get_use_fwi <- paste0(files_to_get_fwi,".mat")
    #
    #test <- readMat(paste0(files_to_get,".mat"))

    #myfiles = readMat(lapply(files_to_get_use, read.delim))

    #files <- list.files()

    # if(file.exists(files_to_get_use)){
    #
    #   data1 <- lapply(files_to_get_use, readMat)
    #
    #   setwd("C:/Users/aparicio/Desktop/autopackage/meteo/rh/era5/3.1")
    #   data2 <- lapply(files_to_get_use, readMat)
    #
    #   setwd("C:/Users/aparicio/Desktop/autopackage/meteo/wind_speed/era5/3.1")
    #   data3 <- lapply(files_to_get_ws_use, read.matrix,skip=6)
    #
    #
    #   setwd("C:/Users/aparicio/Desktop/autopackage/meteo/wind_dir/era5/3.1")
    #   data4 <- lapply(files_to_get_wd_use, read.matrix,skip=6)
    #
    #   setwd("C:/Users/aparicio/Desktop/autopackage/meteo/fwi/ecmwf/v4.0")
    #   data5 <- lapply(files_to_get_use_fwi, readMat)
    #   #data5[[1]]$Mat[1]
    #
    #   results_subset_my_points$linha_mat<- as.numeric(results_subset_my_points$linha_mat)
    #   results_subset_my_points$col_mat<- as.numeric(results_subset_my_points$col_mat)
    #
    #   results_subset_my_points$linha_mat_fwi<- as.numeric(results_subset_my_points$linha_mat_fwi)
    #   results_subset_my_points$col_mat_fwi<- as.numeric(results_subset_my_points$col_mat_fwi)
    #




    results_df <- as.data.frame(results)


    results_subset_my_points <- as.data.frame(results_df[start_results:end_results,])



    #for(k in 1:length(files_to_get_use)){
    for(l in 1:nrow(results_subset_my_points)){

      results_subset_my_points_loop <- results_subset_my_points[l,]

      colnames(results_subset_my_points_loop)<- c("ID","linha_mat","col_mat","long","lati","linha_mat_fwi","col_mat_fwi")
      colnames(results_subset_my_points)<- c("ID","linha_mat","col_mat","long","lati","linha_mat_fwi","col_mat_fwi")
      #head(results)

      #results_df <- as.data.frame(results)

      #results_subset_my_points <- as.data.frame(results_df[start_results:end_results,])

      #results_subset_my_points <- data.frame(lapply(results_subset_my_points, function(x) as.numeric(as.character(x))))

      results_subset_my_points_loop$linha_mat <- as.numeric(results_subset_my_points_loop$linha_mat)
      results_subset_my_points_loop$col_mat <- as.numeric(results_subset_my_points_loop$col_mat)

      #str(results_subset_my_points_loop)

      WS.array_loop_data <- round(WS.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)
      WD.array_loop_data <- round(WD.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)
      RH.array_loop_data <- round(RH.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)
      t2m.array_loop_data <- round(t2m.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)



      weather_loop <- cbind(results_subset_my_points_loop$ID,WS.array_loop_data,WD.array_loop_data,RH.array_loop_data,t2m.array_loop_data,files_to_get)
      #head(weather_loop,5)
      #tail(weather_loop,5)

      colnames(weather_loop) <- c("ID","WS","WD","RH","T","date")




      #
      #
      # temperature <- data1[[k]]$map[results_subset_my_points$linha_mat[l],results_subset_my_points$col_mat[l]]
      # HR <- data2[[k]]$RH[results_subset_my_points$linha_mat[l],results_subset_my_points$col_mat[l]]
      # WS <- data3[[k]][results_subset_my_points$linha_mat[l],results_subset_my_points$col_mat[l]]
      # WD <- data4[[k]][results_subset_my_points$linha_mat[l],results_subset_my_points$col_mat[l]]
      # fwi <- data5[[k]]$omap[[3]][results_subset_my_points$linha_mat_fwi[l],results_subset_my_points$col_mat_fwi[l]]
      # #datalist[k] <- g
      #
      NAindex <- which(is.na(result_hours))
      firstNA <- min(NAindex)


      #length(seal_coords)

      start_results <- firstNA
      end_results <- start_results + nrow(weather_loop)-1


      #esta parte ? para corrigir o UTC+1. Devia meter isto no script para ser o user a definir
      old_date_hours <- as.character(gsub("-","",files_to_get))
      as.Date(old_date_hours,format="%Y%m%d%H")

      new_date_hours_almost <- strptime(old_date_hours, format='%Y%m%d%H')

      new_date_hours <- as.character(new_date_hours_almost + 3601)


      new_date_hours_vs1 <- gsub("-","",new_date_hours)
      new_date_hours_vs1 <- gsub(" ","",new_date_hours_vs1)

      new_date_hours_vs2 <- substr(new_date_hours_vs1,1,10)


      weather_loop <- cbind(results_subset_my_points_loop$ID,WS.array_loop_data,WD.array_loop_data,RH.array_loop_data,t2m.array_loop_data,files_to_get)

      id_use_table <- rep(results_subset_my_points$ID[l],nrow(weather_loop))
      linha_mat_table <- rep(results_subset_my_points$linha_mat[l],nrow(weather_loop))
      col_mat_table <- rep(results_subset_my_points$col_mat[l],nrow(weather_loop))
      long_table <- rep(results_subset_my_points$long[l],nrow(weather_loop))
      lati_table <- rep(results_subset_my_points$lati[l],nrow(weather_loop))

      result_hours[start_results:end_results,1]<-id_use_table
      result_hours[start_results:end_results,2]<-linha_mat_table
      result_hours[start_results:end_results,3]<-col_mat_table
      result_hours[start_results:end_results,4]<-round(as.numeric(long_table),2)
      result_hours[start_results:end_results,5]<-round(as.numeric(lati_table),2)
      result_hours[start_results:end_results,6]<-as.numeric(old_date_hours)
      result_hours[start_results:end_results,7]<-as.numeric(new_date_hours_vs2)
      result_hours[start_results:end_results,8]<-t2m.array_loop_data
      result_hours[start_results:end_results,9]<-RH.array_loop_data
      result_hours[start_results:end_results,10]<-WS.array_loop_data
      result_hours[start_results:end_results,11]<-WD.array_loop_data
      #result_hours[start_results,12]<-fwi
    }#}

    # }else{
    #   data1<-NA
    #   data2<-NA
    #   data3<-NA
    #   data4<-NA
    #   data5<-NA
    #
    #   NAindex <- which(is.na(result_hours))
    #   firstNA <- min(NAindex)
    #
    #
    #   temperature <- -9999
    #   HR <- -9999
    #   WS <- -9999
    #   WD <- -9999
    #   fwi <- -9999
    #
    #
    #
    #
    #   old_date_hours <- as.character(gsub(".mat","",files_to_get_use[k]))
    #   as.Date(old_date_hours,format="%Y%m%d%H")
    #
    #   new_date_hours_almost <- strptime(old_date_hours, format='%Y%m%d%H')
    #
    #   new_date_hours <- as.character(new_date_hours_almost + 3601)
    #
    #
    #   new_date_hours_vs1 <- gsub("-","",new_date_hours)
    #   new_date_hours_vs1 <- gsub(" ","",new_date_hours_vs1)
    #
    #   new_date_hours_vs2 <- substr(new_date_hours_vs1,1,10)
    #
    #
    #   #length(seal_coords)
    #
    #   start_results <- firstNA
    #
    #   result_hours[start_results,1]<-results_subset_my_points$ID[l]
    #   result_hours[start_results,2]<-results_subset_my_points$linha_mat[l]
    #   result_hours[start_results,3]<-results_subset_my_points$col_mat[l]
    #   result_hours[start_results,4]<-results_subset_my_points$long[l]
    #   result_hours[start_results,5]<-results_subset_my_points$lati[l]
    #   result_hours[start_results,6]<-as.numeric(gsub(".mat","",files_to_get_use[k]))
    #   result_hours[start_results,7]<-as.numeric(new_date_hours_vs2)
    #   result_hours[start_results,8]<-temperature
    #   result_hours[start_results,9]<-HR
    #   result_hours[start_results,10]<-WS
    #   result_hours[start_results,11]<-WD
    #   result_hours[start_results,12]<-fwi
    # }

    #datalist <- list()

    #result_hours$hour <- NA



    colnames(result_hours)<- c("ID","row","col","lon","lat","day_files","day_UTC_corrected","temperature","RH","WS","WD","FWI")



    #setTxtProgressBar(pb,i)

    # extra <- nchar('||100%')
    # width <- 98
    # step <- round(i / nrow(my_fires_t_dated_2020) * (width - extra))
    #
    # progress_text<-sprintf('|%s%s|% 3s%%', strrep('=', step),
    #         strrep(' ', width - step - extra), round(i / nrow(my_fires_t_dated_2020) * 100))
    #
    # cat(progress_text)
    #print(i)

  }


  #tail(result_hours)


  result_hours_final <- result_hours[,1:11]

  result_hours_final <- na.omit(result_hours_final)
  #tail(result_hours_final)





  ###now save the meteo#####

  setwd(output.folder)
  result_hours_final_df <- as.data.frame(result_hours_final)

  #save(result_hours_final_df, file = "fire_weather_study_area.RData")

  write.csv(result_hours_final_df,"fire_weather_study_area.csv",row.names = FALSE)





}
