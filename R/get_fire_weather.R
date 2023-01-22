#' Downloads ERA5-Land reanalysis data and stores it in a csv file
#' @description Downloads ERA5-Land reanalysis data and stores it in a csv file
#' @param study.area Shapefile with the limits of the study area (polygon). Must not contain more than one polygon.
#' @param my.fires Polygon shapefile containing the dated fire perimeters. The shapefile must contain a field with a unique id per fire perimeter (ID), a field with the burned area per perimeter in hectares (Area_ha), a field with the date of start of the fire (Date_ini) and the end of the fire (Date_end). The fields Date_ini and Date_end must follow the format yyyy-mm-dd.
#' @param output.folder Path to the folder where the outputs should be saved.
#' @param utc.zone Numeric. Represents the UTC time zone in the study area. Should be set to the time zone during the fire season (i.e. the user should consider the daylight saving time). E.g. for Portugal, the utc.zone should be set to +1.
#' @param wf_user Personal UID that identifies the user in the Climate Change Service (CDS). Can be retrieved from the user profile page after logging in at https://cds.climate.copernicus.eu/user
#' @param wf_key Personal API Key that identifies the user in the Climate Change Service (CDS). Can be retrieved from the user profile page after logging in at https://cds.climate.copernicus.eu/user
#'
#' @return Returns a raw netdcf file with all the hourly meteorological variables of temperature, relative humidity, wind speed and direction (same as simply using the wf_request function from the ecmwfr package; the request is stored at https://cds.climate.copernicus.eu/cdsapp#!/yourrequests). Also returns a csv file with the hourly meteorological data per fire perimeter in the study area (fire_weather_study_area.csv)
#' @export
#' @import lubridate sp tidyverse ecmwfr ncdf4 udunits2 sf rgdal raster rgeos stringr zoo lwgeom tibble abind foreach doParallel parallel
#' @examples
#' \dontrun{get_fire_weather(study.area="C:/user/study_area.shp", my.fires="C:/user/my_fires.shp",
#' utc.zone=+1,wf_user="12345",wf_key="123456ab-12a3-1234-1234-123ab4567891",
#' output.folder="C:/user/results")}
#'
get_fire_weather <- function(study.area, my.fires,output.folder,utc.zone,wf_user,wf_key) {




  my_fires <- readOGR(my.fires)

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

  str(my_fires_t_inside_study_area)

  my_fires_t_dated <- subset(my_fires_t_inside_study_area, Data_ini!="NaN")
  my_fires_t_dated <- subset(my_fires_t_dated, Data_end!="NaN")


  length(my_fires_t_dated)

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

    if (utc.zone<0) {
      last_day <- nrow(results_pt)
      last_day_use <- as.character(as.POSIXlt(results_pt$days_fire[last_day]) + 86400) #day
      results_pt<- rbind(results_pt,last_day_use)
    }
    if (utc.zone>0) {
      last_day <- nrow(results_pt)
      last_day_use <- as.character(as.POSIXlt(results_pt$days_fire[last_day]) - 86400) #day
      results_pt<-rbind(results_pt,last_day_use)
    }

    results <- rbind(results,results_pt)
  }

  days_fire <- results


  #remove duplicated dates
  days_fire_final <- days_fire[!duplicated(days_fire), ]


  hours <- c("00:00","01:00","02:00","03:00","04:00","05:00",
             "06:00","07:00","08:00","09:00","10:00","11:00",
             "12:00","13:00","14:00","15:00","16:00","17:00",
             "18:00","19:00","20:00","21:00","22:00","23:00")


  t <- extent(study_area) #study area needs to be in WGS84
  class(t)

  extent_studyarea <- as.numeric(c((t@ymin-0.5),(t@xmin-0.5),(t@ymax+0.5),(t@xmax+0.5)))
  extent_studyarea <- round(extent_studyarea,0)

  extent_studyarea_use <- paste(extent_studyarea[1],extent_studyarea[2],extent_studyarea[3],extent_studyarea[4],sep="/")

  extent_studyarea <- as.numeric(round(c((t@ymin-0.5),(t@xmin-0.5),(t@ymax+0.5),(t@xmax+0.5))),0)

  test_dates <- expand.grid(days_fire_final,hours)

  test_dates <- test_dates[order(test_dates$Var1),]


  test_dates$Var3 <- paste(test_dates$Var1,test_dates$Var2,sep=" ")

  strptime(test_dates$Var3, format="%Y-%m-%d  %H:%M")

  test_dates$Var3 <- as.POSIXlt(test_dates$Var3)

  head(test_dates)

  str(test_dates)
  test_dates$Var4 <- test_dates$Var3 - (3600 * (-utc.zone))

  as.character(test_dates$Var4)

  days_fire_final <- gsub(" .*","",test_dates$Var4)

  days_fire_final <- days_fire_final[!duplicated(days_fire_final)]

  n_times_required <- ceiling(length(days_fire_final)/10)

  wf_set_key(user=wf_user, key=wf_key, service="cds")


  setwd(output.folder)

  list_requests <- list()

  for (w in 1:n_times_required){
    w_loop<-w

    if (w ==1) {
      w_loop_ini <- 1
    } else {
      w_loop_ini <- w_loop_end + 1
    }


    w_loop_end <- w_loop_ini + 9

    days_fire_final_loop <- days_fire_final[w_loop_ini:w_loop_end]
    days_fire_final_loop <- days_fire_final_loop[!is.na(days_fire_final_loop)]


    request <- list("dataset_short_name" = "reanalysis-era5-land",
                    "product_type"="reanalysis",
                    "variable"=c("2m_temperature","2m_dewpoint_temperature","10m_u_component_of_wind","10m_v_component_of_wind"),
                    "date" = days_fire_final_loop,
                    "time"=hours,
                    "area" = extent_studyarea_use,
                    "format" = "netcdf",
                    "target" = paste("era5_weather_study_area_",w,".nc",sep=""))

    list_requests[[w]]<-request


  }




  options(timeout = max(500, getOption("timeout")))
  cores<-detectCores()
  cl <- makeCluster(cores[1])
  registerDoParallel(cl)

  foreach(w=1:length(list_requests)) %dopar% {
    data <- ecmwfr::wf_request(list_requests[[w]], user = wf_user,transfer = T, path=".",time_out=7200)
  }





  setwd(output.folder)

  my_nc_files <- list.files(pattern="*.nc")



  for(z in 1:length(my_nc_files)){

    nc_open_loop <- paste("era5_weather_study_area_",z,".nc",sep="")

    my_data_temp <- nc_open(nc_open_loop)

    if (z == 1){

      lon <- ncvar_get(my_data_temp,"longitude")
      lat <- ncvar_get(my_data_temp,"latitude")

      lat <- round(lat,1)
      lon <- round(lon,1)

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

    } else {

      t <- ncvar_get(my_data_temp, "time")

      timestamp_use_z <- as_datetime(c(t*60*60),origin="1900-01-01")
      t2m.array_z <- ncvar_get(my_data_temp, "t2m")
      d2m.array_z <- ncvar_get(my_data_temp, "d2m")
      u10.array_z <- ncvar_get(my_data_temp, "u10")
      v10.array_z <- ncvar_get(my_data_temp, "v10")



      fillvalue_t2m_z <-ncatt_get(my_data_temp,"t2m","_FillValue")
      fillvalue_d2m_z <-ncatt_get(my_data_temp,"d2m","_FillValue")
      fillvalue_u10_z <-ncatt_get(my_data_temp,"u10","_FillValue")
      fillvalue_v10_z <-ncatt_get(my_data_temp,"v10","_FillValue")

      t2m.array_z[t2m.array_z==fillvalue_t2m_z$value] <- NA
      t2m.array_z[d2m.array_z==fillvalue_d2m_z$value] <- NA
      t2m.array_z[u10.array_z==fillvalue_u10_z$value] <- NA
      t2m.array_z[v10.array_z==fillvalue_v10_z$value] <- NA



      nc_close(my_data_temp)

      t2m.array = abind(t2m.array, t2m.array_z)
      d2m.array = abind(d2m.array, d2m.array_z)
      u10.array = abind(u10.array, u10.array_z)
      v10.array = abind(v10.array, v10.array_z)

      timestamp_use <- c(timestamp_use,timestamp_use_z)

    }
  }



  t2m.array <- t2m.array - 273.15
  d2m.array <- d2m.array - 273.15



  RH.array <- 100*(+exp((17.625*d2m.array)/(243.04+d2m.array))/exp((17.625*t2m.array)/(243.04+t2m.array)))



  WD.array <- 180+(180/pi)*atan2(u10.array,v10.array)
  WS.array <- sqrt ((u10.array)^2+(v10.array)^2) #WS is given in m/s-1

  WS.array <- WS.array*3.6 #convert to km/h




  #coordinates

  lat_df <- as.data.frame (lat)
  lon_df <- as.data.frame (lon)

  coordenadas <- expand.grid(lat,lon)

  colnames(coordenadas)<- c("lati","long")

  coordinates(coordenadas) <- ~long+lati
  proj4string(coordenadas) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  coordenadas <- st_as_sf(coordenadas,coords = 1:2)


  days_fire_final_sorted <- sort(days_fire_final)


  #get meteo
  my_fires_t_dated_df <- as.data.frame(my_fires_t_dated)

  results <- matrix(ncol=7, nrow= (length(my_fires_t_dated)+10000))
  result_hours <- matrix(ncol=12, nrow= (length(my_fires_t_dated)+15000*24))

  result_hours_test <- result_hours

  newMatrix <- rbind(result_hours_test, matrix(data=NA, ncol=12, nrow=150000))

  result_hours<- newMatrix


  my_fires_t_dated_2020<- my_fires_t_dated

  my_fires_t_dated_2020$date_diff_t <- ifelse (my_fires_t_dated_2020$date_diff==0,1, my_fires_t_dated_2020$date_diff)

  my_fires_t_dated_2020$Area_ha<-as.numeric(my_fires_t_dated_2020$Area_ha)
  my_fires_t_dated_2020$date_diff_t<-as.numeric(my_fires_t_dated_2020$date_diff_t)

  my_fires_t_dated_2020 <- subset(my_fires_t_dated_2020,!(date_diff_t > 10 & Area_ha < 500))


  my_fires_t_dated_2020_separate <- as.data.frame(str_split_fixed(my_fires_t_dated_2020$Data_ini, "-", 3))


  my_fires_t_dated_2020_separate$V2<-as.numeric(my_fires_t_dated_2020_separate$V2)


  timestamp_use_loop <- format(as.POSIXct(timestamp_use,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')



  for (i in 1:nrow(my_fires_t_dated_2020)){

    my_id_use <- my_fires_t_dated_2020$ID[i]
    my_fires_t_dated_loop <-  subset(my_fires_t_dated_2020, my_fires_t_dated_2020$ID == my_id_use)


    days_fire <- itemizeDates(startDate=my_fires_t_dated_loop$Data_ini, endDate=my_fires_t_dated_loop$Data_end)

    results_pt <- as.data.frame(days_fire)

    if (utc.zone<0) {
      last_day <- nrow(results_pt)
      last_day_use <- as.character(as.POSIXlt(results_pt$days_fire[last_day]) + 86400)
      results_pt<- rbind(results_pt,last_day_use)
    }
    if (utc.zone>0) {
      last_day <- nrow(results_pt)
      last_day_use <- as.character(as.POSIXlt(results_pt$days_fire[last_day]) - 86400)
      results_pt<-rbind(results_pt,last_day_use)
    }


    days_fire <- as.character(results_pt$days_fire)




    class(timestamp_use_loop)



    results_j <- data.frame()
    for(j in 1:length(days_fire)){



      g <- which(timestamp_use_loop==days_fire[j])
      g <- as.data.frame(g)

      results_j <- rbind(results_j,g)

    }

    if (nrow(results_j)>0) {

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


      days_fire_vs2 <- rep(days_fire, each=24)




      seq_vs2 <- sprintf("%02d", 0:23)
      seq_vs3 <- rep(seq_vs2,length(days_fire))

      files_to_get <- paste(days_fire_vs2,seq_vs3,sep="-")



      coordenadas_st <- st_as_sf(coordenadas)



      my_fires_t_dated_loop_st <- st_as_sf(my_fires_t_dated_loop)



      out <- suppressMessages(st_intersection(coordenadas_st, my_fires_t_dated_loop_st))



      #If the fire perimeter has at least one era5-land point inside

      rm(seal_coords)
      rm(sacar_col)
      rm(sacar_linha)

      if (dim(out)[1] != 0) {


        seal_coords <- do.call(rbind, st_geometry(out)) %>%
          as_tibble() %>% setNames(c("lon","lat"))

        seal_coords <- as.data.frame(seal_coords)

        seal_coords$lat <- round(seal_coords$lat,1)
        seal_coords$lon <- round(seal_coords$lon,1)



        sacar_linha <- which(lon %in% seal_coords$lon,arr.ind = TRUE)


        sacar_col <- which(lat %in% seal_coords$lat,arr.ind = TRUE)



        sacar_linha_fin <- as.numeric(unique(sacar_linha))
        sacar_col_fin <- as.numeric(unique(sacar_col))


        seal_coords$posicao_row<-NA
        seal_coords$posicao_col<-NA



        seal_coords$lat <- round(seal_coords$lat,1)
        seal_coords$lon <- round(seal_coords$lon,1)

        for(j in 1:nrow(seal_coords)){
          sacar_col_loop <- which(lat == seal_coords$lat[j],arr.ind = TRUE)

          sacar_linha_loop <- which(lon == seal_coords$lon[j],arr.ind = TRUE)

          seal_coords[j,3] <- as.numeric(sacar_linha_loop[1])
          seal_coords[j,4] <- as.numeric(sacar_col_loop[1])
        }




        seal_coords <- na.locf(seal_coords)



        NAindex <- which(is.na(results))
        firstNA <- min(NAindex)



        start_results <- firstNA
        end_results <- firstNA+nrow(seal_coords)-1




        results[start_results:end_results,1]<- my_fires_t_dated_loop_st$ID
        results[start_results:end_results,2]<- seal_coords$posicao_row
        results[start_results:end_results,3]<- seal_coords$posicao_col
        results[start_results:end_results,4]<- seal_coords$lon
        results[start_results:end_results,5]<- seal_coords$lat

      }





      #If the fire perimeter does not contain any era5-land point inside, we create a buffer around the fire perimeter
      #and search for the nearest point.


      if (dim(out)[1] == 0) {
        b  <- suppressMessages(sf::st_buffer(my_fires_t_dated_loop, dist=10000))


        coordenadas_st <- st_as_sf(coordenadas)
        b_st <- st_as_sf(b)
        my_fires_t_dated_loop_st <- st_as_sf(my_fires_t_dated_loop)

        b_st <- suppressMessages(st_buffer(b_st,0))



        out <- suppressMessages(st_intersection(coordenadas_st, b_st))


        gDists <- st_distance(my_fires_t_dated_loop_st, out)


        get_min <- out$geometry[which.min(gDists)]


        seal_coords <- do.call(rbind, st_geometry(get_min)) %>%
          as_tibble() %>% setNames(c("lon","lat"))

        seal_coords <- as.data.frame(seal_coords)



        seal_coords$lat <- round(seal_coords$lat,1)
        seal_coords$lon <- round(seal_coords$lon,1)

        sacar_linha <- which(lon %in% seal_coords$lon,arr.ind = TRUE)

        sacar_col <- which(lat %in% seal_coords$lat,arr.ind = TRUE)

        sacar_linha_fin <- as.numeric(unique(sacar_linha))
        sacar_col_fin <- as.numeric(unique(sacar_col))


        seal_coords$posicao_row<-NA
        seal_coords$posicao_col<-NA


        is_all_na <- round(WS.array[sacar_linha_fin,sacar_col_fin,],2)

        if(all(is.na(is_all_na))==TRUE) {

          out$gDists <- as.numeric(gDists)

          gDists_sort <- out[order(gDists),]

          get_min <- gDists_sort[2,]



          seal_coords <- do.call(rbind, st_geometry(get_min)) %>%
            as_tibble() %>% setNames(c("lon","lat"))

          seal_coords <- as.data.frame(seal_coords)
        }



        seal_coords$lat <- round(seal_coords$lat,1)
        seal_coords$lon <- round(seal_coords$lon,1)

        sacar_linha <- which(lon %in% seal_coords$lon,arr.ind = TRUE)


        sacar_col <- which(lat %in% seal_coords$lat,arr.ind = TRUE)


        sacar_linha_fin <- as.numeric(unique(sacar_linha))
        sacar_col_fin <- as.numeric(unique(sacar_col))

        seal_coords$posicao_row<-NA
        seal_coords$posicao_col<-NA


        is_all_na <- round(WS.array[sacar_linha_fin,sacar_col_fin,],2)




        seal_coords$lat <- round(seal_coords$lat,1)
        seal_coords$lon <- round(seal_coords$lon,1)

        for(j in 1:nrow(seal_coords)){
          sacar_col_loop <- which(lat == seal_coords$lat[j],arr.ind = TRUE)

          sacar_linha_loop <- which(lon == seal_coords$lon[j],arr.ind = TRUE)


          seal_coords[j,3] <- as.numeric(sacar_linha_loop[1])
          seal_coords[j,4] <- as.numeric(sacar_col_loop[1])




          NAindex <- which(is.na(results))
          firstNA <- min(NAindex)


          start_results <- firstNA
          end_results <- firstNA+nrow(seal_coords)-1


          results[start_results:end_results,1]<- my_fires_t_dated_loop_st$ID
          results[start_results:end_results,2]<- seal_coords$posicao_row
          results[start_results:end_results,3]<- seal_coords$posicao_col
          results[start_results:end_results,4]<- seal_coords$lon
          results[start_results:end_results,5]<- seal_coords$lat



        }}




      results_df <- as.data.frame(results)


      results_subset_my_points <- as.data.frame(results_df[start_results:end_results,])




      for(l in 1:nrow(results_subset_my_points)){

        results_subset_my_points_loop <- results_subset_my_points[l,]

        colnames(results_subset_my_points_loop)<- c("ID","linha_mat","col_mat","long","lati","linha_mat_fwi","col_mat_fwi")
        colnames(results_subset_my_points)<- c("ID","linha_mat","col_mat","long","lati","linha_mat_fwi","col_mat_fwi")

        results_subset_my_points_loop$linha_mat <- as.numeric(results_subset_my_points_loop$linha_mat)
        results_subset_my_points_loop$col_mat <- as.numeric(results_subset_my_points_loop$col_mat)


        WS.array_loop_data <- round(WS.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)
        WD.array_loop_data <- round(WD.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)
        RH.array_loop_data <- round(RH.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)
        t2m.array_loop_data <- round(t2m.array_loop[results_subset_my_points_loop$linha_mat,results_subset_my_points_loop$col_mat,],2)



        weather_loop <- cbind(results_subset_my_points_loop$ID,WS.array_loop_data,WD.array_loop_data,RH.array_loop_data,t2m.array_loop_data,files_to_get)

        colnames(weather_loop) <- c("ID","WS","WD","RH","T","date")


        NAindex <- which(is.na(result_hours))
        firstNA <- min(NAindex)


        start_results <- firstNA
        end_results <- start_results + nrow(weather_loop)-1


        #Correct the UTC. Note that all Era5-Land data is at UTC+0
        old_date_hours <- as.character(gsub("-","",files_to_get))
        as.Date(old_date_hours,format="%Y%m%d%H")

        new_date_hours_almost <- strptime(old_date_hours, format='%Y%m%d%H')

        new_date_hours <- as.character(new_date_hours_almost - (3600 * (-utc.zone)))


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

      }



      colnames(result_hours)<- c("ID","row","col","lon","lat","day_files","day_UTC_corrected","temperature","RH","WS","WD","FWI")



    }



    result_hours_final <- result_hours[,1:11]

    result_hours_final <- na.omit(result_hours_final)


    #export the meteorology data
    setwd(output.folder)
    result_hours_final_df <- as.data.frame(result_hours_final)


    results_pt <- as.data.frame(matrix())
    colnames(results_pt)<-"days_fire_original"

    for (q in 1:nrow(my_fires_t_dated)) {
      days_fire_original <- itemizeDates(startDate=my_fires_t_dated$Data_ini[q], endDate=my_fires_t_dated$Data_end[q])
      results_pt_1 <- as.data.frame(days_fire_original)
      results_pt <- rbind(results_pt,results_pt_1)}

    results_pt <- subset(results_pt, days_fire_original!="NaN")

    results_pt_hours <- as.character(gsub("-","",results_pt$days_fire_original))

    results_pt_hours <- expand.grid(results_pt_hours,hours)
    results_pt_hours$Var2 <- as.character(gsub(":","",results_pt_hours$Var2))

    results_pt_hours$Var3 <- paste(results_pt_hours$Var1,results_pt_hours$Var2,sep="")

    results_pt_hours$Var3 <- substr(results_pt_hours$Var3,1,10)

    result_hours_final_df <- result_hours_final_df[result_hours_final_df$day_UTC_corrected %in% results_pt_hours$Var3,]

    write.csv(result_hours_final_df,"fire_weather_study_area.csv",row.names = FALSE)



  }


}
