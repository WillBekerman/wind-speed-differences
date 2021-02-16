library(purrr)
library(R.utils)

# Only for .nc.gz files

list_of_files <- list.files(path = "path/to/your/folder", # make new folder in Documents
                            pattern = "\\.gz$",
                            full.names = TRUE)

list_of_files %>% 
  walk(gunzip)

# Extract file/(s) in new folder to Documents

library(ncdf4) # package for netcdf manipulation
#library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

cygnss_092020_raw <- nc_open('cyg.ddmi.s20200920-000000-e20200920-235959.l2.wind-mss.a30.d31.nc')
cygnss_092120_raw <- nc_open('cyg.ddmi.s20200921-000000-e20200921-235959.l2.wind-mss.a30.d31.nc')
cygnss_092220_raw <- nc_open('cyg.ddmi.s20200922-000000-e20200922-235959.l2.wind-mss.a30.d31.nc')
cygnss_092320_raw <- nc_open('cyg.ddmi.s20200923-000000-e20200923-235959.l2.wind-mss.a30.d31.nc')
cygnss_092420_raw <- nc_open('cyg.ddmi.s20200924-000000-e20200924-235959.l2.wind-mss.a30.d31.nc')
cygnss_092520_raw <- nc_open('cyg.ddmi.s20200925-000000-e20200925-235959.l2.wind-mss.a30.d31.nc')
cygnss_092620_raw <- nc_open('cyg.ddmi.s20200926-000000-e20200926-235959.l2.wind-mss.a30.d31.nc')

# print(<name_of_dataset>) to see variables

# important variables: spacecraft_id, lat, lon, sc_lat, sc_lon, wind_speed, 
# wind_speed_uncertainty, sample_time

dats = c("cygnss_092020", "cygnss_092120", "cygnss_092220", "cygnss_092320", "cygnss_092420", "cygnss_092520", "cygnss_092620")
vars = c("spacecraft_id", "lat", "lon", "sc_lat", "sc_lon", "wind_speed", "wind_speed_uncertainty", "sample_time")

fillvalues = list()
for (att in vars){
  fillvalues[[att]] = ncatt_get(cygnss_092020_raw, att, "_FillValue")
}

for (dat in dats){
  dataset = get(paste(dat, "_raw", sep=""))
  varlist = list()
  for (var in vars) {
    varlist[[var]] = ncvar_get(dataset, var)
    varlist[[var]][varlist[[var]] == fillvalues[[var]]$value] <- NA
  }
  assign(paste(dat, "_df", sep=""), t(data.frame(matrix(unlist(varlist), nrow=length(varlist), byrow=TRUE),stringsAsFactors=FALSE)))
}

# change sample_times to seconds since since 2020-09-20 00:00:00.0000 
cygnss_092020_df[,length(vars)] = cygnss_092020_df[,length(vars)] + 0.5
cygnss_092120_df[,length(vars)] = cygnss_092120_df[,length(vars)] + 0.5 + 1*86400
cygnss_092220_df[,length(vars)] = cygnss_092220_df[,length(vars)] + 0.5 + 2*86400
cygnss_092320_df[,length(vars)] = cygnss_092320_df[,length(vars)] + 0.5 + 3*86400
cygnss_092420_df[,length(vars)] = cygnss_092420_df[,length(vars)] + 0.5 + 4*86400
cygnss_092520_df[,length(vars)] = cygnss_092520_df[,length(vars)] + 0.5 + 5*86400
cygnss_092620_df[,length(vars)] = cygnss_092620_df[,length(vars)] + 0.5 + 6*86400

# assign variable names to df column names
colnames(cygnss_092020_df) <- vars
colnames(cygnss_092120_df) <- vars
colnames(cygnss_092220_df) <- vars
colnames(cygnss_092320_df) <- vars
colnames(cygnss_092420_df) <- vars
colnames(cygnss_092520_df) <- vars
colnames(cygnss_092620_df) <- vars

# omit rows w/ NAs
cygnss_092020_df = na.omit(cygnss_092020_df)
cygnss_092120_df = na.omit(cygnss_092120_df)
cygnss_092220_df = na.omit(cygnss_092220_df)
cygnss_092320_df = na.omit(cygnss_092320_df)
cygnss_092420_df = na.omit(cygnss_092420_df)
cygnss_092520_df = na.omit(cygnss_092520_df)
cygnss_092620_df = na.omit(cygnss_092620_df)


# Show quilt plots
library(fields)
library(maps)

cygnss_dat = do.call("rbind", list(cygnss_092020_df, cygnss_092120_df, cygnss_092220_df, cygnss_092320_df, 
                            cygnss_092420_df, cygnss_092520_df, cygnss_092620_df))

rm(cygnss_092020_df, cygnss_092020_raw, cygnss_092120_df, cygnss_092120_raw, cygnss_092220_df, cygnss_092220_raw,
   cygnss_092320_df, cygnss_092320_raw, cygnss_092420_df, cygnss_092420_raw, cygnss_092520_df, cygnss_092520_raw,
   cygnss_092620_df, cygnss_092620_raw, dataset, fillvalues, varlist)
gc()

quilt.plot(cygnss_dat[,3], cygnss_dat[,2], cygnss_dat[,6])
map("world2", add=T)

# map("usa")
# map("state", add = T, col="grey")

quilt.plot(cygnss_dat[1:10000,3], cygnss_dat[1:10000,2], cygnss_dat[1:10000,6], nx=150, ny=150)
map("world2", add=T)

quilt.plot(cygnss_dat[,3], cygnss_dat[,2], cygnss_dat[,6], nx=150, ny=150)
map("world2", add=T)






# gdr_ssha and gdr_s both have time as dimension
  # gdr_ssha has "wind_speed_alt" "wind_speed_alt_mle3" (altimeter, altimeter w/ MLE3 retracking)
  # gdr_s has "wind_speed_model_u" "wind_speed_model_v" "wind_speed_alt" "wind_speed_alt_mle3"               
  #   and "wind_speed_rad" (radiometer wind speed)


# cygnss uses wind speed using Minimum Variance estimator applied to Fully Developed 
# Seas retrievals from NBRCS and LES
# derived from both the NBRCS and the LES observables using the fully developed 
# seas geophysical model function

# nbrcs: normalized bistatic radar cross section
# les: leading edge slope



jason_092020_raw <- nc_open('jason_092020.nc')
jason_092120_raw <- nc_open('jason_092120.nc')
jason_092220_raw <- nc_open('jason_092220.nc')
jason_092320_raw <- nc_open('jason_092320.nc')
jason_092420_raw <- nc_open('jason_092420.nc')
jason_092520_raw <- nc_open('jason_092520.nc')
jason_092620_raw <- nc_open('jason_092620.nc')

# print(<name_of_dataset>) to see variables

# important variables: spacecraft_id, lat, lon, sc_lat, sc_lon, wind_speed, 
# wind_speed_uncertainty, sample_time

dats = c("jason_092020", "jason_092120", "jason_092220", "jason_092320", "jason_092420", "jason_092520", "jason_092620")
vars = c("lat", "lon", "surface_type", "alt", "bathymetry", "wind_speed_alt", "wind_speed_alt_mle3", "time")

fillvalues = list()
for (att in vars){
  fillvalues[[att]] = ncatt_get(jason_092020_raw, att, "_FillValue")
}

for (dat in dats){
  dataset = get(paste(dat, "_raw", sep=""))
  varlist = list()
  for (var in vars) {
    varlist[[var]] = ncvar_get(dataset, var)
    varlist[[var]][varlist[[var]] == fillvalues[[var]]$value] <- NA
  }
  assign(paste(dat, "_df", sep=""), t(data.frame(matrix(unlist(varlist), nrow=length(varlist), byrow=TRUE),stringsAsFactors=FALSE)))
}

# change sample_times to seconds since since 2020-09-20 00:00:00.0000 
jason_092020_df[,length(vars)] = jason_092020_df[,length(vars)] - (56*60 + 1)
jason_092120_df[,length(vars)] = jason_092120_df[,length(vars)] + (21*60 + 47) + 1*86400
jason_092220_df[,length(vars)] = jason_092220_df[,length(vars)] + (43*60 + 21.9) + 2*86400
jason_092320_df[,length(vars)] = jason_092320_df[,length(vars)] + (8*60 + 43.8) + 3*86400
jason_092420_df[,length(vars)] = jason_092420_df[,length(vars)] + (30*60 + 18.6) + 4*86400
jason_092520_df[,length(vars)] = jason_092520_df[,length(vars)] + (51*60 + 53.5) + 5*86400
jason_092620_df[,length(vars)] = jason_092620_df[,length(vars)] + (17*60 + 15.6) + 6*86400

# assign variable names to df column names
colnames(jason_092020_df) <- vars
colnames(jason_092120_df) <- vars
colnames(jason_092220_df) <- vars
colnames(jason_092320_df) <- vars
colnames(jason_092420_df) <- vars
colnames(jason_092520_df) <- vars
colnames(jason_092620_df) <- vars

# omit rows w/ NAs
jason_092020_df = na.omit(jason_092020_df)
jason_092120_df = na.omit(jason_092120_df)
jason_092220_df = na.omit(jason_092220_df)
jason_092320_df = na.omit(jason_092320_df)
jason_092420_df = na.omit(jason_092420_df)
jason_092520_df = na.omit(jason_092520_df)
jason_092620_df = na.omit(jason_092620_df)


# Show quilt plots
library(fields)
library(maps)

jason_dat = do.call("rbind", list(jason_092020_df, jason_092120_df, jason_092220_df, jason_092320_df, 
                                   jason_092420_df, jason_092520_df, jason_092620_df))

rm(jason_092020_df, jason_092020_raw, jason_092120_df, jason_092120_raw, jason_092220_df, jason_092220_raw,
   jason_092320_df, jason_092320_raw, jason_092420_df, jason_092420_raw, jason_092520_df, jason_092520_raw,
   jason_092620_df, jason_092620_raw, dataset, fillvalues, varlist)
gc()

# surface_type: 0 = open oceans or semi-enclosed seas; 1 = enclosed seas or lakes; 2 = continental ice; 3 = land

quilt.plot(jason_dat[,2][which(jason_dat[,3] == 0)], jason_dat[,1][which(jason_dat[,3] == 0)], jason_dat[,6][which(jason_dat[,3] == 0)])
map("world2", add=T)

# map("usa")
# map("state", add = T, col="grey")

quilt.plot(jason_dat[1:10000,2][which(jason_dat[1:10000,3] == 0)], jason_dat[1:10000,1][which(jason_dat[1:10000,3] == 0)], jason_dat[1:10000,6][which(jason_dat[1:10000,3] == 0)], nx=150, ny=150)
map("world2", add=T)



#### Sanity check: similar times for CYGNSS and Jason satellites
summary(jason_dat[,ncol(jason_dat)])
summary(cygnss_dat[,ncol(cygnss_dat)])



#### Plots of wind speed using similar lat, long
summary(cygnss_dat[,3]) # lon
summary(jason_dat[,2]) # lon

summary(cygnss_dat[,2]) # lat
summary(jason_dat[,1]) # lat


par(mfrow=c(2,1))
quilt.plot(cygnss_dat[,3], cygnss_dat[,2], cygnss_dat[,6], main = 'CYGNSS (9/20 - 9/26)')
map("world2", add=T)

reduced_jason_dat = jason_dat[which(jason_dat[,1] > -38),]
reduced_jason_dat = reduced_jason_dat[which(reduced_jason_dat[,1] < 38),]

quilt.plot(reduced_jason_dat[,2][which(reduced_jason_dat[,3] == 0)], reduced_jason_dat[,1][which(reduced_jason_dat[,3] == 0)], reduced_jason_dat[,6][which(reduced_jason_dat[,3] == 0)], main = 'Jason (9/20 - 9/26)')
map("world2", add=T)




#### Plots of orbits of CYGNSS satellites over short time
# cygnss_dat[,1][cygnss_dat[,1] == 0] = 'end to end simulator'
# cygnss_dat[,1][cygnss_dat[,1] == 14] = 'engineering model'
# cygnss_dat[,1][cygnss_dat[,1] == 15] = 'default'
# cygnss_dat[,1][cygnss_dat[,1] == 255] = 'unknown'

spacecrafts = c('CYGNSS 1', 'CYGNSS 2', 'CYGNSS 3', 'CYGNSS 4', 'CYGNSS 5', 'CYGNSS 6',
                'CYGNSS 7', 'CYGNSS 8')#, 'end to end simulator', 'engineering model',
                #'default', 'unknown'
                
cygnss_dat[,1][cygnss_dat[,1] == 247] = 1
cygnss_dat[,1][cygnss_dat[,1] == 249] = 2
cygnss_dat[,1][cygnss_dat[,1] == 43] = 3
cygnss_dat[,1][cygnss_dat[,1] == 44] = 4
cygnss_dat[,1][cygnss_dat[,1] == 47] = 5
cygnss_dat[,1][cygnss_dat[,1] == 54] = 6
cygnss_dat[,1][cygnss_dat[,1] == 55] = 7
cygnss_dat[,1][cygnss_dat[,1] == 73] = 8                


plot(cygnss_dat[1:10000,3], cygnss_dat[1:10000,2], main = 'CYGNSS Satellite Orbits', xlab = 'Longitude', ylab = 'Latitude', col = cygnss_dat[1:10000,1])
legend("bottomright", spacecrafts[unique(cygnss_dat[1:10000,1])],col=unique(cygnss_dat[1:10000,1]), pch=16, cex=0.65, pt.cex = 1)
map("world2", add=T)

plot(cygnss_dat[1:30000,3], cygnss_dat[1:30000,2], main = 'CYGNSS Satellite Orbits', xlab = 'Longitude', ylab = 'Latitude', col = cygnss_dat[1:30000,1])
legend("bottomright", spacecrafts[unique(cygnss_dat[1:30000,1])],col=unique(cygnss_dat[1:30000,1]), pch=16, cex=0.65, pt.cex = 1)
map("world2", add=T) #USED THIS

plot(cygnss_dat[1:50000,3], cygnss_dat[1:50000,2], main = 'CYGNSS Satellite Orbits', xlab = 'Longitude', ylab = 'Latitude', col = cygnss_dat[1:50000,1])
legend("bottomright", spacecrafts[unique(cygnss_dat[1:50000,1])],col=unique(cygnss_dat[1:50000,1]), pch=16, cex=0.65, pt.cex = 1)
map("world2", add=T)






#### Distances stuff
library(geosphere)
distVincentyEllipsoid(cygnss_dat[1:10, c(3,2)])



library(Imap)
#gdist(lon.1, lat.1, lon.2, lat.2, units = "m")
gdist(cygnss_dat[1,3], cygnss_dat[1,2], jason_dat[1,2], jason_dat[1,1], units = "m")


# for CYGNSS 1, find distance to Jason for 
cygnss_dat[which(cygnss_dat[,1] == 1),]


# ### note, we use sc_lat and sc_lon for CYGNSS

# for each jason observation (let's only use first 10,000), find cygnss observations recorded within 0.25 seconds 
# of time recorded for jason.
# compare these latitudes and longitudes

# n=10
# cygnss_obs = 700
# obslist = list()
# 
# for (jason_obs in 1:n){
#   jason_time = jason_dat[jason_obs, ncol(jason_dat)]
#   min_cygnss_time = jason_time - 0.25
#   
#   # find at which obs. index is cygnss time greater than or equal to min_cygnss_time
#   if (jason_obs != 1) cygnss_obs = obslist$cygnss[length(obslist$cygnss)]
#   found = F
#   while (cygnss_obs < length(cygnss_dat[,1]) && !found) {
#     cygnss_obs = cygnss_obs + 1
#     if (cygnss_dat[cygnss_obs, ncol(cygnss_dat)] - min_cygnss_time >= 0) found = T
#   }
#   
#   obslist$jason = c(obslist$jason, jason_obs)
#   obslist$cygnss = c(obslist$cygnss, cygnss_obs)
#     
# }


# get first index in cygnss where time - jason is positive
cyg_obs = 760
cyg_vec = numeric()
n = nrow(jason_dat)

for (j_obs in 1:n){
  while (cygnss_dat[cyg_obs,ncol(cygnss_dat)] - jason_dat[j_obs,ncol(jason_dat)] < 0){
    cyg_obs = cyg_obs + 1
  }
  cyg_vec = c(cyg_vec, cyg_obs)
}

obslist2 = list()
obslist2$jason = 1:n
obslist2$cygnss = cyg_vec - 1


summary(jason_dat[obslist2$jason,ncol(jason_dat)] - cygnss_dat[obslist2$cygnss,ncol(cygnss_dat)])


dists = numeric()
for (ix in obslist2$jason){
  dist = gdist(cygnss_dat[obslist2$cygnss[ix],3], cygnss_dat[obslist2$cygnss[ix],2], jason_dat[ix,2], jason_dat[ix,1], units = "m")
  dists = c(dists, dist)
}

plot()


#probs: 
jason_dat[which(is.na(dists))[1:5],]
cygnss_dat[obslist2$cygnss[which(is.na(dists))][1:5],]



### Drop Jason observations not in 9/20 - 9/26
jason_dat = jason_dat[-which(jason_dat[,ncol(jason_dat)] < 0),]
jason_dat = jason_dat[-which(jason_dat[,ncol(jason_dat)] > 86400*7),]







# CYGNSS 1: for each CYGNSS1 obs, find Jason obs closest in time and find distance
cygnss1_dat = cygnss_dat[which(cygnss_dat[,1] == 1),]

j_obs = 1
j_vec = numeric()
n = nrow(cygnss_dat)

for (cyg_obs in 1:n){
  while (cygnss1_dat[cyg_obs,ncol(cygnss1_dat)] - jason_dat[j_obs,ncol(jason_dat)] < 0){
    j_obs = j_obs + 1
  }
  j_vec = c(j_vec, j_obs)
}

obslist = list()
obslist$jason = 1:n
obslist$cygnss = cyg_vec



dists1 = numeric()
for (ix in 1:n){
  dist = gdist(cygnss1_dat[obslist2$cygnss[ix],3], cygnss1_dat[obslist2$cygnss[ix],2], jason_dat[ix,2], jason_dat[ix,1], units = "m")
  dists1 = c(dists, dist)
}





