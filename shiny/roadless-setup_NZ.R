###
## May 2017
## Adapted from http://nhorton.people.amherst.edu/roadless-setup.R
###

# desired samples for student
nsamp <- 50

# automatically grab 1000 pts because many will be over water
samples <- rgeo(n=1000, latlim=c(-32.5, -47.5), lonlim=c(165, 180))

# get world spatial data
data(wrld_simpl)

# reduce to samples on land
pts <- SpatialPoints(samples[, c('lon', 'lat')], proj4string=CRS(proj4string(wrld_simpl)))
onland <- which(over(pts, wrld_simpl)$FIPS == 'NZ')
samples <- samples[onland, ]

# reduce to desired size for students
samples <- sample_n(samples, nsamp)

# assemble myroadless df
myroadless <- data.frame(Latitude=round(samples$lat, 4),
                         Longitude=round(samples$lon, 4),
                         OnLand = NA, 
                         Within1Mile = NA)

rm(samples)
myroadless
