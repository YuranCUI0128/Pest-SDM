library(raster)

library(ncdf4)

library(sp)
library(fields)

library(biomod2)
library(climdex)


library(MuMIn)
x <- rnorm(100, mean=0, sd=1)
z <- rnorm(100, mean=10, sd=20)
y <- 3*x + z + rnorm(100, sd=.25)
model <- lm(y ~ x + z)
coef(model)
par(mfrow=c(1,2))
plot(y ~ x)
plot(y ~ z)
s.x <- as.numeric(scale(x))
s.z <- as.numeric(scale(z))
s.model <- lm(y ~ s.x + s.z)
coef(s.model)

a <- rnorm(100); b <- rnorm(100); c <- rnorm(100); d <- rnorm(100)
y <- a - 0.5*b + 2.7 + rnorm(100)
top.model <- lm(y ~ (a + b + c + d)^2, na.action=na.pass) # what does ^2 do?...
models <- dredge(top.model)

# 读取NC文件
nc_file <- ("E:/R-4.2.1/data_p/diff_pr_Amon_modmean_rcp45_000_2040-2060_minus_1960-1990_mon1_ave1_withsd.nc")
nc_data <- brick(nc_file)
time(nc_data)
brick_1 <- subset(nc_data, time = 1)
# 将NC数据转换为Raster Stack对象
r_stack <- stack(nc_data)
# 将NC数据转换为RasterLayer对象
r_layer <- brick(nc_data[[1]])
print(r_stack)
plot(r_stack)
print(nc_data)

nc.pre <- nc_open("E:/R-4.2.1/data_p/diff_pr_Amon_modmean_rcp45_000_2040-2060_minus_1960-1990_mon1_ave1_withsd.nc")
print(nc.pre)
pre <- brick("E:/R-4.2.1/data_p/diff_pr_Amon_modmean_rcp45_000_2040-2060_minus_1960-1990_mon1_ave1_withsd.nc", varname="diff")
data.crop <- crop(pre,model_extent_Europe)
plot(data.crop)

map <- readPicture("E:/R-4.2.1/data_p/diff_pr_Amon_onemean_rcp45_000_2040-2060_minus_1960-1990_mon6_ave1_withsd_mean_Europe.eps")
displayPicture(map)



# 读取.dat文件
dtr <- read.table("E:/R-4.2.1/grid_10/grid/grid_10min_dtr.dat", header = FALSE)
colnames(dtr)<-c("latitude", "longitude", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
coords <- cbind(dtr$longitude, dtr$latitude)
SPDF <- SpatialPointsDataFrame(coords, dtr, proj4string=CRS("+proj=longlat +datum=WGS84"))
myraster <- raster(extent(SPDF), res = 0.17, crs = projection(SPDF))
# Rasterize the point data using the Jan variable
dtrjan <- rasterize(SPDF, myraster, field = "Jan", fun = mean)
dtrjan_Eur<- crop(myraster,model_extent_Europe)
plot(dtrjan_Eur)





data <- brick(thing)
#crop to UK
UK <- extent(-15,3.25,49.5,59.75)
#UK
data.crop <- crop(data,UK)
#extract temp for all images
id <- aegeria$image_ID
lat <- as.numeric(as.character(aegeria$decimalLatitude))
lon <- as.numeric(as.character(aegeria$decimalLongitude))
image_data <- cbind(lon, lat)
rownames(image_data) <- id
coords <- na.omit(image_data)
# Extract climate data from the RasterBrick as a data.frame
image_extract <- raster::extract(data.crop, coords)
# Add sample site names
row.names(image_extract) <- row.names(coords)
#colnames
years <- 1901:2019
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names <- paste(rep(years, each=12), rep(month, times=116), sep="_")
image_extract2 <- as.data.frame(image_extract)
colnames(image_extract2) <- names

# Create the raster layer from the interpolated data
myraster <- raster(t(myinterp$z), xmn = xrange[1], xmx = xrange[2], ymn = yrange[1], ymx = yrange[2], crs = CRS("+proj=longlat +datum=WGS84"))


# 将数据框转换为SpatialPointsDataFrame对象
coordinates(dtr) <- c("longitude", "latitude")
proj4string(dtr) <- CRS("+proj=longlat +datum=WGS84")
r <- raster(xmn=min(dtr$longitude), xmx=max(dtr$longitude), ymn=min(dtr$latitude), ymx=max(dtr$latitude), nrow=753, ncol=753)
# 将SpatialPointsDataFrame对象转换为RasterLayer对象
r <- rasterize(dtr, r, field=1)
rs <- stack(r)
# 将Raster Stack对象写入TIFF文件
writeRaster(rs, "filename.tif", format="GTiff", overwrite=TRUE)
print(rs)


# Convert to an sf object and set the projection
dtr <- st_as_sf(dtr, coords=c('longtitude', 'latitude'))
st_crs(dtr) <- 4326
plot(dtr)

tmp <- read.table("E:/R-4.2.1/grid_10/grid_10min_tmp.dat", header = TRUE)
# 将数据转换为矩阵
data_mat <- as.matrix(dtr[,1:ncol(dtr)])
# 创建Raster对象
rast <- raster(data_mat)
# 指定坐标系和分辨率（如果需要）
projection(rast) <- "+proj=longlat +datum=WGS84"
plot(rast)
res(rast) <- c(0.5, 0.5)



#Climgen
nc <- raster("E:/R-4.2.1/data_p/tmp_patCMIP3ipsl_cm4_______A2A1B_____dtsA1B_20__20062100ann_monthly_reglandboxes_______.climgen")




# 读取NC文件
pre <- brick("E:/R-4.2.1/data_p/diff_pr_Amon_modmean_rcp45_000_2040-2060_minus_1960-1990_mon1_ave1_withsd.nc", varname = "diff")
data.crop <- crop(pre,model_extent_Europe)
plot(data.crop)

please <- brick("E:/R-4.2.1/data_p/diff_tas_Amon_onemean_rcp45_000_2040-2060_minus_1960-1990_mon1_ave12_withsd.nc", varname= "diff")

plot(please)


future <- brick("E:/R-4.2.1/data_p/diff_pr_Amon_modmean_rcp45_000_2040-2060_minus_1960-1990_mon1_ave1_withsd.nc", varname = "diff")
var_names <- c("diff", "error", "prob")
futuretJan <- stack()
for (var_name in var_names) {
  file_path <- file_paths[1]  # Assuming the same file path for all variables
  brick_data <- brick(file_path, varname = var_name)
  data.crop <- crop(brick_data, model_extent_Europe)
  selected_layers <- paste0("X", 1110:1140)
  cropped_data <- calc(data.crop[[selected_layers]], mean)
  landuse <- stack(landuse, cropped_data)
}
names(futuretJan) <- c("diff", "error", "prob")
plot(future)
#writeRaster(landuse, filename = "E:/R-4.2.1/grid_10/landuse", format = "GTiff")



selected_layers <- paste0("X", 1110:1140)
mean_crop <- calc(data.crop[[selected_layers]], mean)

pre <- stack("E:/R-4.2.1/data_p/wc2.1_10m_prec_ACCESS-CM2_ssp245_2041-2060.tif")
data.crop <- crop(pre,model_extent_Europe)
