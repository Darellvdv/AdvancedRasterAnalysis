# AdvancedRasterAnalysis
# TeamTropical - Darell.vanderVoort & Froede.Vrolijk
# 13th of Janaury 2016

-----------------------------------------------------------------------------------------------------------------------
## 1: produce one or more plots that demonstrate the relationship between the Landsat bands and the VCF tree cover ###

# load libraries
library(raster)

# load files
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")

# Remove values other than trees for VCF
vcfGewata[vcfGewata > 100] <- NA

# Create a brick with all layers
gewata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)

# Show all correlation plots
#pairs(gewata, hist=TRUE)

# show bands with high correlation
#par(mfrow = c(1, 4))
#plot(gewata[[1]], gewata[[6]])
#plot(gewata[[2]], gewata[[6]])
#plot(gewata[[4]], gewata[[6]])
#plot(gewata[[5]], gewata[[6]])
#par(mfrow = c(1, 1)) # reset plot area

------------------------------------------------------------------------------------------------------------------

## 2: create an lm() model and show a summary (e.g. using summary()) of the model object you created. ##
     # Which predictors (bands) are probably most important in predicting tree cover?

# Get a value table with all data (and remove NAs = optional)
valtable <- as.data.frame(getValues(gewata))
# valtable <- na.omit(valtable)

# Create the LM model
lm.1 <- lm(vcf2000Gewata ~ gewataB1 + gewataB2 + gewataB3 + gewataB4 + gewataB5 + gewataB7, data = valtable)

# Give a summary 
summary(lm.1)
# Here we see that band 2 is most important, altough the std. error is highest. The lowest is band 7 and also has a high error.

------------------------------------------------------------------------------------------------------------------

## 3: Plot the predicted tree cover raster and compare with the original VCF raster ##
treemap <- predict(gewata[[1:6]], model = lm.1)

# Remove negative values
treemap[treemap < 0] <- NA

# Plot vcf and predicted map
par(mfrow = c(2, 1))
plot(treemap)
plot(gewata[[7]])

# Extract the values, bind to VCF values and ommit NA values
valTreemap <- as.data.frame(getValues(treemap))
pred.val <- cbind(valTreemap, valtable[,7])
pred.val <- na.omit(pred.val)

------------------------------------------------------------------------------------------------------------------

## 4: Compute the RMSE between your predicted and the actual tree cover values ##

rmse <- sqrt(mean((pred.val[,2]-pred.val[,1])^2))
rmse

------------------------------------------------------------------------------------------------------------------

# 5: are the differences between the predicted and actual tree cover the same for all of the 3 classes we used for the random forest classfication? 
#    Using the training polygons from the random forest classification, calculate the RMSE separately for each of the classes and compare. 
#    Hint - see ?zonal()

# load the training polygons
load("data/trainingPoly.rda")
poly <- trainingPoly
poly.df <- as.data.frame(poly)

-------------------------------------------------------------------------------------------------------------------
## METHOD 1 using original polygons ##

# Extract mean values from vcf and predicted for polygon areas and create dataframe
vcfpoly <- extract(gewata[[7]], poly, fun=mean, df=T, sp=F, na.rm=TRUE)
predpoly <- extract(treemap, poly, fun=mean, df=T, sp=F, na.rm=TRUE)

# Cbind vcf and predicted values in 1 dataframe and name
pred.val.poly <- cbind(predpoly, vcfpoly[,2], poly.df[,2])
names(pred.val.poly) <- c("ID", "Predicted", "VCF", "Class")


# Select rows from certain class value with function
selectClass <- function(x)
{
  m <- pred.val.poly[which(pred.val.poly$Class == x),]
  c <- m[,2:3]
  return(c)
}

classes <- list("cropland", "forest", "wetland")
val.classes <- lapply(classes, selectClass)

# Compute RMSE per class
rmse.crop <- sqrt(mean((val.classes[[1]][,2]-val.classes[[1]][,1])^2))
rmse.forest <- sqrt(mean((val.classes[[2]][,2]-val.classes[[2]][,1])^2))
rmse.wetland <- sqrt(mean((val.classes[[3]][,2]-val.classes[[3]][,1])^2))

rmse.classes2 <- cbind(rmse.crop, rmse.forest, rmse.wetland)
# Here we see that the rmse for forest is the smallest and for wetland the highest

-------------------------------------------------------------------------------------------------------------------
## METHOD 2 using rasterized polygons ##

# Add code to classes
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
# Zone 1 = cropland, Zone 2 = forest, Zone 3 = wetland

# Rasterize polygons
poly.raster <- rasterize(trainingPoly, gewata[[7]], field='Code')

# Make brick of vcf and predicted treemap
vcf.pred.brick <- brick(gewata[[7]], treemap)
names(vcf.pred.brick) <- c("VCF", "Predicted")

# Compute difference and use output in zonal statistics
dif <- vcf.pred.brick[[1]]- vcf.pred.brick[[2]]
dif2 <- dif^2
zonal.stat <- zonal(dif2, poly.raster, fun=mean, na.rm=TRUE)
zonal.stat.sqrt <- sqrt(zonal.stat[,2])
names(zonal.stat.sqrt) <- c("cropland", "forest", "wetland")
# Here we see that the rmse overall is higher and again lowest for forest and highest for wetland
-------------------------------------------------------------------------------------------------------------------

# Show differences between using original polygons and rasterized polygons based on RMSE:
rmse.2methods <- rbind(zonal.stat.sqrt, rmse.classes2)
row.names(rmse.2methods) <- c("sqrt.zonal", "extract.rmse")
