# AdvancedRasterAnalysis
# TeamTropical - Darell.vanderVoort & Froede.Vrolijk
# 13th of Janaury 2016

# To do
# 1: produce one or more plots that demonstrate the relationship between the Landsat bands and the VCF tree cover

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

# Get 5% of dataset
#gewata[runif(1177*1548) >= 0.05] <- NA

# Show all correlation plots
#pairs(gewata, hist=TRUE)

# show bands with high correlation
#par(mfrow = c(1, 4))
#plot(gewata[[1]], gewata[[6]])
#plot(gewata[[2]], gewata[[6]])
#plot(gewata[[4]], gewata[[6]])
#plot(gewata[[5]], gewata[[6]])
#par(mfrow = c(1, 1)) # reset plot area

# 2: create an lm() model and show a summary (e.g. using summary()) of the model object you created. 
     #Which predictors (bands) are probably most important in predicting tree cover?

# Get a value table with all data and remove NAs
valtable <- as.data.frame(getValues(gewata))
valtable <- na.omit(valtable)

# Create the LM model
lm.1 <- lm(valtable$vcf2000Gewata ~ valtable$gewataB1 + valtable$gewataB2 + valtable$gewataB3 + valtable$gewataB4 + valtable$gewataB5 + valtable$gewataB7)

# Give a summary 
summary(lm.1)
# Here we see that band 2 and 1 are most important, altough the std. error of band 2 is highest. Then band 5 and 4. The lowest is band 7


# 3: Plot the predicted tree cover raster and compare with the original VCF raster
treemap <- predict(gewata, model = lm.1, na.rm=FALSE)


# 4: Compute the RMSE between your predicted and the actual tree cover values 


# 5: are the differences between the predicted and actual tree cover the same for all of the 3 classes we used for the random forest classfication? 
#    Using the training polygons from the random forest classification, calculate the RMSE separately for each of the classes and compare. 
#    Hint - see ?zonal().



# Load libraries