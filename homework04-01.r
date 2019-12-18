data <- read.csv('./data.csv', header = TRUE)

# 1.	Explore Data
# Extract as much as possible information coming out from the data set
# using graphical tools as described in Chapter 2 of Everitt.

## The data were collected from an investigation of Sudden Infant Death Syndrome (SIDS) The two groups consist of 16 SIDS victims and 49 controls. The Factor68 variable arises from spectral analysis of 24-hour recordings of electrocardiograms and respiratory movements mad on each child. All the infans have a gestational age of 37 weeks or more and were regarded as full term.

# REPORT
## Data Explanation
## The data are the results of investigation of Sudden Infant Death Syndrome (SIDS)
### Group - 1, 2
#### -> 1 = Control (49 rows), 2 = Victim of SIDS (16 rows)
### HR - 76.9~167.0
#### -> Heart rate (beats per minute)
### BW - 1890~4790
#### -> Birthweight (grammes)
### Factor68 - 0.1520~0.6260
#### -> Heart and lung function: spectral analysis of 24-hour electrocardiogrammes recordings and respiratory movements
### Gesage - 37~42
#### -> Gestational Age (weeks)
# /REPORT

attach(data)

# 1.1.	Scatterplots
plot(HR, Factor68, pch = 1, lwd = 2)
abline(lm(Factor68~HR), lwd = 2)
rug(jitter(HR), side = 1)
rug(jitter(Factor68), side = 2)
# REPORT
## CODE: Drawing scatterplots with linear regression line with data jittering at the axis.
## EXPLANATION: From the figure, we can see that there are some week correlation between children's heart rate (HR) and spectrum value (Factor68).
# /REPORT

# 1.2.	Convex Hull
dev.off()
convexHullIndices <- chull(HR, Factor68)
convexHullIndices # Result: [1] 37 48 20  5 51 22
plot(HR, Factor68, pch = 1, lwd = 1)
polygon(HR[convexHullIndices], Factor68[convexHullIndices],
    density = 10, angle = 30)
# REPORT
## CODE: From the result of `chull`, there are 6 data which are in convex hull, which is to say there are outliers of the data.
## EXPLANATION: The figure is showing position of 6 convex hull points and their polygon.
# /REPORT

# 1.3.	Chiplot
dev.off()
source('./functions/chiplot.r')
chiplot(HR, Factor68, vlabs = c('Heart Rate (HR)', 'Heart and Lung Function (Factor68)'))
# REPORT
## CODE: Drawing chiplot using given function
## EXPLANATION: From the figure, it could be interpreted that children's heart rate and heart-lung function data are independent to each other, because in chi-plot, most of the points are inside the bandwidth.
# /REPORT

# 1.4.	Bivariate Boxplot
dev.off()
source('./functions/bvbox.r')
bvbox(
  cbind(HR, Factor68),
  xlab = 'Heart Rate (HR)',
  ylab = 'Heart and Lung Function (Factor68)',
  method = 'O'
)
# REPORT
## CODE: Drawing Bivariate Boxplot using given function
## EXPLANATION: From the figure, it could be interpreted that the value of children's heart rate and heart-lung function are weakly dependent, as the angle between eclipse axises are quite big.
### There are also few outliers can be identified here as they are located outside the eclipse.
# /REPORT

# 1.5.	Estimating Bivariate Densities
dev.off()
source('./functions/bivden.r')
bivariateDensity <- bivden(HR, Factor68)
par(mfrow = c(1, 2))
persp(
  bivariateDensity$seqx,
  bivariateDensity$seqy,
  bivariateDensity$den,
  xlab = 'Heart Rate (HR)',
  ylab = 'Heart and Lung Function (Factor68)',
  zlab = 'Density',
  lwd = 1,
  theta = 230,
  phi = 30
) # Perspective Plot

# Contour Plot
plot(
  HR, Factor68,
  xlab = 'Heart Rate (HR)',
  ylab = 'Heart and Lung Function (Factor68)',
  lwd = 1
)
contour(
  bivariateDensity$seqx,
  bivariateDensity$seqy,
  bivariateDensity$den,
  lwd = 1,
  nlevels = 20,
  add = TRUE
)
# REPORT
## CODE: Drawing perspective plot and contour plot of bivariate densities
## EXPLANATION: From the figure, it is able to identify outliers in the data as the surface of contour is not continuously like normal distribution and there are some small mountains.
# /REPORT

# 1.6.	Bubbleplot
dev.off()
plot(HR, Factor68,
  xlab = 'Heart Rate (HR)',
  ylab = 'Heart and Lung Function (Factor68)',
  pch = 1, lwd = 1)
symbols(HR, Factor68,
  circles = max(Gesage) - Gesage, # Normalise the size of circle
  inches = 0.2, add = TRUE, lwd = 1)
# REPORT
## CODE: Drawing bubble plots of children's heart rate and function value whose bubbles indicate gestational ages.
## FIGURE: Scatter plots of children's heart rate and heart-lung function value having circle whose size indicates gestational ages.
## EXPLANATION: From the figure, it is difficult to indicate that gestitional age has relationship with either heart rate or function value, because the different sizes of circles are placed independently.
# /REPORT

# 1.7.	Scatterplot Matrix
dev.off()
pairs(
  data,
  panel = function(x, y) {
    abline(lm(y~x), lwd = 1)
    points(x, y, lwd = 1)
  }
)
# REPORT
## CODE: Drawing matrix of scatterplots between variables in data
## FIGURE: Matrix of scatterplots between each columns in data
## EXPLANATION: From the figure, it is diffucult to identify wether there is strong correlation between variables in given data, because none of any pairs have such points arrangement that indicate strong relationship.
# /REPORT

# 1.8.	Conditioning Plots
dev.off()
coplot(Factor68~HR|BW)
# REPORT
## CODE: Drawing plots of relationship between heart rate and function value with given birth weight.
## FIGURE: Conditioning plots of heart rate and function value with given birth weight.
## EXPLANATION: From the figure, it can be interpreted that the relationship between children's heart rate and heart-lung function value are independent in both overall and in given specific range of birth weight.
# /REPORT
