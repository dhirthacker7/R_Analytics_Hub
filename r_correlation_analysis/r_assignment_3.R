# pierson correlation

# importing dataset
data(SaratogaHouses, package = "mosaicData")
View(SaratogaHouses)

# selecting numeric variables
df <- dplyr::select_if(SaratogaHouses, is.numeric)

# calculating the correlations
r <- cor(df, use="complete.obs")
round(r, 2)

library(ggplot2)
library(ggcorrplot)
# dark red - strong positive correlations
# dark blue - strong negative correlations
# white - no correlations
ggcorrplot(r)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

# hc.order = TRUE reorders variables, placing similar correlation 
# patterns together.
# type = "lower" plots the lower portion of the correlation matrix.
# lab = TRUE overlays the correlation coefficients (as text) on the plot.