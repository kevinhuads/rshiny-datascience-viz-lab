## Data preparation for the clustering section (US exports and PCA)

library(geojsonio)
library(ade4)
library(factoextra)
library(plotly)  # for toRGB()

usa_df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")  # US exports by state (2011)
usa_leaf <- geojson_read("Data_prep/Rawdata/state.geojson", what = "sp")                               # state geometries for maps

usa_l <- list(color = toRGB("white"), width = 2)  # state border style for choropleth

usa_export = colnames(usa_df)[-(1:3)]             # keep only export measure columns
rownames(usa_df) = usa_df$state                   # state names as row names for PCA

usa_pca = dudi.pca(usa_df[, usa_export], scannf = FALSE, nf = 10)  # PCA on exports, first 10 components

usa_eig = as.data.frame(cbind(0:length(usa_pca$eig),
                              c(0, cumsum(usa_pca$eig) / sum(usa_pca$eig))))  # cumulative variance explained
colnames(usa_eig) = c("Eigenvalues", "Variance_Explained")

power_abs = function(x, p = 0.5) {  # signed power transform
  y = x
  y[y >= 0] = y[y >= 0]^p
  y[y < 0]  = -((-y[y < 0])^p)
  return(y)
}

usa_pca_sqrt = usa_pca
for (i in names(usa_pca_sqrt)[sapply(usa_pca_sqrt, class) == "data.frame"]) {
  usa_pca_sqrt[[i]] = power_abs(usa_pca_sqrt[[i]], p = 0.25)  # soften extremes in PCA scores/loadings
}

save(list = ls(pattern = "usa"), file = "Application/Saved/usa.RData")  # persist clustering objects for the app