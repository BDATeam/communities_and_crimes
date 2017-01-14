#----------------------------------------------
#               PCA Analysis
#----------------------------------------------




#----------------------------------------------
#               Instructions if you are in a hurry

# Remember to do PCA only when there are no missing values in your data. 

# Please fill the following lines
your_data = tab_full # fill with the full dataset on which you wish to do a PCA
n = 100 # fill with the column number in which the values you wish to predict are in your dataset

# Now run the lines at the bottom of the document (starting line 70) and to the basic outputs section
#----------------------------------------------





#----------------------------------------------
#               Basic outputs and plots 

# Outputs
out_pca = PCA(your_data[,-n],scale.unit = TRUE, ncp = 100, graph = FALSE) # for global information on the PCA
summary(out_pca)
your_data_in_PCA = out_pca$ind$coord # transformed database

# Plots : individuals in PCA representation
proj_individuals(all_data = your_data, dim = c(1,2,3), coloring = "hclust4") 
# 3D visualization of individuals in PCA
# dim are the projection dimensions 
# coloring can be "none", "quartile" and "hclust4" (hierarchical clustering with 4 clusters)
screeplot = plot(ev$cp,ev$eigenv,type="h",lwd=50,lend="butt",xlab="Principal components",ylab="Eigenvalues", main="Screeplot")

#----------------------------------------------





#----------------------------------------------
#               More refined outputs and plots
# Outputs
eigenv # for the eigenvalues

# Plots
radar_diagram(data_to_represent = your_data[,-n], names = non_pred$communitynam, individuals = c(1:5), features = c(1,2,3,4,5)) # warning : there needs to be 6 or less columns/features and not too many lines/individuals, or else it is not readable
summary(your_data[,-n])
make_boxplot(data_to_represent = your_data[,-n], features = c(1,2,3,4,5)) # warning : if there are too many features (ie columns), it is not readable
make_scatterplot(data_to_represent = your_data[,-n], features = c(1,2,3,4,5)) # warning : if there are too many features, it is not readable
cor(your_data[,-n]) # correlation matrix
inertia_resumed = plot(ev_cum$cp, ev_cum$eigenv_cum, type = "h", lwd = 50, lend = "butt", xlab = "Principal components", ylab = "Total inertia resumed", main = "Total inertia resumed / Principal components")

# Correlation circle
# Argument for color intensity can be "cos2" (for color varying according to quality of representation) or "contrib" (for contributions) or "none"
corr_circle(data_to_represent = your_data[,-n], dim1 = 1, dim2 = 2, argument = "cos2")
corr_circle(data_to_represent = your_data[,-n], dim1 = 1, dim2 = 2, argument = "contrib")

# Communities with biggest impact (label is the index of the line)
states_with_biggest_impact(out_pca_to_consider = out_pca, dim = c(1,2), type_of_impact = "contributions")  # type of impact can be "qualities_of_rep" or "contributions"




#----------------------------------------------
#               Run lines after this

# Libraries
library(FactoMineR)
library(ggplot2)
library(rgl)
library(factoextra)

# Radar diagram
radar_diagram = function(data_to_represent, names, individuals, features){
    plot = stars(data_to_represent[individuals, features],labels = names[individuals],key.loc=c(10,1.8),main="Radar diagrams",cex=0.5,flip.labels=TRUE)
    return(plot)
} 

# Box plot
make_boxplot = function(data_to_represent, features){
    plot = boxplot(data_to_represent[,features], main="Boxplots") 
    return(plot)
}

# Scatterplot
make_scatterplot = function(data_to_represent, features){
    plot = pairs(data_to_represent[,features], main = "Scatterplot")
    return(plot)
}

# Eigenvalues and screeplot
eigenv = out_pca$eig$eigenvalue
eigenv_cum=cumsum(out_pca$eig$eigenvalue)/sum(eigenv) 
cp=1:length(eigenv) 
ev=data.frame(cp=cp,eigenv=eigenv) 
ev_cum=data.frame(cp=cp,eigenv_cum=eigenv_cum)

# Correlation circle
corr_circle = function(data_to_represent,dim1,dim2,argument){
    out_pca_inter = PCA(data_to_represent,scale.unit = TRUE, ncp = 100, graph = FALSE)
    if (argument == "none"){
        corr_circl = plot.PCA(out_pca_inter, shadow = TRUE, cex = 0.8, axes = c(dim1,dim2), choix = "var", new.plot = TRUE, title = "Correlation circle")
    } else {
        corr_circl = fviz_pca_var(out_pca_inter, alpha.var = argument, axes = c(dim1,dim2)) + theme_minimal()
    }
        return(corr_circl)
} 

# 3D visualization of PCA
proj_individuals = function(all_data, dim, coloring){ 
    out_pca_inter = PCA(all_data[,-n],scale.unit = TRUE, ncp = 100, graph = FALSE) # for global information on the PCA
    data_to_represent = out_pca_inter$ind$coord
    if (coloring == "hclust4"){
        fit <- hclust(dist(data_to_represent[,dim]), method="complete") # cluster
        groups <- cutree(fit, k=4)
        if (length(dim) == 2){
            plot = plot(data_to_represent[,dim], col=groups, main = "Projections of individuals: hclust classes")
        } else {
            plot = plot3d(data_to_represent[,dim], col=groups, type="s", size=1, axes=F)
            axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
            grid3d("x")
            grid3d("y")
            grid3d("z")
        }
    } else if (coloring == "quartiles"){
        all_data$Classes = cut(all_data$ViolentCrimesPerPop,breaks=quantile(all_data$ViolentCrimesPerPop),include.lowest=TRUE)
        levels(all_data$Classes)=c(1,2,3,4)
        if (length(dim) == 2){
            plot = plot(data_to_represent[,dim], col=all_data$Classes, main = "Projections of individuals: quartile classes")
        } else {
            plot = plot3d(data_to_represent[,dim], col=all_data$Classes, type="s", size=1, axes=F)
            axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
            grid3d("x")
            grid3d("y")
            grid3d("z")
        }
    } else {
        if (length(dim) == 2){
            plot = plot(data_to_represent[,dim], main = "Projections of individuals")
        } else {
            plot = plot3d(data_to_represent[,dim], type="s", size=1, axes=F)
            axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
            grid3d("x")
            grid3d("y")
            grid3d("z")
        }
    }
    return(plot)
} 

# States with biggest impact
states_with_biggest_impact = function(out_pca_to_consider, dim, type_of_impact){ 
    if (type_of_impact == "contributions"){
        plot = plot.PCA(out_pca_to_consider, axes = dim, shadow = TRUE, cex = 0.8, select = "contrib 10", title = "The 10 individuals with the biggest contributions")
    } else if (type_of_impact == "qualities_of_rep") {
        plot = plot.PCA(out_pca_to_consider, axes = dim, shadow = TRUE, cex = 0.8, select = "cos2 10", title = "The 10 individuals with the biggest qualities of representation")
    }
    return(plot)
} 


