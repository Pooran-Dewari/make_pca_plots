library(tidyverse)
library(ggrepel)
library(svglite)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set source R script locatio as setwd()


make_PCA <- function(matrix_for_PCA) {
  #load
  file_name <- matrix_for_PCA
  
  plot_title <- strsplit(file_name, " ")[[1]][2]
  
  my_file <- read_csv(file_name)
  
  #factor Stage values (reorder to display chronologically)    
  my_file$Stage <- factor(my_file$Stage, 
                               levels=c("Late blastula", "Mid gastrula", 
                                        "Early somitogenesis","Mid somitogenesis", 
                                        "Late somitogenesis", "Late-eyed"))
  #plot
  #turn show.legend = TRUE if needed
  my_plot = ggplot(my_file, aes(x=PC1,y=PC2)) + 
    geom_point(aes(colour = Stage), size =12, alpha = 0.7, show.legend = FALSE) +
    labs(
      title = plot_title,
      x = "PC1",
      y = "PC2") +
    scale_color_brewer(palette="Paired")+
    theme_bw(base_size = 45)
  
  ggsave(file=paste(plot_title,"PCA_plot",".svg", sep='_'), plot=my_plot, width=10, height=8)
}


#get list of overlap.txt files
myFiles <- list.files(pattern = ".csv") 

#apply function over the list of files
lapply(myFiles, make_PCA)

rm(myFiles)
rm(list = ls())
