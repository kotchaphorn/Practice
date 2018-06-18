# Function to plot two replicates of a sample against each other and calculate Pearson correlation
# Arguments:
  # repA: string indicating the path to the file including the file name and extension (filename.isoform.results in this case)
  # repB: string indicating the path to the file including the file name and extension (filename.isoform.results in this case)
  # sample_name: string indicating the name of the sample

replicate <- function(repA, repB, sample_name) {
  #Reading in data tables
  data_A <- read.table(repA, header = TRUE)
  data_B <- read.table(repB, header = TRUE)
  df_FPKM <- data.frame(data_A$FPKM + 1, data_B$FPKM + 1)
  
  #Pearson correlation
  pearson <- cor(data.frame(data_A$FPKM, data_B$FPKM), use = "everything", method = "pearson")
  
  #Text for plot
  r2_on_plot <- paste("R^2 == ", (round(pearson[1,2], 4)))
  
  #ggplot
  library(ggplot2)
  library(scales)
  ggplot() + geom_point(data = df_FPKM, aes(x = df_FPKM[,1], y = df_FPKM[,2])) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
    labs(x = "Replicate A (FPKM)", y = "Replicate B (FPKM)", title = sample_name) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text", x = min(df_FPKM$data_A.FPKM...1), y = max(df_FPKM$data_B.FPKM...1), hjust = 0.1 ,label = r2_on_plot, parse = TRUE)
  
  #Text for file name
  library(stringr)
  filename <- paste(str_replace_all(sample_name," ", "_"), "replicates.png", sep = "_")
  
  #Save plot as png
  ggsave(filename = filename)
}
