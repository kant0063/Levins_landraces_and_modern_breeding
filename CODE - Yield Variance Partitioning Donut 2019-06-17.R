#! /bin/R

library(ggplot2)

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Figures"
file_in = "PLOT DATA - Yield Variance Partitioning Donut 2019-06-17.csv"
pltdf = read.csv(file.path(dir_in, file_in), header=TRUE, stringsAsFactors=FALSE)

pltdf$Source = gsub("Genotype", "Variety", pltdf$Source)


pltdf$Source = factor(pltdf$Source, levels = pltdf$Source)

donut = ggplot(pltdf, aes(fill=Source,
                          xmin=2,
                          xmax=5,
                          ymin=MN,
                          ymax=MX)) +
  scale_fill_manual(values=c('steelblue1', 'steelblue2', 'steelblue3', 'tomato2', 'steelblue4', 'gray'),
                    name='Source of Yield\nVariation') +
  geom_rect() +
  coord_polar(theta='y') +
  xlim(c(0, 5)) +
  theme_light() +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank())
donut

#### Write to PDF ####
ww = FALSE
# ww = TRUE
if (ww) {
  fout = paste("Fig 3 - Yield Variance Partitioning Donut ",
               Sys.Date(),
               ".pdf",
               sep="")
  fout = file.path(dir_in, fout)
  pdf(file=fout, height=4, width=5)#, res=300)
  donut
  dev.off()
}