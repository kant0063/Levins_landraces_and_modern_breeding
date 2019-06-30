library(ggplot2)

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Figures"
file_in = "PLOT DATA - Fitness Sets 2019-06-25.csv"
pltdf = read.csv(file.path(dir_in, file_in), header=TRUE, stringsAsFactors=FALSE)

plt = ggplot(pltdf) +
  scale_y_continuous(breaks=scales::pretty_breaks(2)) +
  scale_x_continuous(breaks=scales::pretty_breaks(2)) +
  facet_wrap(~YEAR,
             scales='free', 
             ncol=6) +
  geom_point(aes(x=SOUTHERN,
                 y=NORTHERN),
             alpha=0.5,
             color='steelblue4') +
  xlab(expression(paste("Yield, South Derived Region [Mg ", "ha"^"-1", "]"))) +
  ylab(expression(paste("Yield, North Derived Region [Mg ", "ha"^"-1", "]"))) +
  theme_light() +
  theme(strip.text.x=element_text(margin=margin(2, 0, 2, 0, 'pt')))
plt


#### Write to PDF ####
ww = FALSE
# ww = TRUE
if (ww) {
  fout = paste("Fig 2 - Fitness Sets ",
               Sys.Date(),
               ".pdf",
               sep="")
  fout = file.path(dir_in, fout)
  pdf(file=fout, height=3, width=6)#, res=300)
  plt
  dev.off()
}
