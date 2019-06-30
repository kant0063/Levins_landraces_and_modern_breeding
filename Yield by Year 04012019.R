library(ggplot2)

dir_in = #path_to_file
dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data"

df = file.path(dir_in, "Yield by Year 04012019.csv")
df = read.csv(df, stringsAsFactors=FALSE)

plt = ggplot(yields) +
  xlab("Year") +
  ylab("Yield [bu/ac]") +
  geom_errorbar(aes(x=YEAR,
                    ymin=YIELD-ERROR,
                    ymax=YIELD+ERROR),
                width=0.2,
                color="#666666") +
  geom_smooth(aes(x=YEAR,
                  y=YIELD),
              color='tomato4',
              method='lm',
              weight=0.1,
              linetype="dashed") +
  geom_point(aes(x=YEAR,
                 y=YIELD),
             color='steelblue3',
             size=4) +
  theme_light()
plt

write=FALSE
if (write) {
  file_out = paste("Yield by Year ",
                   Sys.Date(),
                   ".pdf",
                   sep="")
  file_out = file.path(dir_in, file_out)
  
  pdf(file_out, height=4, width=5)
  plt
  dev.off()
}