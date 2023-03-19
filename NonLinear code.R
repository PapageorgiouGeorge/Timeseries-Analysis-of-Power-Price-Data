library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ifultools)
library(tseries)
library(nonlinearTseries)
library(plot3D)
library(tseriesChaos)
library(fractaldim)

my_data <- read_excel(file.choose())

df <- my_data %>% 
  unite(date, YEAR , MONTH, DAY, sep="-") %>% 
  mutate(timestamp=paste(date, TIME) %>% 
           as.POSIXct(., format="%Y-%m-%d %H")) %>% 
  select(timestamp, 'PRICES Sicilia') 

which(is.na(df$timestamp))

df[2067, 1] = as_datetime("2011-03-28 00:59:59")
df[2067, 1]

# produce the timeseries graph
ggplot(df, aes(timestamp, `PRICES Sicilia`)) + geom_line()


# calculate the proper time delay, we prefer ami beacuse usually is smaller

t_acf = timeLag(df$`PRICES Sicilia`, technique = 'acf') 
t_acf

t_ami = timeLag(df$`PRICES Sicilia`, technique = 'ami') 
t_ami


# produce the phase portrait graph
takens = buildTakens(df$`PRICES Sicilia`, 3, t_ami)
takens_df = as.data.frame(takens)

scatter3D(takens_df$V1, takens_df$V2, takens_df$V3)

# calculate and produce the correlation integrals’ graph
cor_dim = corrDim(time.series = df$`PRICES Sicilia`, 
        min.embedding.dim = 3, 
        max.embedding.dim = 12, 
        time.lag = t_ami, 
        min.radius = 0.001,
        max.radius = 50,
        n.points.radius=100,
        theiler.window=100,
        number.boxes=100)


d2 = d2(series = df$`PRICES Sicilia`,
        m = 15,
        d = t_ami,
        t = 4,
        eps.min = 2)

plot(d2)

fd = fd.estim.boxcount(df$`PRICES Sicilia`,
                       nlags = "auto",
                       plot.loglog = TRUE)
fractal_dim = fd$fd
fractal_dim




# produce the correlation dimension vs. embedding dimension graph and estimate the fractal dimension, the minimum embedding dimension and the essential embedding dimension
emb.dim = estimateEmbeddingDim(df$`PRICES Sicilia`, time.lag = t_ami,
                               max.embedding.dim = 15)
emb.dim



# calculate Kolmogorov entropy and (optional) produce the relevant graph
sampleEntropy(cor_dim)


# Convert the power-price timeseries from hourly sampling (8160 points) to 4-hour sampling timeseries (2190 points), averaging the 4 points to be replaced for each new sample. Then follow the previous procedure.

new = colMeans(matrix(df$`PRICES Sicilia`, nrow=4))

newdf = as.data.frame(new)

setDT(newdf, keep.rownames = TRUE)[]



# calculate the proper time delay, we prefer ami beacuse usually is smaller

t_acf2 = timeLag(newdf$new, technique = 'acf') 
t_acf2

t_ami2 = timeLag(newdf$new, technique = 'ami') 
t_ami2

# produce the phase portrait graph

takens2 = buildTakens(newdf$new, 3, t_ami2)
takens_df2 = as.data.frame(takens2)

scatter3D(takens_df2$V1, takens_df2$V2, takens_df2$V3)


# produce the correlation dimension vs. embedding dimension graph and estimate the fractal dimension, the minimum embedding dimension and the essential embedding dimension

emb.dim2 = estimateEmbeddingDim(newdf$new, time.lag = t_ami2,
                               max.embedding.dim = 15)
emb.dim2


# calculate Kolmogorov entropy and (optional) produce the relevant graph
sampleEntropy(cor_dim2)

