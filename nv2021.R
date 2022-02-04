setwd("")
#install.packages(c("tidyverse", "psych"))
library(tidyverse)
library(psych)
rm(list = ls())

df <- read.csv("nv2021.csv")

### Cohen's d [Between-Group] ###

# Cohen's d_s (w. pooled SD): Between-group
df$d_s <- (df$m2-df$m1) / sqrt( (((df$n2-1)*df$sd2^2) + ((df$n1-1)*df$sd1^2)) / (df$n2+df$n1-2))

# Cohen's d_s CIs (calculated with the 'psych' package)

d_sLL <- vector()
d_sUL <- vector()
i = 1
for(i in i:nrow(df)){
  d_sLL <- append(d_sLL, d.ci(df$d_s[i], n1 = df$n1[i], n2 = df$n2[i])[1])
  d_sUL <- append(d_sUL, d.ci(df$d_s[i], n1 = df$n1[i], n2 = df$n2[i])[3])
  i = i + 1
}

df$d_sLL <- d_sLL
df$d_sUL <- d_sUL

# Cohen's d_s SE (Borenstein et al., p. 27)
df$d_sSE <- sqrt((df$n1 + df$n2) / (df$n1 * df$n2) + df$d_s^2 / (2*(df$n1 + df$n2)))

### Hedge's g [Between-Group] ###

# Hedge's g_s (approximated correction factor J): Between-group
df$g_s <- df$d_s*(1-(3/(4*(df$n1+df$n2)-9)))

# Hedge's g_s CIs (calculated with the 'psych' package)

g_sLL <- vector()
g_sUL <- vector()
i = 1
for(i in i:nrow(df)){
  g_sLL <- append(g_sLL, d.ci(df$g_s[i], n1 = df$n1[i], n2 = df$n2[i])[1])
  g_sUL <- append(g_sUL, d.ci(df$g_s[i], n1 = df$n1[i], n2 = df$n2[i])[3])
  i = i + 1
}

df$g_sLL <- g_sLL
df$g_sUL <- g_sUL

# Hedge's g_s SE (Borenstein et al., p. 27)
df$g_sSE <- sqrt((df$n1 + df$n2) / (df$n1 * df$n2) + df$g_s^2 / (2*(df$n1 + df$n2)))


### Cohen's d [Within-Group] ###

# Cohen's d_av (w. pooled SD): Within-group
df$d_av <- (df$m2-df$m1)/((df$sd2+df$sd1)/2)

# Cohen's d_av CIs (calculated with the 'psych' package)

d_avLL <- vector()
d_avUL <- vector()
i = 1
for(i in i:nrow(df)){
  d_avLL <- append(d_avLL, d.ci(df$d_av[i], n1 = df$n1[i], n2 = df$n2[i])[1])
  d_avUL <- append(d_avUL, d.ci(df$d_av[i], n1 = df$n1[i], n2 = df$n2[i])[3])
  i = i + 1
}

df$d_avLL <- d_avLL
df$d_avUL <- d_avUL

# Cohen's d_av SE (Borenstein et al., p. 27)
df$d_avSE <- sqrt((df$n1 + df$n2) / (df$n1 * df$n2) + df$d_av^2 / (2*(df$n1 + df$n2)))


### Hedge's g [Within-Group] ###

# Hedge's g_av (approximated correction factor J): Within-group
df$g_av <- df$d_av*(1-(3/(4*(df$n1+df$n2)-9)))

# Hedge's g_av CIs (calculated with the 'psych' package)

g_avLL <- vector()
g_avUL <- vector()
i = 1
for(i in i:nrow(df)){
  g_avLL <- append(g_avLL, d.ci(df$g_av[i], n1 = df$n1[i], n2 = df$n2[i])[1])
  g_avUL <- append(g_avUL, d.ci(df$g_av[i], n1 = df$n1[i], n2 = df$n2[i])[3])
  i = i + 1
}

df$g_avLL <- g_avLL
df$g_avUL <- g_avUL

# Hedge's g_av SE (Borenstein et al., p. 27)
df$g_avSE <- sqrt((df$n1 + df$n2) / (df$n1 * df$n2) + df$g_av^2 / (2*(df$n1 + df$n2)))

### Write the data to a new file

write.csv(df, "nv2021_EffectSizes.csv")

### Split the data into between- and within-group datasets

df_btw <- subset(df, b_w == "1")
df_wtn <- subset(df, b_w == "2")

write.csv(df_btw, "nv2021_btw.csv")
write.csv(df_wtn, "nv2021_wtn.csv")
