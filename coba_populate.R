province <- c("a","a","a","b","b","b")
year <- c(2019,2020,2021,2019,2020,2021)
gdpr_gr <- c(1,0.5,0.5,2,0.2,0.2)
gdpr_now <- c(1,0,0,2,0,0)
df <- data.frame(province, year, gdpr_gr, gdpr_now)
df
df$gdpr_gr[df$year > 2019 & df$province == "b"]


for (i in df$year>2019){
  print(df$gdpr_gr[i])
}


for (i in 1:nrow(df)){
  for (j in 1:ncol(df)){
    print(df[i,j])
  }
}



names(projection_data)[names(projection_data) == "variable"] <- "year"


df %>%  mutate (gdpr_now = first(gdpr_now)*(gdpr_gr+1))


df$gdpr_now[df$year == 2019]
df[df$year == 2019 & df$province == "b","gdpr_now"]
df %>% gdpr_now
first_year = 2019

  if (df$province == first_year){
    df$gdpr_nowa == df[df$year == 2019 & df$province == "a","gdpr_now"]
    }
  if (df$province > first_year){
    df$gdpr_nowa == df[df$year == 2019 & df$province == "b","gdpr_now"]
  }

df



