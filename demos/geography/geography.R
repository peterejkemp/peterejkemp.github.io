library(gt)
library(gtExtras)
library(ggrepel)

type = "ks4"
data_folder <- r"[C:\Users\Peter\Google Drive\Kings\Code\PISR\Data\]"
data_folder <- r"[D:\Data\EdR\]"


node <- Sys.info()

if(node["nodename"] == "KCL8KWNXY2"){
  # basefolder <- r"(C:\Users\w1926273\OneDrive - King's College London\Code\SCARI\)"
  basefolder <- r"(C:/Users/w1926273/Google Drive/Kings/Code/PISR/)"
}else if(node["nodename"] == "PETES_PC"){
  basefolder <- r"(C:/Users/Peter/Google Drive/Kings/Code/PISR/)"
  old_proj_folder <- r"(C:/Users/Peter/Google Drive/Kings/Code/OpenAnalysis/)"
}else if(node["nodename"] == "KCL3Y7NKR3"){ # new laptop
  basefolder <- r"(G:\My Drive\Kings\Code\PISR\)"
  old_proj_folder <- r"(C:/Users/Peter/Google Drive/Kings/Code/OpenAnalysis/)"
}

source(paste0(basefolder, "_reports/TRACER/TRACER-setup.R"))
source(paste0(codefolder, 'OpenAnalysis_load.R'))

# work out number of quals per school
subjects_entered <- results_all %>% 
  left_join(schools %>% 
              mutate(year = as.numeric(gsub(".*[-]", "",year))), 
            by=c("year"="year", "urn"="URN")) %>%
  # filter(urn == 140862) %>% 
  select(year, urn, Postcode, `School name`, Qualification, `Subject description`, 
         `Total number entered`, `Number of students at the end of year`, 
         schType, Ofsted, OfstedDate) %>%
  mutate(per_entered = `Total number entered`/ `Number of students at the end of year`) 

  # TODO: work out why there are figures over 1 here?!
#  filter(#per_entered == max(per_entered, na.rm = TRUE))
#    per_entered > 1.0)

# schools with History or Geography

plot_data <- subjects_entered %>% 
  filter(
    grepl("GCSE", Qualification),
    !grepl("iGCSE", Qualification),
    !grepl("GCSE Short Course", Qualification),
    !grepl("(S|s)pecial", schType),
    !grepl("dependent", schType),
    grepl("(History|Geography)", `Subject description`),
    !grepl("Ancient History", `Subject description`))
    #!year %in% c(2012, 2013))

map_plot <- plot_data %>% 
  mutate(per_entered = ifelse(per_entered > 1.0, 1.0, per_entered)) %>%
  left_join(schools %>% select(year, URN, lat, lon), by=c("year", "urn"="URN"))


combos <- plot_data %>% 
  select(year, urn, `Subject description`, per_entered, `Number of students at the end of year`) %>% 
  group_by(year) %>%
  pivot_wider(names_from = `Subject description`, values_from = per_entered)


no_sub <- combos %>% 
  group_by(year) %>%
  summarise(all = n(),
            no_geo = sum(is.na(Geography)),
            no_geo_per = round(100*(no_geo/all),1),
            no_his = sum(is.na(History)),
            no_his_per = round(100*(no_his/all),1)) %>%
  arrange(desc(year)) %>%
  gt() %>%
  cols_label(
    year = "Year",
    all = "Providers",
    no_geo = "No Geography n",
    no_geo_per = "%",
    no_his = "No History n",
    no_his_per = "%"
  )# %>%
  #fmt_percent(columns = c("no_geo_per","no_his_per"))
  

# dup_urn <- plot_data %>%
#   dplyr::group_by(year, urn, `Subject description`) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) %>% pull(urn)
# 
# plot_data %>% filter(urn %in% c(101349))
  #filter(urn %in% dup_urn)

# about the same number of schools offer both qualifications
providers <- plot_data %>% group_by(year, `Subject description`) %>% 
  summarise(n=n(),
            students = sum(`Total number entered`)) %>%
  mutate(year = as.numeric(year))

years <- seq(min(providers$year), max(providers$year), by = 1)

plt_total_providers <- ggplot(data=providers, 
       aes(x=year, y=n, 
           colour=`Subject description`, group=`Subject description`)) + 
  geom_point() + geom_line() +xlab(NULL)+ ylab(NULL) + 
  labs(title = "Providers of History and Geography") +
  scale_x_continuous(
    breaks = years,
    labels = ifelse(years %in% providers$year, as.character(years), "")
  ) +
  theme(legend.position="bottom")

plt_total_students <- ggplot(data=providers, 
       aes(x=year, y=students, 
           colour=`Subject description`, group=`Subject description`)) + 
  geom_point() + geom_line() +xlab(NULL)+ ylab(NULL) + 
  labs(title = "Students taking History and Geography") +
  scale_x_continuous(
    breaks = years,
    labels = ifelse(years %in% providers$year, as.character(years), "")
  ) +
  theme(legend.position="bottom")

provider_size <- plot_data %>% group_by(year) %>%
  mutate(per_tile = ntile(per_entered, 10),
         n_tile = ntile(`Total number entered`, 10))

# Stuff that might be interesting: History classes more likely to
plt_density <- ggplot(data=provider_size, 
       aes(x=per_entered)) + 
  geom_density(aes(colour=`Subject description`, fill=`Subject description`), alpha=0.4)+
  xlab(NULL)+ ylab(NULL) + facet_wrap(year~.) + 
  xlim(0,1) +
  labs(title = "Geography and History GCSE classes as percentage of school cohort, density graphs") +
  theme(legend.position="bottom")

plt_per_school <- ggplot(data=provider_size, 
       aes(x=as.factor(per_tile), 
           fill=`Subject description`, group=`Subject description`)) + 
  geom_bar(position=position_dodge()) +
  xlab(NULL)+ ylab(NULL) + facet_wrap(year~.) + 
  labs(title = "Geography and History GCSE classes as percentage of school cohort",
       subtitle= "tiled into 10 equal groups, by year") +
  theme(legend.position="bottom")

tmp <- provider_size %>% group_by(year, per_tile) %>% summarise(max_tile = max(per_entered))

### leaflet
library(leaflet)
library(sf)
library(scales)



map_data <- map_plot %>% 
  # filter(urn %in% c(115369, 137946, 145061, 137814)) %>% 
  # filter(urn %in% c(106538, 119816, 138470, 112998, 101814)) %>% 
  # filter(grepl("(St Benedict's Catholic College)|Morant|Colchester",`School name`)) %>%
  group_by(year, urn) %>%
  select(year, urn, `School name`, Postcode, lat, lon, `Subject description`, `Number of students at the end of year`, schType, Ofsted, per_entered) %>%
  pivot_wider(names_from = "Subject description", values_from = "per_entered") %>%
  mutate(Geography = round(100*Geography,1),
         History = round(100*History,1)) %>%
  # merge multiple schools
  group_by(`School name`, Postcode) %>%
  arrange(desc(year)) %>%
  mutate(urn_old = ifelse(setequal(lat, unique(lat)) & 
                            setequal(lon, unique(lon)) &
                            setequal(urn, unique(urn)),
                          paste(tail(unique(urn), n=-1), collapse=";"),
                          ""),
         urn = ifelse(setequal(lat, unique(lat)) & 
                        setequal(lon, unique(lon)) &
                        setequal(urn, unique(urn)),
                      head(unique(urn), n=1),
                      urn),
         lat = head(lat, n=1),
         lon = head(lon, n=1)) %>%
  # jitter schools on top of each other, e.g. "(St Benedict's Catholic College)|Colchester County"
  # filter(urn %in% c(115369, 137946, 145061))
  # mutate(grepl("(St Benedict's Catholic College)|Colchester County",`School name`))
  group_by(lat, lon) %>%
  mutate(same_loc = n_distinct(urn) > 1) %>%
  group_by(urn) %>%
  mutate(lat = ifelse(same_loc, lat + runif(1, min = -0.0002, max = 0.0002), lat),
         lon = ifelse(same_loc, lon + runif(1, min = -0.0002, max = 0.0002), lon)) 

most_recent_entries <- map_data %>% 
  group_by(urn) %>% 
  filter(year == max(year)) %>% 
  select(urn, `Number of students at the end of year`, Geography, History) #%>% 
  # distinct() %>% 
  # group_by(urn) %>%
  # count() %>% filter(n > 1)
#format(History, nsmall = 1)
  # 

map_data_final <- map_data %>%
  mutate(content = paste0(year, " (n=",`Number of students at the end of year`,") G: <span>",
                          format(Geography, nsmall = 1), "%</span> <span>H: ",
                          format(History, nsmall = 1), "%</span>")) %>%
  group_by(urn, urn_old, `School name`, lon, lat) %>%
  arrange(desc(year)) %>%
  dplyr::summarise(content = 
                     paste(
                       paste(
                        paste("<h4>",`School name`, 
                              " <a href='https://www.get-information-schools.service.gov.uk/Establishments/Establishment/Details/",
                             urn,"'>",urn,"</a></h4>", sep=""),
                            if_else(unique(urn_old) == "", 
                                    "", 
                                    paste0("[previously: ", 
                                    unique(urn_old), "]</br>"))),
                       reduce(content, paste, sep="<br/>"), 
                       sep="<br/>")) %>% 
  distinct() %>%
  left_join(most_recent_entries) %>%
  ungroup() %>%
  mutate(size = `Number of students at the end of year` / max(`Number of students at the end of year`)) %>%
  arrange(desc(size))

bene <- map_data_final %>% 
  filter(urn %in% c(115369, 137946, 145061))
  # filter(grepl("(St Benedict's Catholic College)|Morant|Colchester",`School name`))



pal <- colorNumeric(c("black", "green"), 
                    domain=c(min(map_data_final$Geography, na.rm = TRUE):max(map_data_final$Geography, na.rm = TRUE)))

map_geo_provision <- leaflet(data=map_data_final) %>%
  # addProviderTiles("Stamen.Toner") %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   fillColor = ~ifelse(is.na(Geography), "red", pal(Geography)), 
                   #fillColor = "black", 
                   color = "black", 
                   weight =1, opacity = 1, fillOpacity = 1,
                   popup = ~content, 
                   radius = ~rescale(size, c(1,10)) + 3.5,
                   label = ~paste(as.character(`School name`), Geography, "%")) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~Geography,
            title = "% taking Geography",
            opacity = 1)


