library(gt)
library(gtExtras)

# work out number of quals per school
subjects_entered <- results_all %>% 
  left_join(schools %>% 
              mutate(year = as.numeric(gsub(".*[-]", "",year))), 
            by=c("year"="year", "urn"="URN")) %>%
  # filter(urn == 140862) %>% 
  select(year, urn, `School name`, Qualification, `Subject description`, 
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


combos <- plot_data %>% 
  select(year, urn, `Subject description`, per_entered) %>% 
  group_by(year) %>%
  pivot_wider(names_from = `Subject description`, values_from = per_entered)


combos %>% 
  group_by(year) %>%
  summarise(all = n(),
            no_geo = sum(is.na(Geography)),
            no_geo_per = round(100*(no_geo/all),1),
            no_his = sum(is.na(History)),
            no_his_per = round(100*(no_his/all),1))

# dup_urn <- plot_data %>%
#   dplyr::group_by(year, urn, `Subject description`) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) %>% pull(urn)
# 
# plot_data %>% filter(urn %in% c(101349))
  #filter(urn %in% dup_urn)

# about the same number of schools offer both qualications
providers <- plot_data %>% group_by(year, `Subject description`) %>% 
  summarise(n=n(),
            students = sum(`Total number entered` ))

ggplot(data=providers, 
       aes(x=as.factor(year), y=n, 
           colour=`Subject description`, group=`Subject description`)) + 
  geom_point() + geom_line() +xlab(NULL)+ ylab(NULL) + 
  labs(title = "Providers of History and Geography")

ggplot(data=providers, 
       aes(x=as.factor(year), y=students, 
           colour=`Subject description`, group=`Subject description`)) + 
  geom_point() + geom_line() +xlab(NULL)+ ylab(NULL) + 
  labs(title = "Students taking History and Geography")

provider_size <- plot_data %>% group_by(year) %>%
  mutate(per_tile = ntile(per_entered, 10),
         n_tile = ntile(`Total number entered`, 10))

# Stuff that might be interesting: History classes more likely to

ggplot(data=provider_size, 
       aes(x=per_entered)) + 
  geom_density(aes(colour=`Subject description`, fill=`Subject description`), alpha=0.4)+
  xlab(NULL)+ ylab(NULL) + facet_wrap(year~.) + 
  xlim(0,1) +
  labs(title = "Geography and History GCSE classes as percentage of school cohort, density graphs")


ggplot(data=provider_size, 
       aes(x=as.factor(per_tile), 
           fill=`Subject description`, group=`Subject description`)) + 
  geom_bar(position=position_dodge()) +
  xlab(NULL)+ ylab(NULL) + facet_wrap(year~.) + 
  labs(title = "Geography and History GCSE classes as percentage of school cohort, tiled into 10 equal groups, by year")

tmp <- provider_size %>% group_by(year, per_tile) %>% summarise(max_tile = max(per_entered))




ggplot(data=provider_size, 
       aes(x=as.factor(n_tile), 
           fill=`Subject description`, group=`Subject description`)) + 
  geom_bar(position=position_dodge()) +
  xlab(NULL)+ ylab(NULL) + facet_wrap(year~.)+ 
  labs(title = "Geography and History GCSE class sizes, tiled into 10 equal groups, by year")

