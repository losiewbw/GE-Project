## Set wd

# starter code to access the MySQL database, which is hosting the GE data

# * Loading the Required Packages ------------------------------------------

pacman::p_load(RMySQL, magrittr, dplyr, geosphere, shiny, flexdashboard, plotly, dplyr)

# * Connecting to the Database and Listing the 7 Tables -------------------

mysqlconnection = dbConnect(
  RMySQL::MySQL(),
  dbname='gedata',
  host='mysql.fsb.miamioh.edu',
  port=3306,
  user='fsbstud',
  password='fsb4you')

dbListTables(mysqlconnection) # displays the tables available in this database.

# * Saving each table as a data frame -------------------------------------

engine_data_aic <- RMySQL::dbReadTable(mysqlconnection, "engine_data_aic")
engine_data_axm <- dbReadTable(mysqlconnection, "engine_data_axm")
engine_data_fron <- dbReadTable(mysqlconnection, "engine_data_fron")
engine_data_pgt <- dbReadTable(mysqlconnection, "engine_data_pgt")
esn_rul <- dbReadTable(mysqlconnection, "esn_rul")
airport_codes <- dbReadTable(mysqlconnection, "lkp_airport_codes_t")
manufacturing <- dbReadTable(mysqlconnection, "manufacturing_sql_by_esn")

# Binding the four airline tables together --------------------------------

allairlines <- rbind(engine_data_aic, engine_data_axm, engine_data_fron, engine_data_pgt)

# Joining the esn_rul and manufacturing table by esn -------------------------------------

df1 <- allairlines %>%
  right_join(esn_rul
             , by='esn')

df1 <- df1 %>%
  left_join(manufacturing,
            by = 'esn')

# Joining the airport codes table by departure & destination airport --------

df1 <- inner_join(df1, airport_codes, by=c('depart_icao'='airport_icao'))
df1 <- inner_join(df1, airport_codes, by=c('destination_icao'='airport_icao'))

# Renaming latitude and longitude variables -------------------------------

df1$latitude.x -> df1$latitude_departure
df1$longitude.x -> df1$longitude_departure
df1$latitude.y -> df1$latitude_destination
df1$longitude.y -> df1$longitude_destination

# Removing old variables --------------------------------------------------

df1$latitude.x = NULL
df1$latitude.y = NULL
df1$longitude.x = NULL
df1$longitude.y = NULL

# Using departure and destination coordinates to calculate distance traveled --------

df1 <- df1 %>% rowwise() %>% 
  mutate(distance = distHaversine(c(longitude_departure, latitude_departure),c(longitude_destination, latitude_destination)))

# Writing df1 into a csv for Power BI Dashboard -----------
write.csv(df1, "C:/Users/bwlos/OneDrive/Documents/ISA401/GE Project//df1.csv", row.names = FALSE)

# Models 1, 2, and 3 were each created using either the mean, min, or max of 
# each predictor variable
# Each model is grouped by each esn that contained rul data
# Each model contains the total distance traveled by each esn, along with 
# the max flight cycle the esn was at while at its current rul

# The goal of these models was to see which variables helped predict rul

# MODEL 1 (mean) ################
mean_df <- df1 %>%
  group_by(esn) %>%
  summarise(distance = sum(distance),
            max_cycle = max(flight_cycle),
            rul = min(rul),
            hpc_eff_mod = mean(hpc_eff_mod),
            hpc_flow_mod = mean(hpc_flow_mod),
            t24 = mean(t24),
            t30 = mean(t30),
            t50 = mean(t50),
            p15 = mean(p15),
            p30 = mean(p30),
            nf = mean(nf),
            nc = mean(nc),
            ps30 = mean(ps30),
            phi = mean(phi),
            nrf = mean(nrf),
            nrc = mean(nrc),
            bpr = mean(bpr),
            htbleed = mean(htbleed),
            w31 = mean(w31),
            w32 = mean(w32),
            X44321P02_op016_median_first = mean(X44321P02_op016_median_first),
            X44321P02_op420_median_first = mean(X44321P02_op420_median_first),
            X54321P01_op116_median_first = mean(X54321P01_op116_median_first),
            X54321P01_op220_median_first = mean(X54321P01_op220_median_first),
            X65421P11_op232_median_first = mean(X65421P11_op232_median_first),
            X65421P11_op630_median_first = mean(X65421P11_op630_median_first))
# smallest model to consider
nullmod_mean = lm(rul ~ 1, data = mean_df)
summary(nullmod_mean)
## largest model to consider
fullmod_mean = lm(rul ~ ., data = mean_df)
summary(fullmod_mean)
## code for stepwise
reg_stepwise = step(nullmod_mean, scope = list(lower = nullmod_mean, upper = fullmod_mean),
                    trace = 1, k = 2) ## k = 2 for AIC, k = log(804) for BIC
test_model_mean <- lm(rul ~ max_cycle + nrc + X54321P01_op116_median_first + X44321P02_op016_median_first + 
                   X65421P11_op232_median_first + nrf + w31 + phi + t50 + t30, data = mean_df)
summary(test_model_mean)

# MODEL 2 (max) ##########################
max_df <- df1 %>%
  group_by(esn) %>%
  summarise(distance = sum(distance),
            max_cycle = max(flight_cycle),
            rul = min(rul),
            hpc_eff_mod = max(hpc_eff_mod),
            hpc_flow_mod = max(hpc_flow_mod),
            t24 = max(t24),
            t30 = max(t30),
            t50 = max(t50),
            p15 = max(p15),
            p30 = max(p30),
            nf = max(nf),
            nc = max(nc),
            ps30 = max(ps30),
            phi = max(phi),
            nrf = max(nrf),
            nrc = max(nrc),
            bpr = max(bpr),
            htbleed = max(htbleed),
            w31 = max(w31),
            w32 = max(w32),
            X44321P02_op016_median_first = max(X44321P02_op016_median_first),
            X44321P02_op420_median_first = max(X44321P02_op420_median_first),
            X54321P01_op116_median_first = max(X54321P01_op116_median_first),
            X54321P01_op220_median_first = max(X54321P01_op220_median_first),
            X65421P11_op232_median_first = max(X65421P11_op232_median_first),
            X65421P11_op630_median_first = max(X65421P11_op630_median_first))

# smallest model to consider
nullmod_max = lm(rul ~ 1, data = max_df)
summary(nullmod_max)
## largest model to consider
fullmod_max = lm(rul ~ ., data = max_df)
summary(fullmod_max)
## code for stepwise
reg_stepwise = step(nullmod_max, scope = list(lower = nullmod_max, upper = fullmod_max),
                    trace = 1, k = 2) ## k = 2 for AIC, k = log(804) for BIC
test_model_max <- lm(rul ~ phi + ps30 + nrc + X54321P01_op116_median_first + bpr + 
                   X65421P11_op232_median_first + X44321P02_op016_median_first, data = max_df)
summary(test_model_max)

# MODEL 3 (min) ##########################
min_df <- df1 %>%
  group_by(esn) %>%
  summarise(distance = sum(distance),
            max_cycle = max(flight_cycle),
            rul = min(rul),
            hpc_eff_mod = min(hpc_eff_mod),
            hpc_flow_mod = min(hpc_flow_mod),
            t24 = min(t24),
            t30 = min(t30),
            t50 = min(t50),
            p15 = min(p15),
            p30 = min(p30),
            nf = min(nf),
            nc = min(nc),
            ps30 = min(ps30),
            phi = min(phi),
            nrf = min(nrf),
            nrc = min(nrc),
            bpr = min(bpr),
            htbleed = min(htbleed),
            w31 = min(w31),
            w32 = min(w32),
            X44321P02_op016_median_first = min(X44321P02_op016_median_first),
            X44321P02_op420_median_first = min(X44321P02_op420_median_first),
            X54321P01_op116_median_first = min(X54321P01_op116_median_first),
            X54321P01_op220_median_first = min(X54321P01_op220_median_first),
            X65421P11_op232_median_first = min(X65421P11_op232_median_first),
            X65421P11_op630_median_first = min(X65421P11_op630_median_first))

# smallest model to consider
nullmod_min = lm(rul ~ 1, data = min_df)
summary(nullmod_min)
## largest model to consider
fullmod_min = lm(rul ~ ., data = min_df)
summary(fullmod_min)
## code for stepwise
reg_stepwise = step(nullmod_min, scope = list(lower = nullmod_min, upper = fullmod_min),
                    trace = 1, k = 2) ## k = 2 for AIC, k = log(804) for BIC
test_model_min <- lm(rul ~ p30 + ps30 + w31 + nc + phi, data = max_df)
summary(test_model_min)

## Summary of 3 models
summary(test_model_mean)
summary(test_model_max)
summary(test_model_min)

# COMBINED MODEL ################
# This model was the combination of the statistically significant variables
# in the 3 previous models
comb_df <- df1 %>%
  group_by(esn) %>%
  summarise(distance = sum(distance),
            max_cycle = max(flight_cycle),
            rul = min(rul),
            mean_nrc = mean(nrc),
            mean_X54321P01_op116_median_first = mean(X54321P01_op116_median_first),
            mean_X44321P02_op016_median_first = mean(X44321P02_op016_median_first),
            mean_X65421P11_op232_median_first = mean(X65421P11_op232_median_first),
            mean_nrf = mean(nrf),
            mean_w31 = mean(w31),
            mean_phi = mean(phi),
            mean_t50 = mean(t50),
            mean_t30 = mean(t30),
            max_phi = max(phi),
            max_ps30 = max(ps30),
            max_nrc = max(nrc),
            max_X54321P01_op116_median_first = max(X54321P01_op116_median_first),
            max_bpr = max(bpr),
            max_X65421P11_op232_median_first = max(X65421P11_op232_median_first),
            max_X44321P02_op016_median_first = max(X44321P02_op016_median_first),
            min_ps30 = min(ps30),
            min_p30 = min(p30),
            min_w31 = min(w31),
            min_nc = min(nc),
            min_phi = min(phi))
# smallest model to consider
nullmod_comb = lm(rul ~ 1, data = comb_df)
summary(nullmod_comb)
## largest model to consider
fullmod_comb = lm(rul ~ ., data = comb_df)
summary(fullmod_comb)
## code for stepwise
reg_stepwise = step(nullmod_comb, scope = list(lower = nullmod_comb, upper = fullmod_comb),
                    trace = 1, k = 2) ## k = 2 for AIC, k = log(804) for BIC
test_model_comb <- lm(rul ~ max_ps30 + min_ps30 + max_nrc + min_p30 + mean_X54321P01_op116_median_first,
                      data = comb_df)

# Full Combined MODEL ################
# This model contains the mean, min, and max of each predictor variable
all_df <- df1 %>%
  group_by(esn) %>%
  summarise(distance = sum(distance),
            max_cycle = max(flight_cycle),
            rul = min(rul),
            mean_hpc_eff_mod = mean(hpc_eff_mod),
            mean_hpc_flow_mod = mean(hpc_flow_mod),
            mean_t24 = mean(t24),
            mean_t30 = mean(t30),
            mean_t50 = mean(t50),
            mean_p15 = mean(p15),
            mean_p30 = mean(p30),
            mean_nf = mean(nf),
            mean_nc = mean(nc),
            mean_ps30 = mean(ps30),
            mean_phi = mean(phi),
            mean_nrf = mean(nrf),
            mean_nrc = mean(nrc),
            mean_bpr = mean(bpr),
            mean_htbleed = mean(htbleed),
            mean_w31 = mean(w31),
            mean_w32 = mean(w32),
            mean_X44321P02_op016_median_first = mean(X44321P02_op016_median_first),
            mean_X44321P02_op420_median_first = mean(X44321P02_op420_median_first),
            mean_X54321P01_op116_median_first = mean(X54321P01_op116_median_first),
            mean_X54321P01_op220_median_first = mean(X54321P01_op220_median_first),
            mean_X65421P11_op232_median_first = mean(X65421P11_op232_median_first),
            mean_X65421P11_op630_median_first = mean(X65421P11_op630_median_first),
            max_hpc_eff_mod = max(hpc_eff_mod),
            max_hpc_flow_mod = max(hpc_flow_mod),
            max_t24 = max(t24),
            max_t30 = max(t30),
            max_t50 = max(t50),
            max_p15 = max(p15),
            max_p30 = max(p30),
            max_nf = max(nf),
            max_nc = max(nc),
            max_ps30 = max(ps30),
            max_phi = max(phi),
            max_nrf = max(nrf),
            max_nrc = max(nrc),
            max_bpr = max(bpr),
            max_htbleed = max(htbleed),
            max_w31 = max(w31),
            max_w32 = max(w32),
            max_X44321P02_op016_median_first = max(X44321P02_op016_median_first),
            max_X44321P02_op420_median_first = max(X44321P02_op420_median_first),
            max_X54321P01_op116_median_first = max(X54321P01_op116_median_first),
            max_X54321P01_op220_median_first = max(X54321P01_op220_median_first),
            max_X65421P11_op232_median_first = max(X65421P11_op232_median_first),
            max_X65421P11_op630_median_first = max(X65421P11_op630_median_first),
            min_hpc_eff_mod = min(hpc_eff_mod),
            min_hpc_flow_mod = min(hpc_flow_mod),
            min_t24 = min(t24),
            min_t30 = min(t30),
            min_t50 = min(t50),
            min_p15 = min(p15),
            min_p30 = min(p30),
            min_nf = min(nf),
            min_nc = min(nc),
            min_ps30 = min(ps30),
            min_phi = min(phi),
            min_nrf = min(nrf),
            min_nrc = min(nrc),
            min_bpr = min(bpr),
            min_htbleed = min(htbleed),
            min_w31 = min(w31),
            min_w32 = min(w32),
            min_X44321P02_op016_median_first = min(X44321P02_op016_median_first),
            min_X44321P02_op420_median_first = min(X44321P02_op420_median_first),
            min_X54321P01_op116_median_first = min(X54321P01_op116_median_first),
            min_X54321P01_op220_median_first = min(X54321P01_op220_median_first),
            min_X65421P11_op232_median_first = min(X65421P11_op232_median_first),
            min_X65421P11_op630_median_first = min(X65421P11_op630_median_first))
# smallest model to consider
nullmod_comb = lm(rul ~ 1, data = all_df)
summary(nullmod_comb)
## largest model to consider
fullmod_comb = lm(rul ~ ., data = all_df)
summary(fullmod_comb)
## code for stepwise
reg_stepwise = step(nullmod_comb, scope = list(lower = nullmod_comb, upper = fullmod_comb),
                    trace = 1, k = 2) ## k = 2 for AIC, k = log(804) for BIC
test_model_all <- lm(rul ~ min_ps30 + min_p30 + max_nc + mean_X54321P01_op116_median_first + 
                        max_ps30 + min_p15, data = all_df)
## Summary of all 5 models
summary(test_model_all)
summary(test_model_comb)
summary(test_model_mean)
summary(test_model_max)
summary(test_model_min)

# Refined Data & Model ################
# This data frame only contains the variables used in the final linear regression
# model in order to create the flex dashboard
df <- df1 %>%
  group_by(esn) %>%
  summarise(distance = sum(distance),
            max_cycle = max(flight_cycle),
            rul = min(rul),
            mean_X54321P01_op116_median_first = mean(X54321P01_op116_median_first),
            max_nc = max(nc),
            max_ps30 = max(ps30),
            min_p15 = min(p15),
            min_p30 = min(p30),
            min_ps30 = min(ps30))
# Write the df into a csv for the flex dashboard
write.csv(df, "C:/Users/bwlos/OneDrive/Documents/ISA401/GE Project//df.csv", row.names = FALSE)

# * Data Validation Reporting (Pointblank) --------------------------------
pacman::p_load(pointblank) # new pkg for data validation reporting

# Steps based on package repo: https://rich-iannone.github.io/pointblank/articles/VALID-I.html
# (A) set the action levels
# (B) create the agent for your data
# (C) Create all validation functions
# (D) interrogate to create your HTML report

# (A) action levels
# warn and notify if something >= 1% and do not stop
act = action_levels(warn_at = 0.01, notify_at = 0.01, stop_at = NULL) 
act

# (B) create the agent for your data
agent = create_agent(tbl = df, actions = act)
agent

# (C) Create Validation functions and I will use the concept of piping to chain them

agent %>%
  col_is_numeric(columns = vars(distance, mean_X54321P01_op116_median_first,
                                       max_nc, max_ps30, min_p15, min_p30, min_ps30)) %>% 
  col_is_integer(columns = vars(max_cycle, rul)) -> agent

# save it to agent
agent # overwriting the agent object

# (D) Evaluate Using the interrogate function
res = interrogate(agent) # only steps 6, 7 and 8 passed (because of OK)

res 

export_report(x = res, filename = 'GE_Project_pointblank.html')
