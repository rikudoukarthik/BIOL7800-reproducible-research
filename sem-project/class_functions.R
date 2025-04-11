# get inflection point

get_inflection <- function(data) {
  
  data %>% 
    group_by(metric, mountain) %>% 
    # calculate simple diff
    mutate(pred_min1 = lag(pred, default = first(pred)),
           pred_plus1 = lead(pred, default = last(pred))) %>% 
    mutate(pred_diff = pred - pred_min1) %>% 
    # decide whether or not that elevational band is an inflection point for the trend
    # only need lead, since we want just one elevation to be inflection point
    # get two TRUEs if use lead and lag
    mutate(pred_diff_plus1 = lead(pred_diff, default = last(pred_diff))) %>% 
    mutate(inflection = case_when(
      # if first or last elevational band, not inflection
      (pred_min1 == pred | pred_plus1 == pred) ~ FALSE,
      
      (pred_diff_plus1 >= 0 & pred_diff >= 0) |
        (pred_diff_plus1 <= 0 & pred_diff <= 0) ~ FALSE,
      
      TRUE ~ TRUE
    )) %>% 
    # select(-contains("1")) %>% 
    select(metric, mountain, elevation, pval, pred, pred_diff, inflection) %>% 
    ungroup()
  
}


# classify elevational bands into L, M, H

get_elev_cats <- function(data) {
  
  data_cats <- data %>% 
    distinct(mountain, elevation) %>% 
    group_by(mountain) %>% 
    mutate(elev_min = min(elevation),
           elev_max = max(elevation)) %>% 
    # could do ceiling and floor for first and second if we want to be conservative, or opposite if inverse
    # but leave it like this for now
    mutate(elev_thresh1 = round(quantile(elevation, probs = 1/3), -2),
           elev_thresh2 = round(quantile(elevation, probs = 2/3), -2)) %>% 
    group_by(mountain, elevation) %>% 
    reframe(elev_cat = case_when(
      elevation < elev_thresh1 ~ "LOW",
      elevation > elev_thresh2 ~ "HIGH",
      TRUE ~ "MID"
    )) 
  
  data %>% left_join(data_cats, by = c("mountain", "elevation"))
 
  # Mikura-Jima is the mountain with fewest elevational bands classified as MID 
  # but n = 3 (> 2) so this is fine
   
}


# classify trends as valley or peak

class_valley_peak <- function(data) {
  
  data_class <- data %>% 
    group_by(metric, mountain) %>% 
    filter(elev_cat == "LOW") %>% 
    reframe(valleypeak = case_when(
      nth(pred_diff, 2) < 0 ~ "valley",
      nth(pred_diff, 2) > 0 ~ "peak",
      TRUE ~ NA
    )) 
  
  data %>% left_join(data_class, by = c("metric", "mountain"))
  
}
