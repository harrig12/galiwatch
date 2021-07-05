# helper functions for dashboard

##### Branding - color palette

tsc <- colorRampPalette(c('#42C9A9','#75B950', '#12284F'))


##### Plotting helpers 

c2f <- function(temp){(temp * (9/5) + 32)}
f2c <- function(temp){(temp-32) * (5/9)}

cut_names <- function(lvls){
  
  fst <- paste0('<=', gsub(']', '',strsplit(lvls[1], ',')[[1]][2]))
  lst <- paste0('>', gsub('(', '', fixed = T,
                          strsplit(lvls[length(lvls)], ',')[[1]][1]))
  
  lvls <- gsub(',', ' to ', lvls)
  lvls <- gsub(']', '', lvls)
  lvls <- gsub('(', '', lvls, fixed=T)
  lvls[1] <- fst
  lvls[length(lvls)] <- lst
  
  return(lvls)
}


windrose <- function(data, y_bins=5, dir_lab = 'wind direction(Degree)',
                     bar_lab = 'Wind Speed(mph)', legend_title='Wind Speed (mph)'){
  data %>% 
  arrange(!!sym(dir_lab)) %>%
  mutate(x =cut(!!sym(dir_lab), 16, 
                labels = c("N","NNE", "NE", "ENE", 
                           "E", "ESE", "SE", "SSE", 
                           "S", "SSW", "SW", "WSW", 
                           "W", "WNW", "NW", "NNW")),
         z = cut(!!sym(bar_lab), y_bins),
         y = cut(!!sym(bar_lab), y_bins, labels = cut_names(levels(z)))
  ) %>%
  mutate(x = factor(x, levels = c("E", "ESE", "SE", "SSE", 
                                  "N", "NNE", "NE", "ENE", 
                                  "W", "WNW", "NW", "NNW",
                                  "S", "SSW", "SW", "WSW") )) %>%
  group_by(x,y) %>%
  summarise(prop = n(), .groups = 'drop') %>%
  mutate(prop = prop/sum(prop)) %>%
  tidyr::complete(x,y,fill = list(prop=0)) %>%
  plot_ly(type='barpolar', r=~prop, theta=~x, 
          color=~y, colors= tsc(y_bins)) %>%
  return()
   
}

# pretty switch button 
# adapted from https://github.com/statnmap/RshinyApps

fc_switch <- function(inputId, label, value=FALSE, type="TF") {
  
  colclass <- paste("BluGreen", "fc")
  
  tagList(
    tags$div(class = "form-group shiny-input-container",
      tags$div(class = colclass,
        tags$label(label, class = "control-label"),
        tags$div(class = "onoffswitch",
         tags$input(type = "checkbox", name = "onoffswitch", 
                    class = "onoffswitch-checkbox",
                    id = inputId, checked = ""
         ),
         tags$label(class = "onoffswitch-label", `for` = inputId,
                    tags$span(class = "onoffswitch-inner"),
                    tags$span(class = "onoffswitch-switch")
         )
   ))))
  
}

