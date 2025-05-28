
require(tidyverse);
require(sf); 
require(mapview); 
require(magrittr);
require(RColorBrewer);
require(classInt); 
require(ggplot);
require(ggrepel);
require(classInt); 
require(leaflet);
require(tmap);
require(gganimate);
require(ggpubr);
require(patchwork)



getwd()

nys_health_sf <- st_read("nys_health_sf.gpkg", quiet = TRUE)

covid_data <-  st_read("covidPopZipNYC.gpkg", quiet = TRUE)

str(covid_data)

hist(covid_data$COVID_CASE_COUNT,
     main = "Histogram of New COVID-19 Cases",
     col = "skyblue",
     border = "white")

breaks_j <- classIntervals(c(min(covid_data$COVID_CASE_COUNT) - .00001,
                             covid_data$COVID_CASE_COUNT), n = 7, style = "jenks")

covid_data <- covid_data %>% 
  mutate(covid_data_cut = cut(COVID_CASE_COUNT, breaks_j$brks, 
                              include.lowest = TRUE, dig.lab = 4))

clean_labels <- sapply(1:(length(breaks_j$brks) - 1), function(i) {
  paste0(
    format(breaks_j$brks[i], big.mark = ",", scientific = FALSE, trim = TRUE),
    " – ",
    format(breaks_j$brks[i + 1], big.mark = ",", scientific = FALSE, trim = TRUE)
  )
})

ggplot(covid_data) + 
  geom_sf(aes(fill = covid_data_cut)) +
  scale_fill_brewer(palette = "Purples", 
                    name = ' Covid Cases per Zipcode', 
                    labels = clean_labels) +
  labs(
    x = 'Longitude', y = 'Latitude',
    title = 'New York City Covid Cases in 2021',
    caption = 'Data Source: The City of New York'
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0.5,    
      color = "grey60",   
      size = 8,           
      margin = margin(t = 5) 
    )
  ) 


breaks_j_pop <- classIntervals(c(min(covid_data$elderlyPop) - .00001,
                                 covid_data$elderlyPop), n = 5, style = "jenks")

covid_data <- covid_data %>% 
  mutate(elderlyPop_cut = cut(elderlyPop, breaks_j_pop$brks, 
                              include.lowest = TRUE, dig.lab = 4))



clean_labels_pop <- sapply(1:(length(breaks_j_pop$brks) - 1), function(i) {
  paste0(
    format(breaks_j_pop$brks[i], big.mark = ",", scientific = FALSE, trim = TRUE),
    " – ",
    format(breaks_j_pop$brks[i + 1], big.mark = ",", scientific = FALSE, trim = TRUE))
}
)

ggplot(covid_data) + 
  geom_sf(aes(fill = elderlyPop_cut)) +
  scale_fill_brewer(palette = "OrRd", 
                    name = 'Elderly Population',
                    labels = clean_labels_pop) +
  labs(
    x = 'Longitude', y = 'Latitude',
    title = 'Elderly Population in New York City',
    caption = 'Data Source: American Community Survey'
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0.5,    
      color = "grey60",   
      size = 8,           
      margin = margin(t = 5) 
    )
  ) 


g1 <- ggplot(covid_data) + 
  geom_sf(aes(fill = covid_data_cut), show.legend = TRUE) +
  scale_fill_brewer(palette = "OrRd", name = 'Covid Cases per Zipcode', labels = clean_labels) +
  coord_sf(
    xlim = c(-74.26, -73.70),
    ylim = c(40.48, 40.92),
    default_crs = sf::st_crs(4326)
  ) + 
  geom_sf_label(data = covid_data %>% dplyr::filter(COVID_CASE_COUNT > 10000),
                aes(label = COVID_CASE_COUNT),
                label.size = .07,
                size = 2) +
  labs(
    x = 'Longitude', 
    y = 'Latitude', 
    title = 'New York City Covid Cases in 2021',
    caption = 'Data Source: The City of New York'
  ) +
  theme(
    legend.position = "bottom",  
    legend.justification = "center",
    legend.box.just = "center",  
    legend.title = element_text(hjust = 1), 
    plot.caption = element_text(
      hjust = 0,    
      color = "grey60",   
      size = 8,           
      margin = margin(t = 5) 
    )) + 
  guides(fill = guide_legend(
    title.position = "top", 
  )) 


g2 <- ggplot(nys_health_sf) + 
  geom_sf(colour='blue', size=0.1) +
  scale_fill_brewer(palette = "OrRd", name='Noise Complaints') +
  coord_sf(xlim = c(-74.26, -73.70),
           ylim = c(40.48, 40.92), default_crs = sf::st_crs(4326))  +
  labs(x='Longitude', y='Latitude', 
       title='NYC Health Facilities')

g1 + g2 + plot_layout(ncol = 2, nrow = 1)


covid_data <- st_transform(covid_data, 4326)

pal_fun <- colorQuantile("Purples", NULL, n = 5)

p_popup <- paste0(
  "<strong>Covid Cases: </strong>", covid_data$COVID_CASE_COUNT, 
  "<br/>",
  "<strong>Covid Deaths: </strong>", covid_data$COVID_DEATH_COUNT
)

polyHighlightOption <- leaflet::highlightOptions(opacity = 0.8, fillColor = 'grey')
polyLabelOption <- leaflet::labelOptions(opacity = 0.6)

htmlMap <- leaflet(covid_data) %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addPolygons(
    stroke = FALSE,
    fillColor = ~pal_fun(COVID_CASE_COUNT),
    fillOpacity = 0.8,
    group = "Covid Cases",
    smoothFactor = 0.5,
    popup = p_popup,
    highlightOptions = polyHighlightOption,
    labelOptions = polyLabelOption
  ) %>%
  addCircleMarkers(radius = 0.03, weight = 1,
                   data = nys_health_sf %>% 
                     sf::st_set_crs(4326) %>% sf::st_cast("POINT"),
                   group = "Health Facilities")%>%
  addLegend(
    "bottomright",
    colors = brewer.pal(7, "Purples"),
    labels = paste0("up to ", format(breaks_j$brks[-1], digits = 1)),
    title = "Covid Cases Per Zipcode"
  ) %>%
  addLayersControl(baseGroups = c("OSM", "Carto"), 
                   overlayGroups = c("Covid Cases", "Health Facilities")) %>%
  hideGroup(c("Health Facilities")) %>%
  setView(lng = -73.93, lat = 40.73, zoom = 10)

htmlMap

htmlwidgets::saveWidget(htmlMap, 
                        file = "nyc_covid_map.html",
                        selfcontained = TRUE)

