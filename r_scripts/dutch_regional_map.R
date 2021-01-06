### Script for making a map visual of Dutch regions by using sf and ggplot2
# Feel free to improve this script. Bonus points if you manage to add nice geographical features such as hillshade, rivers, etc. without the map looking a mess.
library(tidyverse)
library(magrittr)
library(sf)
library(httr)
library(tmaptools)
library(zoo)
library(glue)
library(cowplot)
library(extrafont)

### First, get a map of Dutch provinces from the web by using WFS
wfs_pdok <- "https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs"
url_provkaart <- parse_url(wfs_pdok)

url_provkaart$query <- list(service = "wfs",
                  version = '2.0.0',
                  request = "GetFeature",
                  typeName = 'cbsgebiedsindelingen:cbs_provincie_2020_gegeneraliseerd',
                  outputFormat = "application/json")
                  
request_provkaart <- build_url(url_provkaart)
prov_kaart <- st_read(request_provkaart)

### The geometry and province names are the only relevant fields for this visualisation. We'll keep only the 'statnaam' column for this purpose. The geometry column
# is saved by default because it is an sf object
prov_kaart %<>% select(statnaam)

### We'll now get the regional map in similar fashion
wfs_cultgis <- 'http://services.rce.geovoorziening.nl/cultgis/wfs'
url_regions <- parse_url(wfs_cultgis)
url_regions$query <- list(service = "WFS",
                  version = '2.0.0',
                  request = "GetFeature",
                  typeName = 'cultgis:regios',
                  outputFormat = "application/json")
request_regiokaart <- build_url(url_regions)
regio_kaart <- st_read(request_regiokaart)

### Our regio_kaart object has 92 rows, while there should be 82 different regions. This difference is due to the fact that 10 regions which are part of multiple provinces
# have a suffix between brackets indicating this. Because of the addition to the regional name, the polygons are saved as separate units.
# We're getting rid of the suffix now and will collapse the separate polygons into the 82 regions
regio_kaart$naam[grep('[()]', regio_kaart$naam)] %<>% word(start = 1, end = -2)

regio_kaart %<>%
  dplyr::select(naam) %>% 
  group_by(naam) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

### When we plot the regional map 'regio_kaart', we find that the regional polygons still contain water surfaces
regio_kaart %>%
  select(naam) %>%
  st_simplify() %>%
  plot()

### To get rid of the surfaces, we'll use the outline of the prov_kaart object, which does contain the correct water boundaries
# We're intersecting these 'contours' of the Netherlands with the regio_kaart object, trimming the edges of this object (like pressing a cookie cutter in some dough)
nl_contours <- prov_kaart %>%
  summarise(geometry = sf::st_union(geometry))

regio_kaart %<>% st_intersection(nl_contours)

###### This is where the ugly stuff starts #####

### To prevent problems later on with overlapping polygon labels (the regional names), we'll have to make a fair few changes to the names
# Some are pretty general, like starting a new line after every 'en' or ','
# Others are super specific, regarding some regional names that have shown to be clashing with others
# When changing things such as font size of the labels later on, or the scale of the map, you'll find that you'll have to do some additional tweaking over here.
# The map is never perfect at your first try

regio_kaart$naam %<>% str_replace_all('Bommelerwaard', 'Bommeler-\nwaard')
regio_kaart$naam %<>% str_replace_all('Kennemerland', 'Kennemer-\nland')
regio_kaart$naam %<>% str_replace_all('Waterland en Zaanstreek', 'Water-\nland en Zaanstreek')
regio_kaart$naam %<>% str_replace_all('Lauwersmeer', 'Lauwers-\nmeer')
regio_kaart$naam %<>% str_replace_all('Noordoostpolder en Urk', 'Noordoost-\npolder en Urk')
regio_kaart$naam %<>% str_replace_all(' en ', ' en\n')
regio_kaart$naam %<>% str_replace_all(', ', ',\n')
regio_kaart$naam %<>% str_replace_all('van ', 'van\n')

### We'll now make a visualisation of the regional map on the national level (AKA fully zoomed out).
# The coloring of the polygons is done by using an alorithm which minimises the amount of different colors needed while maintaining that adjacent polygons can't share a
# color. The algorithm ends up at around 8 colours, but you can further reduce it by specifying the argument minimize = TRUE. Because of randomness in the algorithm,
# the colors end up different every time!

### If you just want the map without making any animation, you can just end here. Uncomment the geom_sf_text layer, pick a nice font and play around with the color
# palette a bit

basis_regio_plot <- regio_kaart %>%
  mutate(groep = map_coloring(.)) %>%
  ggplot(aes(color = factor(groep), fill = factor(groep))) +
  geom_sf(show.legend = F) +
  # geom_sf_text(aes(label = naam), col = 'black', size = 3) +
  geom_sf(aes(),prov_kaart, color = 'black', fill = NA) +
  scale_fill_brewer(palette = 'Paired') +
  scale_color_brewer(palette = 'Paired') +
  theme_void()

basis_regio_plot

#### This is where the movie part starts #####

# The idea is to zoom in on every separate province. This allows for the regional labels to be read properly.
# In order to zoom in, we need the so called 'bounding box' of every provincial polygon. They define the x and y extents of the frame
prov_bboxes <- data.frame(naam = character(),
                          xmin = double(),
                          ymin = double(),
                          xmax = double(),
                          ymax = double())

for(i in 1:12){
  
  provincie <- prov_kaart[i,'statnaam']
  bbox <- provincie %>% st_bbox()
  prov_bboxes[i,] <- c(provincie$statnaam, bbox)
}

prov_bboxes %<>% 
  mutate_at(vars(-naam), as.numeric)

# We now have all the unique bounding boxes by province but want to make sure there's a somewhat smooth transition between these frames.
# the incredibly ugly function below takes the province names in a certain order, uses their bounding boxes and defines transitional bounding boxes between them
# The first and last bounding box of the sequence is the bounding box of the Netherlands as a whole, to ensure the zooming in and out effect in the video
# The interpolation between frames is linear as of now, but it could perhaps give a smoother effect if we would interpolate exponentially

make_frames <- function(order_of_provinces, n_frames_transition, n_frames_still){
  
  bbox_frames <- data.frame(xmin = double(),
                            ymin = double(),
                            xmax = double(),
                            ymax = double())
  
  bbox_frames[1, ] <- regio_kaart %>% st_bbox()
  
  for(province in order_of_provinces){
    
    target_index_transition <- nrow(bbox_frames) + n_frames_transition
    target_index_still <- target_index_transition + n_frames_still
    
    temp_bbox <- prov_bboxes %>% 
      filter(naam == province) %>% 
      dplyr::select(-naam) %>% 
      unlist()
    
    bbox_frames[target_index_transition,] <- temp_bbox
    bbox_frames[target_index_still,] <- temp_bbox
    
  }
  
  bbox_frames[nrow(bbox_frames) + n_frames_transition,] <- regio_kaart %>% st_bbox()
  bbox_frames %<>% na.approx()
  
  return(bbox_frames)
}

# Before using the function, we will define the order of the provinces that will be shown. This is totally arbitrary
# Additionaly, we'll define how many frames the video will stay at one province before transitioning into the next. Again, this is a matter of taste plus trial and error
order_prov <- c('Limburg', 'Noord-Brabant','Zeeland', 'Zuid-Holland', 'Noord-Holland', 'Flevoland', 'Utrecht', 'Gelderland', 'Overijssel', 'Drenthe', 'Groningen', 'Friesland')
nr_of_transitional_frames <- 20
nr_of_still_frames <- 30

# Apply the function and save the result. We will loop through frames_animation, generating and saving pictures with its input
frames_animation <- make_frames(order_prov, nr_of_transitional_frames, nr_of_still_frames)

# First, define the font you want to use for the labels if you haven't already. I went with the 'Wt Cen WT' fonts because it is one of the rare R fonts that isn't ugly
# Furthermore, it has a kind of classic vibe that reminds me of old school maps. It's from the extrafonts package. I think you'll need to download all fonts the first
# time you use it
lettertype = 'Tw Cen MT'

# Set the working directory where you want to save the pictures. You should make a new folder for this. It'll be a mess otherwise
setwd('C:\\Users\\Path\\Pictures')

# The loop below that will generate the frames has one major inefficiency: rendering new frames when the 'camera' isn't moving. Ideally, when we're pausing at a province
# for a couple of seconds, we should just copy the previous frame instead of rendering a new picture of the very same frame. This will save a lot of time, but I'm too
# tired to fix it

for(i in 1:nrow(frames_animation)){
  regio_plot <- basis_regio_plot +
    coord_sf(xlim = frames_animation[i,c(1, 3)],
             ylim = frames_animation[i,c(2, 4)])
  
  # We'll only use the labels when we're zoomed in. We're reusing our n_transitional frames variable to help us determine whether we're zoomed in
  if(i > nr_of_transitional_frames & i <= (nrow(frames_animation) - nr_of_transitional_frames)){
    # You can play around with font size, as well as interesting arguments such as check_overlap (that doesn't really do what it says on the box)
    regio_plot <- regio_plot + geom_sf_text(aes(label = naam),
                                            col = 'black',
                                            fontface = 'bold',
                                            family = lettertype,
                                            lineheight = .5,
                                            show.legend = F,
                                            check_overlap = F, 
                                            size = 5.5)
  }
  
  # The title and caption will be added to the picture using the amazing cowplot package. GGplot2 has a title function as well, but that title tends to shift a bit
  # when the visual changes. Cowplot assures the title and caption are rock solid and will stay in place
  ggdraw() + 
    draw_plot(regio_plot, y = 0.025, height = .9) +
    draw_label('Author & Sourcel',
               y = 0.015, 
               fontfamily = lettertype) +
    draw_label("Title",
               fontface = 'bold', 
               y = .975, 
               fontfamily = lettertype,
               size = 30)
  
  ggsave(filename = glue("figure{str_pad(i, 4, pad = '0')}.png"))
}

### When all the frames are in the directory, go to the directory in the command line and run the code below. Be sure to have installed ffmpeg first.
# you could probably use magick as well. There might even be an R package that does this properly, but I found all of them lacking (e.g. gganimate)
# ffmpeg -framerate 10 -i figure%04d.png output.mp4
