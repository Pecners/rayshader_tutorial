# Load packages we'll use ----------

library(elevatr)
library(sf)
library(rayshader)
library(MetBrewer)
library(magick)
library(ggplot2)
library(glue)
library(scales)
library(fontawesome)
library(grid)
library(svgparser)

# Load and prepare data ----------

# Read in GCNP boundaries
data <- st_read("data/grca_tracts/GRCA_boundary.shp")

# Query for elevation data, clipping with the GCNP boundaries
gcnp_elev <- get_elev_raster(data, z = 10, clip = "location")

# Convert the returned raster to a matrix
mat <- raster_to_matrix(zelev)

# Initial rendering using defaults ----------
# Simple rendering in 3D, using all default settings
mat %>%
  height_shade() %>%
  plot_3d(heightmap = mat)

# Always close the window before you render again
rgl::rgl.close()

# Sometimes it's helpful to have a small version of your data
# which can be achieved with this commented-out code.

#small <- rayshader::resize_matrix(hm, .25)

# Set up aspect ratio and color palette ----------

# Calculate aspect ratio based on the size of our data
w <- nrow(mat)
h <- ncol(mat)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}

# Set color palette
pal <- "Demuth"
colors <- met.brewer(pal)


# Create 3D object ----------

# I put this before my `plot_3d()` calls so that I always remember
# to close an open window before trying to re-render
rgl::rgl.close()

# Render in 3D
mat %>%
  # Set your color palette
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(heightmap = mat, 
          solid = FALSE, # remove solid base
          zscale = 10, # set the scale for height, based on resolution
          windowsize = c(800*wr,800*hr), # set width and heigh based on aspect
          phi = 90, # azimuth, i.e. your angle of view
          zoom = .7, # zoom, obvi
          theta = 0) # angle rotation of scene (as in lazy susan)

# Create high-res graphic from our scene ----------

# This is what will actually render our high-res png
# also what could take a long time depending on size of data and
# power of your machine. 

render_highquality(
  # MAKE SURE THIS FILENAME IS CORRECT, PATH IS GOOD BEFORE RENDERING
  "plots/gcnp_highres.png", 
  parallel = TRUE, # use parallel processing
  samples = 300, # honestly don't know if this has an impact
  light = FALSE, # turn off artificial lighting because we'll use env. light
  interactive = FALSE, # don't accidentally screw with the scene
  environment_light = "env/phalzer_forest_01_4k.hdr", # env. light
  intensity_env = 1.5, # up the env. light intensity from 1 to 1.5
  rotate_env = 180, # rotate for desired shadow
  width = round(6000 * wr), 
  height = round(6000 * hr)
)


# Add Annotations ----------

# Set text color
text_color <- colors[1]

# Read in image, DO NOT OVERWRITE ORIGINAL
img <- image_read("plots/gcnp_highres.png")

# Title 'A PORTRAIT OF'
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = text_color, size = 125, gravity = "north",
                       location = "+0+200")
# Title NAME
img_ <- image_annotate(img_, "Grand Canyon National Park", weight = 700, 
                       font = "Cinzel Decorative", location = "+0+400",
                       color = text_color, size = 200, gravity = "north")
  
# Used for blog post
#image_write(img_, "plots/added_titles.png")

# Sub-title annotation ----------

# Area in square miles
area <- as.numeric(st_area(data)) / 2.59e6

# Elevation range in ft, converted from meters
elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

# Area
img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"),
                       font = "Cinzel Decorative", location = "+1200-1000",
                       color = text_color, size = 110, gravity = "west")

# Elevation range
img_ <- image_annotate(img_, glue("Elevation Range: {label_comma()(round(elev_range))} ft"),
                       font = "Cinzel Decorative", location = "+1200-1300",
                       color = text_color, size = 110, gravity = "west")

# Used for blog post
#image_write(img_, "plots/added_area_elev.png")

# Inset map ----------

# Make inset map
states <- spData::us_states 

# Create circle polygon centered on center of our park
spot <- st_buffer(st_centroid(data), 100000)

loc_plot <- ggplot() + 
  geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
  geom_sf(data = spot, fill = NA, color = colors[2]) +
  theme_void() + 
  coord_sf(crs = 3347)

# Save plot
ggsave(loc_plot, filename = glue("plots/gcnp_inset.png"), w = 4*1.5, h = 3*1.5)

# Read plot back in so we can manipulate it
inset <- image_read(glue("plots/gcnp_inset.png"))

# Scale plot
new_inset <- image_scale(inset, "x1000")

# Add plot to annotated graphic
img_ <- image_composite(img_, new_inset, gravity = "east",
                        offset = "+1200-1000")

# Used for blog post
#image_write(img_, "plots/with_inset.png")

# Caption ----------

# Caption, leave space for Twitter icon
img_ <- image_annotate(img_, glue("Graphic by Spencer Schien (     @MrPecners) | ", 
                                  "Data from AWS Terrain Tiles and USGS"), 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")




# Create Twitter icon -- there has to be a better way to do this
# Read SVG
twitter <- fa("twitter", fill = text_color, fill_opacity = .5)

# Plot SVG and save as PNG
grid.newpage()
tmp <- tempfile()
png(tmp, bg = "transparent")
grid.draw(read_svg(twitter))
dev.off()

# Read Twitter icon back in and scale it
tw <- image_read(tmp)
tw <- image_scale(tw, "x75")

# Add Twitter icon to annotated image
img_ <- image_composite(img_, tw, gravity = "south",
                              offset = "-530+65")


# Save our annotated image -- DO NOT OVERWRITE THE ORIGINAL
image_write(img_, glue("plots/gcnp_fully_annotated.png"))

