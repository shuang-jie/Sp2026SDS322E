library(tidyverse)

############ install RColorBrewer
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
library(RColorBrewer)

############  displays the a few palettes 
RColorBrewer::display.brewer.all()

############ revisit airquality data
airquality |> as_tibble()

############ Temp vs Ozone
airquality |>
  ggplot(aes(Temp, Ozone)) + 
  geom_point(size = 4) + 
  theme_bw()

############ scale_color_continuous
############ continuous data values are mapped onto the color
airquality |> 
  ggplot(aes(Temp, Ozone)) + 
  geom_point(aes(color = Solar.R), size = 4) + 
  theme_bw()

############ scale_color_gradient⁠ creates a two colour gradient (low-high),
airquality |> 
  ggplot(aes(Temp, Ozone)) + 
  geom_point(aes(color = Solar.R), size = 4) +
  scale_color_gradient(low = "blue", high = "sienna2")+ 
  theme_bw()

############ scale_color_gradient2⁠creates a diverging clour gradient (low-mid-high)
airquality |> 
  ggplot(aes(Temp, Ozone)) + 
  geom_point(aes(color = Solar.R), size = 4) +
  scale_color_gradient2(low = "black", mid = "white" , high = "red")+ 
  theme_bw()


############ meaningful midpoint in diverging scale
airquality |>
  ggplot(aes(Temp, Ozone)) +
  geom_point(aes(color = Solar.R), size = 4) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = mean(airquality$Solar.R, na.rm = TRUE)
  ) +
  theme_bw()

############ scale_color_distiller for continuous data using palette
airquality |>
  ggplot(aes(Temp, Ozone)) +
  geom_point(aes(color = Solar.R), size = 4) +
  scale_color_distiller(palette = "YlOrRd") +
  theme_bw()

############ scale_color_brewer + palette
airquality |> 
  mutate(Month = factor(Month)) |>
  ggplot(aes(Temp, Ozone)) + 
  geom_point(aes(color = Month), size = 4) + 
  scale_color_brewer(type = "qual", palette = "Dark2")+ 
  theme_bw()

############ scale_color_brewer qualitative + palette = Set 1
airquality |> 
  mutate(Month = factor(Month)) |>
  ggplot(aes(Temp, Ozone)) + 
  geom_point(aes(color = Month), size = 4) + 
  scale_color_brewer(type = "qual", palette = "Set1")+ 
  theme_bw()

############ scale_color_manual comparison
airquality |>
  mutate(Month = factor(Month)) |>
  ggplot(aes(Temp, Ozone)) +
  geom_point(aes(color = Month), size = 4) +
  scale_color_manual(
    values = c("red", "blue", "green", "purple", "orange")) +
  theme_bw()


############ WRONG: using qualitative palette for continuous variable
airquality |>
  ggplot(aes(Temp, Ozone)) +
  geom_point(aes(color = Solar.R), size = 4) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw()
# error since we use qualitative palette for continuous variable Solar.R 









