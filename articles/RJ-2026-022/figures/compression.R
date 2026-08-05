oldest = imager::load.image("figures/walz_oldest_article.png")
d = dim(oldest)[1:2]
d

scale = min(d / 600)
img = imager::resize(oldest, 
                     imager::width(oldest) / scale, 
                     imager::height(oldest) / scale,
                     interpolation_type = 6)
img

imager::save.image(img, file = "figures/walz_oldest_compressed.png")
