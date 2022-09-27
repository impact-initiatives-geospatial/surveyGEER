
col_pt_data_clean |> st_drop_geometry() |> count(adm_uid)
pt_ee <- col_pt_data_clean |>
  mutate(new_uid,val= 1) |>
  select(val) |>
  slice(1:100) |>
  # the buffer concept is interesting.. not sure if its necessary
  #
  # reach_reproject_utm("col") |>
  # st_buffer(dist = 50) |>
  # st_transform(crs = 4326) |>
  sf_as_ee()

bbox_ee <- col_pt_data_clean |>
  mutate(new_uid,val= 1) |>
  select(val) |>
  slice(1:100) |>
  st_bbox() |>
  st_as_sfc() |>
  reach_reproject_utm("col") |>
  st_buffer(dist = 500) |>
  st_transform(crs = 4326) |>
  sf_as_ee()

ee$Image(0)
ee_help(ee$Image$resample)

pt_img = pt_ee$
  filter(ee$Filter$notNull(list("val")))$
  reduceToImage(
    properties= list('val'),
    reducer= ee$Reducer$first()
    )
Map$addLayer(pt_img,list(min=0,max=1,palette=c("white","black")),"pt_img")+
  Map$addLayer(bbox_ee)

pt_img_clip <- pt_img$clip(bbox_ee)
pt_img_clip_mask <- pt_img_clip$updateMask(pt_img_clip$eq(1))
sum_pt_img = pt_img_clip_mask$reduceRegion(
  reducer= ee$Reducer$sum(),
  geometry= bbox_ee,
  scale= 30,
  maxPixels= 1e15
  )
pt_img_clip_red = pt_img_clip$reduceResolution(
  reducer= ee$Reducer$mean(),
  maxPixels= 1024
  )$
  reproject(
    crs= pt_img_clip$projection()
  )

pt_img_clip$projection()$getInfo()

Map$addLayer(pt_img_clip_red,list(min=0,max=1,palette=c("white","black")),"pt_img")



terra <-  ee$ImageCollection("MODIS/061/MOD13Q1")$
  filterBounds(bbox_ee)
terra_img <-  terra$first()
modis_projection <-  terra_img$projection()


pt_img_clip_dfpr <- pt_img_clip$setDefaultProjection( crs=pt_img_clip$projection()$crs() )
pt_img_clip_red = pt_img_clip_dfpr$
  reproject(crs=pt_img_clip$projection())$
  reduceResolution(
  # reducer= ee$Reducer$mean(),
  reducer= ee$Reducer$mean(),
  maxPixels= 1024)$
  reproject(
    crs= modis_projection
  )
Map$addLayer(pt_img_clip_red,list(min=0,max=1,palette=c("white","black")),"pt_img")

  # raster::rasterize(x = col_pt_data_clean)
r <- terra::rast(xmin=0, ncols=18, nrows=18)
r<- raster::raster(res=30)
terra::rasterize(x = col_pt_data_clean,y = r)
terra::rasterize(x = st_geometry(col_pt_data_clean),y=r)


ext <- raster::extent(-180, 180, -65, 75)
r <- raster::raster(ext, res = 1/120)

system.time(rr <- raster::rasterize(col_pt_data_clean, r, value=1, background = 0))
  # generate points
set.seed(1)
p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)
