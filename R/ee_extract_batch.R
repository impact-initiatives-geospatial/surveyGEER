# ee_extract_batch <- (x,y,stat="mean",scale,via="getInfo",container="rgee_backup",
#                      sf=TRUE,
#                      lazy=FALSE,
#                      quiet=FALSE,
#                      rgee_issue_fixed=FALSE,
#                      ...){}
#   if(nrow(y)<=5000){
#   res <- x |>
#     ee_extract_tidy(y = y,stat = stat,scale = scale,via = via)
#   }
#
#   if(nrow(y)>5000){
#     y_split <- y |>
#       dplyr::ungroup() |>
#       dpyr::mutate(
#         id=row_number(),
#         splitter= id %in% (1:(y |> nrow())/2)
#       ) |>
#       group_split(splitter)
#
#     res_list <- list()
#     for(i in seq_along(y_split)){
#       temp_y <- y_split[[i]]
#       res_list[[i]] <- x |>
#         ee_extract_tidy(y = temp_y,stat = stat,scale = scale,via = via)
#     }
#     res <-  bind_rows(res_list)
#   }
#   return(res)
#
# }
# #
# #
# #     }
# #     cat("extraction complete, binding outputs")
# #     grid_with_ndvi_indicators <- bind_rows(extract_grid_list)
# #   }
# # }
