# purrr::map_vec(test, purrr::pluck, "name")
# purrr::map(issues, purrr::pluck, "reactions")
# purrr::map(noaa_fims, \(x) purrr::pluck(x, "license")$name)
# purrr::pluck(noaa_fims[[1]], "license")$name

# get_license_names <- function(get_data) {
#   get_license_name <- function(x) {
#     data <- purrr::map_chr(x, "license")[["name"]]
#     returned <- if (is.null(data)) {
#       "NA"
#     } else {
#       data
#     }
#     return(returned)
#   }
#   data <- purrr::map_dfr(
#     get_data,
#     \(x) data.frame(
#       name = purrr::map_chr(x, "name"),
#       fork = get_license_name(x)
#     ),
#     .id = "user"
#   )
#   return(data)
# }
# get_license_names(noaa_fims)

# chat <- ellmer::chat_vllm(
#   base_url = "https://llm.nrp-nautilus.io/",
#   model = "llama3",
#   api_key = "sk-NOg1F7MpA17GFe9Hl6-sqA"
# )
