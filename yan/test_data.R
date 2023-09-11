#' @name 04_newdata.R
#'
#'

library(dplyr)


# Let's write a function
# that estimates realistic values for vmt vehicles statrs and sourcehours when vmt or vehicles changes.
new = function(.year = 2020, .vmt =43647,  .vehicles = 10, .mph = 13){
  tribble(
    ~year,  ~vmt,                ~vehicles,   ~sourcehours,       ~starts,

    .year,   .vmt * .vehicles,   .vehicles,   .vehicles*.vmt/.mph,  2000*.vehicles

  )
}



d1 = bind_rows(
  new(.year = 2020, .vehicles = 10),
  new(.year = 2025, .vehicles = 15),
  new(.year = 2030, .vehicles = 20),
  new(.year = 2040, .vehicles = 25),
  new(.year = 2050, .vehicles = 30)
)

d1 = bind_rows(
  new(.year = 2020, .vehicles = 10),
  new(.year = 2025, .vehicles = 9),
  new(.year = 2030, .vehicles = 8),
  new(.year = 2040, .vehicles = 7),
  new(.year = 2050, .vehicles = 6)
)

myvmt = 43647
d2 = bind_rows(
  new(.year = 2020, .vmt = myvmt),
  new(.year = 2025, .vmt = myvmt - 1000),
  new(.year = 2030, .vmt = myvmt - 2000),
  new(.year = 2040, .vmt = myvmt - 3000),
  new(.year = 2050, .vmt = myvmt - 4000)
)
