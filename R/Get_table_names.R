#'  Get Database Table Field Names
#'
#' @param username
#'  Character string providing the username required to access the database, if required.
#' @param password
#'  Character string providing password for database access.
#' @param dbname
#'  Character string providing the name of the data source. If using ROracle, it should be GLSC.
#' @param sample_type
#' Character string naming the sample type
#' @return
#'  This is a helper function that gets the names of the fields in tables used
#'  for trawl data that are in the database. The field names in csv input
#'  files are compared against these names.
#' @import RODBC
#' @export
#'
#'@details
#'  This script is a helper function. In its current state, all it does
#'  is generate Rdata data sets that contain the database field names.
#'
#'
Get_table_names <- function(username , password , dbname , sample_type ) {
  # use the credentials to log in to database to get
  # field names for the tables associated with trawl data
  # use the credentials to log in to database to get
  # field names for the tables associated with trawl data
  drv <- dbDriver("Oracle")
  con <- dbConnect(drv, username = "dmwarner",
                   password = "GLSC1908", default = NULL, gui = .GUI,
                   dbname = "GLSC")
  # Get op table field names
  # The query, which selects all fields, row zero,
  # which happens to be the field names.
  op_names <-dbGetQuery(con, "select  *
    from    rvcat.op
                WHERE ROWNUM = 0")
  # Save the field names to an R data set.
  save(op_names, file="./data/op_names.rda")

  # Now do sample_type specific queries to get field names
  # and save the field names to R data files.
  if(sample_type == "Trawl") {
  tr_op_names <- dbGetQuery(con, "select  *
    from    rvcat.tr_op
                      WHERE ROWNUM = 0")
  save(tr_op_names, file="./data/tr_op_names.rda")

  tr_catch_names <- dbGetQuery(con, "select  *
    from    rvcat.tr_catch
    WHERE ROWNUM = 0")
  save(tr_catch_names, file="./data/tr_catch_names.rda")

  tr_l_names <- dbGetQuery(con, "select  *
    from    rvcat.tr_l
    WHERE ROWNUM = 0")
  save(tr_l_names, file="./data/tr_l_names.rda")

  tr_lf_names <- dbGetQuery(con, "select  *
    from    rvcat.tr_lf
    WHERE ROWNUM = 0")
  save(tr_lf_names, file="./data/tr_lf_names.rda")

  tr_fish_names <- dbGetQuery(con, "select  *
    from    rvcat.tr_fish
    WHERE ROWNUM = 0")
  save(tr_fish_names, file="./data/tr_fish_names.rda")


  } else if (sample_type == "Mysis") {
  mysis_op_names <- dbGetQuery(con, "select  *
    from    rvcat.mysis_op
    WHERE ROWNUM = 0")
  save(mysis_op_names, file="./data/mysis_op_names.rda")

  mysis_catch_names <- dbGetQuery(con, "select  *
    from    rvcat.mysis_catch
    WHERE ROWNUM = 0")
  save(mysis_catch_names, file="./data/mysis_catch_names.rda")

  mysis_ind_names <- dbGetQuery(con, "select  *
    from    rvcat.mysis_ind
    WHERE ROWNUM = 0")
  save(mysis_catch_names, file="./data/mysis_ind_names.rda")


  } else if (sample_type == "Zooplankton") {
  zp_op_names <- dbGetQuery(con, "select  *
    from    rvcat.zp_op
    WHERE ROWNUM = 0")
  save(zp_op_names, file="./data/zp_op_names.rda")

  zp_sample_names <- dbGetQuery(con, "select  *
    from    rvcat.zp_sample
    WHERE ROWNUM = 0")
  save(zp_sample_names, file="./data/zp_sample_names.rda")

  zp_subsample_names <- dbGetQuery(con, "select  *
    from    rvcat.subsample
    WHERE ROWNUM = 0")
  save(zp_subsample_names, file="./data/zp_subsample_names.rda")

  zp_species_names <- dbGetQuery(con, "select  *
    from    rvcat.zp_species_total
    WHERE ROWNUM = 0")
  save(zp_species_names, file="./data/zp_species_names.rda")



  } else if (sample_type == "Ichthyoplankton") {
  ip_op_names <- dbGetQuery(con, "select  *
    from    rvcat.ip_op
    WHERE ROWNUM = 0")
  save(ip_op_names, file="./data/ip_op_names.rda")

  ip_catch_names <- dbGetQuery(con, "select  *
    from    rvcat.ip_catch
    WHERE ROWNUM = 0")
  save(ip_catch_names, file="./data/ip_catch_names.rda")

  ip_l_names <- dbGetQuery(con, "select  *
    from    rvcat.ip_l
    WHERE ROWNUM = 0")
  save(ip_l_names, file="./data/ip_l_names.rda")

  ip_fish_names <- dbGetQuery(con, "select  *
    from    rvcat.ip_fish
    WHERE ROWNUM = 0")
  save(ip_fish_names, file="./data/ip_fish_names.rda")

  ip_diet_names <- dbGetQuery(con, "select  *
    from    rvcat.ip_diet
    WHERE ROWNUM = 0")
  save(ip_diet_names, file="./data/ip_diet_names.rda")

  ip_diet_l_names <- dbGetQuery(con, "select  *
    from    rvcat.ip_diet_l
    WHERE ROWNUM = 0")
  save(ip_diet_l_names, file="./data/ip_diet_l_names.rda")


  } else if (sample_type == "Benthos") {
    benthos_ponar_names <- dbGetQuery(con, "select  *
    from    rvcat.benthos_ponar
    WHERE ROWNUM = 0")
    save(benthos_ponar_names, file="./data/benthos_ponar_names.rda")

    benthos_lf_names <- dbGetQuery(con, "select  *
    from    rvcat.benthos_lf
    WHERE ROWNUM = 0")
    save(benthos_lf_names, file="./data/benthos_lf_names.rda")

    benthos_comments_names <- dbGetQuery(con, "select  *
    from    rvcat.benthos_comments
    WHERE ROWNUM = 0")
    save(benthos_comments_names, file="./data/benthos_comments_names.rda")


  } else if (sample_type == "Chlorophyll") {
  chl_op_names <- dbGetQuery(con, "select  *
    from    rvcat.chl_op
    WHERE ROWNUM = 0")
  save(chl_op_names, file="./data/chl_op_names.rda")

  chl_sample_names <- dbGetQuery(con, "select  *
    from    rvcat.chl_sample
    WHERE ROWNUM = 0")
  save(chl_sample_names, file="./data/chl_sample_names.rda")

  chl_calc_names <- dbGetQuery(con, "select  *
    from    rvcat.chl_calc
    WHERE ROWNUM = 0")
  save(chl_calc_names, file="./data/chl_calc_names.rda")



  } else if (sample_type == "Water Profile") {
  profile_op_names <- dbGetQuery(con, "select  *
    from    rvcat.profile_op
    WHERE ROWNUM = 0")
  save(profile_op_names, file="./data/profile_op_names.rda")

  profile_data_names <- dbGetQuery(con, "select  *
    from    rvcat.profile_data
    WHERE ROWNUM = 0")
  save(profile_data_names, file="./data/profile_data_names.rda")



  } else if (sample_type == "Gill Net Set" |
             "Gill Net Lift" | "Gill Net Set and Lift") {
  gn_op_names <- dbGetQuery(con, "select  *
    from    rvcat.gn_op
    WHERE ROWNUM = 0")
  save(gn_op_names, file="./data/gn_op_names.rda")

  gn_effort_names <- dbGetQuery(con, "select  *
    from    rvcat.gn_effort
    WHERE ROWNUM = 0")
  save(gn_effort_names, file="./data/gn_effort_names.rda")

  gn_catch_names <- dbGetQuery(con, "select  *
    from    rvcat.gn_catch
    WHERE ROWNUM = 0")
  save(gn_catch_names, file="./data/gn_catch_names.rda")

  gn_l_names <- dbGetQuery(con, "select  *
    from    rvcat.gn_l
    WHERE ROWNUM = 0")
  save(gn_l_names, file="./data/gn_l_names.rda")

  gn_lf_names <- dbGetQuery(con, "select  *
    from    rvcat.gn_lf
    WHERE ROWNUM = 0")
  save(gn_lf_names, file="./data/gn_lf_names.rda")

  gn_fish_names <- dbGetQuery(con, "select  *
    from    rvcat.gn_fish
    WHERE ROWNUM = 0")
  save(gn_fish_names, file="./data/gn_fish_names.rda")


  } else if (sample_type == "Hydroacoustics") {
    # write some queries like above
  }
}



