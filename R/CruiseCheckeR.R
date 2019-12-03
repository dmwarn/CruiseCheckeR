#' Cruise data check via R
#'
#' @param dat.source
#'   This determines if the function should operate on local csv files or
#'   the results of database queries. If 'database', the user will be prompted
#'   to provide username, password, and dbname, which in the case of the Great
#'   Lakes Science Center, should be GLSC.
#' @param year
#' Character vector, year value in quotes. Provides information about the year, which is
#' used in maming output files.
#' @param lake
#' Character vector. For example, "Michigan". Provides information about
#' location which is used in naming output files.
#' @param stype
#' Numeric vector representing the type of sample data to be evaluated. For example,
#' SAMPLE_TYPE (stype) = 1 for trawl data. Currently only functional for stype = 1.
#' @param target
#' Numeric vector representing the study or study type. For example, target = c(209) would
#' return data collected during the Lake Michigan LWAP fall acoustic survey.
#' @param username
#'   Character vector. This is the username required to access the database, if required.
#' @param password
#'   Password for database access.
#' @param dbname
#'   Name of the data source. If using ROracle, it should be GLSC.
#'
#' @return
#'   Generates an html file with various plots and checks of data consistency,
#'   accuracy, and completeness.
#' @importFrom dbplyr in_schema
#' @import utils tcltk RODBC DBI dplyr
#' @export
#'
#' @details
#'  The purpose of this function is to create tables and plots to help the user
#'  evaluate the consistency, completeness, and accuracy of data that are either
#'  in an Oracle database or are being prepared for appending to an Oracle
#'  database. Currently the functionality is limited to data and data structures
#'  that match those of the Research Vessel Catch database developed by the USGS
#'  Great Lakes Science Center, which is used for storing data collected during
#'  a wide variety of both field and laboratory sampling exercises. More specifically,
#'  the current iteration ONLY works with trawl data. Future developments could add
#'  other sample types.
CruiseCheckeR <- function(dat.source = "csv", year = 2018, lake = 2,
              vessel = 17, cruise = c(1, 2, 3, 4, 5, 6, 7), target = 1,
              stype = 1, username = "", password = "", dbname = "") {
  assign("year", year , envir = .GlobalEnv )
  assign("lake", lake , envir = .GlobalEnv )
  assign("vessel", vessel , envir = .GlobalEnv )
  assign("cruise", cruise, envir = .GlobalEnv )
  assign("target", target , envir = .GlobalEnv )

  if (dat.source == "database") {
    ##############################

    xvar <- tclVar("")
    yvar <- tclVar("")
    zvar <- tclVar("")
    tt <- tktoplevel()
    tkwm.title(tt,"Enter Database Login Credentials")
    x.entry <- tkentry(tt, textvariable=xvar)
    y.entry <- tkentry(tt, textvariable=yvar)
    z.entry <- tkentry(tt, textvariable=zvar)

    reset <- function()
    {
      tclvalue(xvar)<-""
      tclvalue(yvar)<-""
      tclvalue(zvar)<-""
    }

    reset.but <- tkbutton(tt, text="Reset", command=reset)

    submit <- function() {
      x <- (tclvalue(xvar))
      y <- as.character(tclvalue(yvar))
      z <- as.character(tclvalue(zvar))
      e <- parent.env(environment())
      e$x <- x
      e$y <- y
      e$z <- z
      tkdestroy(tt)
    }
    submit.but <- tkbutton(tt, text="submit", command=submit)

    tkgrid(tklabel(tt,text="Enter Database Login Information"),columnspan=2)
    tkgrid(tklabel(tt,text="Username"), x.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt,text="Password"), y.entry, pady = 10, padx =10)
    tkgrid(tklabel(tt,text="Database"), z.entry, pady = 10, padx =10)

    tkgrid(submit.but, reset.but)

    tkwait.window(tt)
    usname <- x
    pwrd <- y
    dbase <- z

    drv <- dbDriver("Oracle")
    #con <- dbConnect(drv, uid = usname,
    #                 pwd = pwrd, default = "", gui = .GUI,
    #                 dbname = dbase)
    #con <- odbcConnect(dbase, uid=usname, pwd= pwrd, believeNRows=FALSE)
    conn <- dbConnect(drv = drv, dbname = dbase, user = usname,
                      password = pwrd)
    #############################
    #############################
    assign('op' <- left_join(x = dplyr::tbl(conn, dbplyr::in_schema("RVCAT", "op")) %>%
          dplyr::filter(YEAR == year & LAKE == lake & SAMPLE_TYPE ==stype &
                          VESSEL == vessel & CRUISE == cruise),
          y = dplyr::tbl(conn, dbplyr::in_schema("RVCAT", "op_target")), by = "OP_ID") %>%
      dplyr::filter(TARGET %in% target) %>%
      dplyr::collect(), envir = .GlobalEnv)
    #  create vector of OP_ID to use to subset subsequent tables.
    assign('opid', op$OP_ID, envir = .GlobalEnv)

    if (stype == 1)
      # Select rows from tr_op that have OP_ID in op
      assign('tr_op', dplyr::tbl(conn, dbplyr::in_schema("RVCAT", "tr_op")) %>%
      dplyr::filter(OP_ID %in% opid) %>%
      collect(), envir = .GlobalEnv)

    # Select rows from tr_catch that have OP_ID in op
    assign('tr_catch', dplyr::tbl(conn, dbplyr::in_schema("RVCAT", "tr_catch")) %>%
      dplyr::filter(OP_ID %in% opid) %>%
      collect(), envir = .GlobalEnv)

    # Select rows from tr_l that have OP_ID in op
    assign('tr_l', dplyr::tbl(conn, dbplyr::in_schema("RVCAT", "tr_l")) %>%
      dplyr::filter(OP_ID %in% opid) %>%
      collect(), envir = .GlobalEnv)

    # Select rows from tr_lf that have OP_ID in op
    assign('tr_lf', dplyr::tbl(conn, dbplyr::in_schema("RVCAT", "tr_lf")) %>%
      dplyr::filter(OP_ID %in% opid) %>%
      collect(), envir = .GlobalEnv)

    # Select rows from tr_op that have OP_ID in op
    assign('tr_fish', dplyr::tbl(conn, dbplyr::in_schema("RVCAT", "tr_fish")) %>%
      dplyr::filter(OP_ID %in% opid) %>%
      collect(), envir = .GlobalEnv)
  } else {

    thedir <- tclvalue(tkchooseDirectory())
    assign("dir", dir , envir = .GlobalEnv )

    f.list <-
      list.files(thedir, pattern = "op|tr_op|tr_catch|tr_lf|tr_l|tr_fish", full.names = TRUE)
    filelist <- lapply(f.list, read.csv)
    names(filelist) <- gsub(pattern = ".csv",
                            "",
                            basename(f.list),
                            fixed = TRUE)
    list2env(filelist, envir = .GlobalEnv)

  }
  pth <- system.file("rmd", "Cruisecheck.Rmd", package = "CruiseCheckeR")
  rmarkdown::render(pth, output_format = "html_document",
                    output_file = paste0(thedir, "/", "Year", "-", year,
                "-", "Lake", "-", lake, "-", "Vessel", "-",vessel, "-",
                "Cruise", "-", cruise, "-",
                "Target", "-", target, "-", "Cruisecheck", "-", Sys.Date()))
}


