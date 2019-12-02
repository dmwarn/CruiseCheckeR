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
#' @param username
#'   Character vector. This is the username required to access the database, if required.
#' @param password
#'   Password for database access.
#' @param dbname
#'   Name of the data source. If using ROracle, it should be GLSC.
#'
#'
#' @return
#'   Generates an html file with various plots and checks of data consistency,
#'   accuracy, and completeness.
#'
#' @import utils tcltk ROracle DBI
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
CruiseCheckeR <- function(dat.source = "csv", year = "2019", lake = "Michigan",
                          username = "", password = "", dbname = "") {
  assign("year", year , envir = .GlobalEnv )
  assign("lake", lake , envir = .GlobalEnv )

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
    con <- dbConnect(drv, username = usname,
                     password = pwrd, default = "", gui = .GUI,
                     dbname = dbase)
    #############################
    #############################
    assign("op", dbGetQuery(con, "select  *
    from    choose, rvcat.op
    where   op_id = op"), envir = .GlobalEnv)
    #oper <- dbGetQuery(con, "select  *
    #from    choose, rvcat.op
    #where   op_id = op")
    #assign("op", oper, envir = .GlobalEnv)

    assign("tr_op", dbGetQuery(con, "select  *
    from    choose, rvcat.tr_op
    where   op_id = op"), envir = .GlobalEnv)

    assign("tr_catch", dbGetQuery(con, "select  *
    from    choose, rvcat.tr_catch
    where   op_id = op"), envir = .GlobalEnv)

    assign("tr_l", dbGetQuery(con, "select  *
    from    choose, rvcat.tr_l
    where   op_id = op"), envir = .GlobalEnv)

    assign("tr_lf", dbGetQuery(con, "select  *
    from    choose, rvcat.tr_lf
    where   op_id = op"), envir = .GlobalEnv)

    assign("tr_fish", dbGetQuery(con, "select  *
    from    choose, rvcat.tr_fish
    where   op_id = op"), envir = .GlobalEnv)
  } else {

    dir <- tclvalue(tkchooseDirectory())
    assign("dir", dir , envir = .GlobalEnv )

    f.list <-
      list.files(dir, pattern = "op|tr_op|tr_catch|tr_lf|tr_l|tr_fish", full.names = TRUE)
    filelist <- lapply(f.list, read.csv)
    names(filelist) <- gsub(pattern = ".csv",
                            "",
                            basename(f.list),
                            fixed = TRUE)
    list2env(filelist, envir = .GlobalEnv)

  }
  pth <- system.file("rmd", "Cruisecheck.Rmd", package = "CruiseCheckeR")
  rmarkdown::render(pth, output_format = "html_document",
                    output_file = paste0(dir, "/", lake, " -", year, " - ", "Cruisecheck-",                     Sys.Date()))
}

