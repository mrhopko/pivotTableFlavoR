new_pt_options <- function(pt_filter = NULL 
                       , pt_row = NA_character_ 
                       , pt_col = NA_character_ 
                       , pt_metrics = NULL 
                       , pt_sort = NA_character_
                       , pt_subtotals = NA_character_ 
                       , pt_grand_totals = NA_character_ 
                       , pt_calc_col = NULL
                       , pt_other = NULL) {
  
  me <- list(pt_filter = pt_filter,
             pt_row = as.character(pt_row),
             pt_col = as.character(pt_col),
             pt_metrics = pt_metrics,
             pt_sort = pt_sort,
             pt_subtotals = as.character(pt_subtotals),
             pt_grand_totals = as.character(pt_grand_totals),
             pt_calc_col = pt_calc_col,
             pt_other = as.list(pt_other))
  
  class(me) <- c(class(me),"pt_options")
  
  return(me)
}

pt_calc <- function(x, pt_options) UseMethod("pt_calc")

pt_calc.default <- function(x, pt_options) {
  stop("x must be a data.table")
}

pt_calc.data.table <- function(x, pt_options) {
  
  #identify group by variables
  pt_group = c(pt_options$pt_row, pt_options$pt_col)
  pt_group = pt_group[!is.na(pt_group) & pt_group != ""]

  #Filter
  x <- if(!is.null(pt_options$pt_filter)) {
    
    if(pt_options$pt_filter != "") {
      
      pt_filter = parse(text = pt_options$pt_filter)
      tryCatch({
          x[eval(pt_filter)]
        },
        error = function(cond) {
          message(paste("filter has failed",pt_options$pt_filter))
          message(cond)
          return(x)
        }
      )
    } else {
      x
    }
    
  } else {
    x
  }
  
#  print(paste("after filter, ", paste(names(x), collapse = ",")))
  
  #Group & metrics
  x <- if(length(pt_group) > 0 & 
          !is.null(pt_options$pt_metrics) & 
          try(pt_options$pt_metrics != "")) {
    
    pt_metrics <- if( substring( pt_options$pt_metrics, 1, 5) != "list(") {
      paste0("list(", pt_options$pt_metrics, ")")
    } else {
      pt_options$pt_metrics
    }
    
    pt_metrics = parse(text = pt_metrics)
    tryCatch({
      x[,eval(pt_metrics),by = pt_group]
    },
    error = function(cond) {
      message(paste("group and metrics have failed","col:" ,pt_options$pt_col, "row:", pt_options$pt_row, "metrics:", pt_options$pt_metrics))
      message(cond)
      return(x)
    }
    )
  } else {
    x
  }
  
  #sort
  pt_sort <- pt_options$pt_sort[!is.na(pt_options$pt_sort) & pt_options$pt_sort != ""]
  x <- if(length(pt_sort) > 0) {
    setorderv(x, pt_sort)
  } else {
    x
  }
  
  
  #rows and cols
  #metric columns are those not included in rows or cols
  x <- if (!is.null(pt_options$pt_col) & length(pt_options$pt_col > 0)) {
    if(pt_options$pt_col[1] != "") {
      value.var <- setdiff(names(x),pt_group)
      formula <- as.formula(paste(paste(pt_options$pt_row, collapse = " + "), 
                                  paste(pt_options$pt_col, collapse = " + "),
                                  sep = " ~ "))
      dcast.data.table(x, formula, value.var = value.var, sep = "__")
    } else {
      x
    }
    
  } else {
    x
  }
  
  
  #Calculated Columns
  x <- if(try(pt_options$pt_calc_col != "") & 
          !is.null(pt_options$pt_calc_col)) {
    
    pt_calc_col <- if( substring( pt_options$pt_calc_col, 1, 5) != "`:=`(") {
      paste0("`:=`(", pt_options$pt_calc_col, ")")
    } else {
      pt_options$pt_calc_col
    }
    
    pt_calc_col = parse(text = pt_calc_col)
    tryCatch({
      x[,eval(pt_calc_col)]
    },
    error = function(cond) {
      message(paste("Calculated Columns have failed: ",pt_options$pt_calc_col))
      message(cond)
      return(x)
    }
    )
  } else {
    x
  }
  

  #TODOD
  #column subtotals
  #column grand totals
  #row subtotals
  #row grand totals
  
  x
}




