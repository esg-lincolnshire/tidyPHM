#' Tidy Public Health Metrics
#'
#' This function transforms a messy public health metrics dataframe into a tidy format.
#'
#' @param x A dataframe containing the raw data.
#' @param primary_segment A string representing the primary segment column name.
#' @param secondary_segment A string representing the secondary segment column name.
#'
#' @return A tidy dataframe.
#' @export
tidy_PHM <- function(x,primary_segment,secondary_segment){
  organised_cols <- x[1:2,2:ncol(x)]
  organised_cols <-as.data.frame(t(organised_cols))
  rownames(organised_cols) <- NULL
  colnames(organised_cols)<-c(primary_segment,secondary_segment)
  organised_cols<-organised_cols[!is.na(organised_cols[[secondary_segment]]),]
  organised_cols<- organised_cols %>%
    mutate(columnID = row.names(organised_cols))
  x<-x[,-ncol(x)]
  colnames(x)[1]<-"METRIC"
  x <- x[x$METRIC!="PCN" & x$METRIC!="Practice" & !is.na(x$METRIC) & x$METRIC!="Ethnicity Sub-Group" & !is.na(x$`Whole population for NHS Lincolnshire ICB`),]
  colnames(x)[2:ncol(x)]<-as.character(seq_along(x)[-1])
  x<- x %>%
    pivot_longer(
      cols = 2:ncol(x),
      names_to = secondary_segment,
      values_to = "METRIC_VALUE"
    )
  x<- x %>%
    pivot_wider(
      names_from = "METRIC",
      values_from = "METRIC_VALUE"
    )
  x <- inner_join(organised_cols,x,by=c("columnID"=secondary_segment))
  x$Population<-as.integer(sub(",","",x$Population))
  x <- x %>%
    mutate(across(4:ncol(x), as.numeric))
  x<- x %>% select(-columnID)
}
