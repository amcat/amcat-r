djangoFieldList <- function(fieldtype, ...){
  field = list()
  args = list(...)
  field[[fieldtype]] = args[!is.na(args)]
  field
}

#' Create a json string for django form fields
#'
#' Create the form fields to be used for R plugins in AmCAT.
#'
#' @param ... the field objects, created using the functions: BooleanField, IntegerField, etc. Note that the field objects have to be named!
#'
#' @return a json string
#' @export 
#'
#' @examples
#' djangoFormFields(field1=BooleanField(initial=F, required=F),
#'                  field2=IntegerField(initial=10, required=T))
djangoFormFields <- function(...){
  rjson::toJSON(list(...))
}

#' Create a Django Form Field
#'
#' Use to create field objects for R plugins in AmCAT. See djangoFormFields().
#'
#' @return
#' @export
BooleanField <- function(label=NA, initial=F, required=T, help_text=NA){
  djangoFieldList('BooleanField', label=label, initial=initial, required=required, help_text=help_text)
}

#' Create a Django Form Field
#'
#' Use to create field objects for R plugins in AmCAT. See djangoFormFields().
#'
#' @return
#' @export
CharField <- function(label=NA, initial='', required=T, min_length=NA, max_length=NA, help_text=NA){
  djangoFieldList('CharField', label=label, initial=initial, required=required, help_text=help_text)
}

#' Create a Django Form Field
#'
#' Use to create field objects for R plugins in AmCAT. See djangoFormFields().
#'
#' @return
#' @export
IntegerField <- function(label=NA, initial=0, required=T, min_value=NA, max_value=NA, help_text=NA){
  djangoFieldList('IntegerField', label=label, initial=initial, required=required, help_text=help_text)
}

#' Create a Django Form Field
#'
#' Use to create field objects for R plugins in AmCAT. See djangoFormFields().
#'
#' @return
#' @export
DecimalField <- function(label=NA, initial=0.0, required=T){
  djangoFieldList('DecimalField', label=label, initial=initial, required=required)
}

#' Create a Django Form Field
#'
#' Use to create field objects for R plugins in AmCAT. See djangoFormFields().
#'
#' @return
#' @export
ChoiceField <- function(label=NA, choices, required=T, help_text=NA){
  ChoiceField('ChoiceField', label=label, choices=choices, required=required, help_text=help_text)
}

function(){
  json = djangoFormFields(field1 = BooleanField(initial=F, required=F),
                          field2 = IntegerField(initial=10, required=T))
  cat(json)
}
