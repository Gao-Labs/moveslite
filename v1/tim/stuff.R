# dbGetQuery(conn = db, statement = "SELECT * FROM granddata.d36109 LIMIT 3;")
# tbl(src = db, from = "granddata.d36109")

# If ANY filters...
any_filters = any(c(".pollutant", ".by", ".sourcetype",
                    ".regclass", ".roadtype", ".fueltype") %in% v)

filter_where =  if(any_filters){"WHERE " }else{ NULL }

# Generate filter values for pollutant
filter_pollutant = if(".pollutant" %in% v){
  in_pollutant = paste0("'", f$.pollutant, "'", collapse = ", ")
  paste0("`pollutant` IN (", in_pollutant, ") ")
}else{ NULL }

# Generate filter values for by
filter_by = if(".by" %in% v){
  in_by = paste0("'", f$.by, "'", collapse = ", ")
  paste0("`by` IN (", in_by, ") ")
}else{ NULL }

# Generate filter values for sourcetype, regclass, fueltype, and roadtype
filter_sourcetype = if(".sourcetype" %in% v){
  in_sourcetype = paste0("'", f$.sourcetype, "'", collapse = ", ")
  paste0("`sourcetype` IN (", in_sourcetype, ") ")
}else{ NULL }

filter_regclass = if(".regclass" %in% v){
  in_regclass = paste0("'", f$.regclass, "'", collapse = ", ")
  paste0("`regclass` IN (", in_regclass, ") ")
}else{ NULL }

filter_fueltype = if(".fueltype" %in% v){
  in_fueltype = paste0("'", f$.fueltype, "'", collapse = ", ")
  paste0("`fueltype` IN (", in_fueltype, ") ")
}else{ NULL }

filter_roadtype = if(".roadtype" %in% v){
  in_roadtype = paste0("'", f$.roadtype, "'", collapse = ", ")
  paste0("`roadtype` IN (", in_roadtype, ") ")
}else{ NULL }

# Build a vector of these statements;
# if any were non-applicable, they received a value of NULL,
# which means they get skipped when collapsing below.
filter_statements = c(filter_pollutant, filter_by,
                      filter_sourcetype, filter_regclass,
                      filter_fueltype, filter_roadtype) %>%
  paste0(collapse = " AND ")

# Make a full statement, starting with WHERE and the filters
# If no filters, this just becomes a "" statement
filter_query = paste0(filter_where, filter_statements, collapse = " ")


# Write the top select phrase...
select_query = paste0("SELECT * FROM ", .table, " ")


#  dplyr::any_of(unique(c("geoid", "year", "emissions", .vars)))

paste0(
  "WITH ",
  "test AS (",
  "SELECT COLUMN_NAME ",
  "FROM information_schema.columns ",
  "WHERE ",
  paste0("TABLE_SCHEMA = '", "granddata", "' "),
  "AND ",
  paste0("TABLE_NAME = '", "d36109", "' "),
  "LIMIT 6) ",
  "SELECT * FROM test;"
) %>%
  dbGetQuery(.db, statement = .)



}

