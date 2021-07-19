
#' @export
valid_folder_name <- function(folder){
  if(missing(folder)) stop("Need a folder name")
  if(nchar(folder) == 0)
    stop("Need a correct folder name")
  if(grepl("[^A-Za-z0-9-]",folder))
    stop("folder name can only contain letters, numbers and dashes")
  folder
}

#' @export
bucket_name <- function(bucket_id){
  if(missing(bucket_id)) stop("Need a bucket_id")
  if(nchar(bucket_id) == 0)
    stop("Need a correct bucket_id")
  # if(nchar(bucket_id) <= 23)
  #   stop("Need a correct bucket_id")
  if(grepl("[^A-Za-z0-9-]",bucket_id))
    stop("bucket_id can only contain letters, numbers and dashes")
  # paste0("dskt-ch-", bucket_id)
  paste0(bucket_id,".dskt.ch")
}

#' @export
board_name <- function(bucket_id, folder){
  paste0(bucket_name(bucket_id),"/",folder)
}


#' @export
dspins_bucket_exists <- function(bucket_id){
  suppressMessages(x <- aws.s3::bucket_exists(bucket_name(bucket_id)))
  as.logical(x)
}


#' @export
dspins_is_board_connected <- function(folder, bucket_id = "user"){
  paste0(bucket_name(bucket_id), "/",folder) %in% user_board_list_local()
}


#' @export
dspins_user_board_connect <- function(folder, bucket_id = "user"){
  .Deprecated("ds_board_s3")

  load_env()

  folder <- valid_folder_name(folder)

  if(!dspins_bucket_exists(bucket_id)){
    message("Bucket does not exist")
    # new_bucket <- tryCatch(user_bucket_create(bucket_id), error=function(e) e, warning=function(w) w)
    new_bucket <- tryCatch(aws.s3::put_bucket(bucket_name(bucket_id), region = "us-east-1"),
                           error=function(e) e, warning=function(w) w)
    message("Bucket created: ", bucket_name(bucket_id))
    if(inherits(new_bucket,"error")){
      stop(new_bucket)
    }
    bucket_name <- bucket_name(bucket_id)
    aws.s3::put_website(bucket_name, request_body = s3_website_xml_body(bucket_id))
    policy <- paste0(
      readr::read_lines(system.file("bucket_policy.json", package = "dspins")),
      collapse = "")
    policy <- glue::glue(policy, .open = "{{", .close = "}}")
    aws.s3::put_bucket_policy(bucket_name, policy)
  }

  bucket_name <- bucket_name(bucket_id)
  board <- board_name(bucket_id, folder)

  if(!board %in% user_board_list_local()){
    board_register_s3_dspins(folder = folder, bucket = bucket_name)
    message("User board registered: ", folder)
  }
  board %in% user_board_list_local()
}

legacy_user_board_list_local <- function(){
  x <- pins::board_list()
  x[grepl("/",x)]
  # x[grepl("*\\.dskt\\.ch$",x)]
}

legacy_user_board_list_remote <- function(){
  x <- aws.s3::bucket_list_df()
  x <- x[[1]]
  x[grepl("^dskt\\.ch\\.",x)]
}

#' @export
legacy_load_env <- function(file = ".env"){
  if(nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0){
    dotenv::load_dot_env()
  }
}


s3_website_xml_body <- function(user_name = ""){
  paste0(
    "
  <WebsiteConfiguration xmlns='http://s3.amazonaws.com/doc/2006-03-01/'>
  <IndexDocument>
    <Suffix>index.html</Suffix>
  </IndexDocument>
  <ErrorDocument>
    <Key>Error.html</Key>
  </ErrorDocument>

  <RoutingRules>
    <RoutingRule>
    <Condition>
      <HttpErrorCodeReturnedEquals>404</HttpErrorCodeReturnedEquals >
    </Condition>
    <Redirect>
      <HostName>datasektch.co</HostName>
      <ReplaceKeyPrefixWith>",user_name,"</ReplaceKeyPrefixWith>
    </Redirect>
    </RoutingRule>
  </RoutingRules>
</WebsiteConfiguration>
  ")
}
