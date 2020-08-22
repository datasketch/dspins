

#' @export
board_name <- function(bucket_id){
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
dspins_user_board_exists <- function(bucket_id){
  suppressMessages(x <- aws.s3::bucket_exists(board_name(bucket_id)))
  as.logical(x)
}

#' @export
dspins_is_board_connected <- function(bucket_id){
  board_name(bucket_id) %in% user_board_list_local()
}




#' @export
dspins_user_board_connect <- function(bucket_id){
  load_env()
  if(!dspins_user_board_exists(bucket_id)){
    message("User board does not exist")
    # new_bucket <- tryCatch(user_bucket_create(bucket_id), error=function(e) e, warning=function(w) w)
    new_bucket <- tryCatch(aws.s3::put_bucket(board_name(bucket_id), region = "us-east-1"),
                           error=function(e) e, warning=function(w) w)
    message("User board created: ", board_name(bucket_id))
    if(inherits(new_bucket,"error")){
      stop(new_bucket)
    }
    bucket_name <- board_name(bucket_id)
    aws.s3::put_website(bucket_name, request_body = s3_website_xml_body(bucket_id))
    policy <- paste0(
      read_lines(system.file("bucket_policy.json", package = "dspins")),
      collapse = "")
    policy <- glue::glue(policy, .open = "{{", .close = "}}")
    aws.s3::put_bucket_policy(board_name(bucket_id), policy)
  }
  nm <- board_name(bucket_id)
  if(!nm %in% user_board_list_local()){
    board_register_s3(name = nm, bucket = nm)
  }
  nm %in% user_board_list_local()
}


user_bucket_create <- function(bucket_id){
  # load_env()
  message("creating bucket: ", board_name(bucket_id))
  cat("hello")
  # aws.s3::put_bucket(board_name(bucket_id), region = "us-east-1")
  # headers = list(`x-amz-acl` = "public-read"))
}

user_board_list_local <- function(){
  x <- pins::board_list()
  x[grepl("*\\.dskt\\.ch$",x)]
}

user_board_list_remote <- function(){
  # load_env()
  x <- aws.s3::bucket_list_df()
  x <- x[[1]]
  x[grepl("^dskt\\.ch\\.",x)]
}

#' @export
load_env <- function(file = ".env"){
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
