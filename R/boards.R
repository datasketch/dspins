#' Use a folder within an AWS S3 bucket as a board.
#'
#' For authentication, an `.env` file with the necessary `AWS_ACCESS_KEY_ID`
#' and `AWS_SECRET_ACCESS_KEY` env vars needs to be located in the root directory.
#'
#' If both `user_name` and `org_name` are specified, `org_name` is used.
#'
#' @param user_name DS user name
#' @param org_name DS org name
#' @param bucket_id DS bucket id. Defaults to `bucket_id = "user"`
#' @export
#'
#' @examples
#' \dontrun{
#' board <- ds_board_s3(user_name = "test", bucket_id = "user")
#' }
ds_board_s3 <- function(
  user_name = NULL,
  org_name = NULL,
  bucket_id = NULL,
  versioned = FALSE,
  access_key = NULL,
  secret_access_key = NULL,
  session_token = NULL,
  credential_expiration = NULL,
  profile = NULL,
  region = NULL,
  endpoint = NULL,
  cache = NULL) {

  if(all(is.null(user_name), is.null(org_name))) stop("User, organization id or name cannot be null")

  load_env()

  folder <- org_name
  if(is.null(folder)) folder <- user_name

  folder <- validate_folder_name(folder)

  bucket_id <- validate_bucket_id(bucket_id)

  bucket <- get_bucket_name(bucket_id)

  dspins_bucket_create(bucket_id = bucket_id)

  config <- compact(list(
    credentials = compact(list(
      creds = compact(list(
        access_key_id = access_key,
        secret_access_key = secret_access_key,
        session_token = session_token
      )),
      profile = profile
    )),
    endpoint = endpoint,
    region = "us-east-1"
  ))

  svc <- paws.storage::s3(config = config)

  # Check that have access to the bucket
  svc$head_bucket(bucket)

  cache <- cache %||% paste0(board_cache_path(paste0("s3-", bucket)), "/", folder)
  new_board_v1("dspins_board_s3",
               name = "s3",
               bucket = bucket,
               folder = folder,
               svc = svc,
               cache = cache,
               versioned = versioned
  )
}


new_board <- function(board, api, cache, ...) {
  if (!is.na(cache)) {
    fs::dir_create(cache)
  }

  board <- structure(
    list(
      board = board,
      api = api,
      cache = cache,
      ...
    ),
    class = c(board, "pins_board")
  )

  board
}


new_board_v1 <- function(board, cache, versioned = FALSE, ...) {
  new_board(
    board = board,
    api = 1L,
    cache = cache,
    versioned = versioned,
    ...
  )
}



validate_folder_name <- function(folder){
  if(missing(folder)) stop("Need a folder name")
  if(nchar(folder) == 0)
    stop("Need a correct folder name")
  if(grepl("[^A-Za-z0-9-]",folder))
    stop("folder name can only contain letters, numbers and dashes")
  folder
}


get_bucket_name <- function(bucket_id){
  bucket_name(bucket_id)
}

get_bucket_id <- function(bucket_name){
  gsub(".dskt.ch", "", bucket_name)
}

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

validate_bucket_id <- function(bucket_id){
  if(is.null(bucket_id)){
    message("No bucket_id specified. Using 'user.dskt.ch' by default.")
    bucket_id <- "user"
  }
  bucket_id
}


#' @export
dspins_bucket_exists <- function(bucket_id){
  suppressMessages(x <- aws.s3::bucket_exists(bucket_name(bucket_id)))
  as.logical(x)
}


dspins_bucket_create <- function(bucket_id){

  if(!dspins_bucket_exists(bucket_id)){
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

}

#' @export
is_dspins_board_s3 <- function(board){
  if (inherits(board, "dspins_board_s3")) {
    TRUE
  } else {
    FALSE
  }
}

user_board_list_remote <- function(){
  x <- aws.s3::bucket_list_df()
  x <- x[,1]
  x[grepl(".dskt\\.ch",x)]
}


#' Load env variables
#'
#' @param file File containing AWS credentials
#'
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
