library(Rfacebook)

app_id="236822447140616"
app_secret="6ba22d1c92ef009bf2c309c00fc19e1e"

new_fbOAuth <- function (app_id, app_secret, extended_permissions = FALSE, 
                         legacy_permissions = FALSE, scope = NULL) {
  facebook <- oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth", 
                             access = "https://graph.facebook.com/oauth/access_token")
  myapp <- oauth_app("facebook", app_id, app_secret)
  if (is.null(scope)) {
    if (extended_permissions == TRUE) {
      
      
      # as alteraÃ§Ãµes significativas estÃ£o aqui >>>
      
      scope <- c("user_birthday", "user_hometown", "user_location", 
                 # "user_relationships", "publish_actions", 
                 "user_status", "user_likes")
      
      
    } else {
      scope <- c("public_profile", "user_friends", "manage_pages")
    }
    if (legacy_permissions == TRUE) {
      scope <- c(scope, "read_stream")
    }
  }
  if (packageVersion("httr") < "1.2") {
    stop("Rfacebook requires httr version 1.2.0 or greater")
  }
  if (packageVersion("httr") <= "0.2") {
    facebook_token <- oauth2.0_token(facebook, myapp, scope = scope)
    fb_oauth <- sign_oauth2.0(facebook_token$access_token)
    if (GET("https://graph.facebook.com/me", config = fb_oauth)$status == 200) {
      message("Authentication successful.")
    }
  }
  if (packageVersion("httr") > "0.2" & packageVersion("httr") <= 
      "0.6.1") {
    fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, cache = FALSE)
    if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status == 200) {
      message("Authentication successful.")
    }
  }
  if (packageVersion("httr") > "0.6.1" & packageVersion("httr") < 
      "1.2") {
    Sys.setenv(HTTR_SERVER_PORT = "1410/")
    fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, 
                               cache = FALSE)
    if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status == 200) {
      message("Authentication successful.")
    }
  }
  if (packageVersion("httr") >= "1.2") {
    fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, 
                               cache = FALSE)
    if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status == 200) {
      message("Authentication successful.")
    }
  }
  error <- tryCatch(callAPI("https://graph.facebook.com/pablobarbera", 
                            fb_oauth), error = function(e) e)
  if (inherits(error, "error")) {
    class(fb_oauth)[4] <- "v2"
  }
  if (!inherits(error, "error")) {
    class(fb_oauth)[4] <- "v1"
  }
  return(fb_oauth)
}

fb_oauth <- new_fbOAuth(app_id, app_secret,extended_permissions = TRUE,scope="read_insights")

token="EAADXY4XWLwgBAJ3ijhlQpWxmvr9UEuB0fZByfiAelECVzzEXiwzJvNDdqXbAa6gLYSHKjRNwpJX8VTrqx93vzXI9j66vjJwrYwWQgSPBZBIAZAg6uz2ZCS1ZCHn2z8puCpFZCzlwU6SNFxqTWAZCCbkFLntejiemaNYMIXBdc0I97Tr9lEZAZAWbr1ZBwAFYkdSGkHxXVlMG4CRAZDZD"

page="598581473683827"

dado <- getPage(page, token, reactions=TRUE)
post <- getPost("598581473683827_770018259873480",token=fb_oauth)

write.table(dado, file="Z:/Núcleo de Informações/BI/FIRJAN/Rafael Saraiva/R arquivos/teste.csv",sep = ";")