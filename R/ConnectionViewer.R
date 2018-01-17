RStudioConnectionOpened <- function(endpoint, token) {
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionOpened(type = "DataRobot",
                              displayName = "DataRobot",
                              icon = file.path(system.file(file.path("icons"),
                                               package = "datarobot"), "datarobot.png"),
                              host = endpoint,
                              listObjectTypes = function() { list(table = NULL) },
                              connectCode = "datarobot::ConnectToDataRobot",
                              disconnect = function() { NULL },
                              listObjects = datarobot::GetProjectList,
                              listColumns = datarobot::GetProjectList,
                              previewObject = datarobot::GetProjectList,
                              connectionObject = NULL)
  }
}
