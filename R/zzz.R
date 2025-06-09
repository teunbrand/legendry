
on_load(
  if ("class_mapping" %in% getNamespaceExports("ggplot2")) {
    class_mapping <- utils::getFromNamespace("class_mapping", "ggplot2")
    new_aes <- function(x, env = caller_env()) {
      class_mapping(x, env = env)
    }
  }
)

.onLoad <- function(lib, pkg) {
  run_on_load() # nocov
}
