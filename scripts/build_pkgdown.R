#!/usr/bin/env Rscript
# Build pkgdown documentation for PEcAn packages
library(pkgdown)
library(yaml)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("No package names provided. Please pass package names as arguments.")
}
packages <- args
output_dir <- "_pkgdown_docs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
if (requireNamespace("PEcAn.logger", quietly = TRUE)) {
  logger <- PEcAn.logger::logger.info
} else {
  logger <- function(...) {
    message(paste(...)) 
  }
}

pkg_config <- function(pkg) {
  pkgname <- desc::desc_get("Package", pkg)

  list(
    url = "https://pecanproject.github.io/",
    home = list(
      title = sprintf("%s Functions for PEcAn", pkgname),
    ),
    template = list(
      bootstrap = 5,
      bslib = list(
        primary = "#0054AD", 
        `border-radius` = "0.5rem",
        `btn-border-radius` = "0.25rem"
      ),
      `light-switch` = TRUE, 
    ),
    navbar = list(
      structure = list(
        left = c("pecan_home", "reference", "news"),
        right = c("search", "github", "light-switch") 
      ),
      components = list(
        pecan_home = list(
        text = "PEcAn Home",
        href = "../../../index.html",
        `aria-label` = "PEcAn Project Home"
      ),
        reference = list(
        text = "Reference",
        href = "reference/index.html"
      ),
        github = list(
        icon = "fab fa-github",
        href = "https://github.com/PecanProject/pecan",
        `aria-label` = "GitHub"
      )
    )
  ),
    reference = list(
      list(
        title = "All Functions",
        desc = "All functions exported by this package",
        contents = list("matches('.*')")
      )
    ),
    news = list(
      text = "News",
      href = "news/index.html"
    ),
    development = list(
      mode = "auto"
    )
  )
}

logger("Building pkgdown docs for:", paste(packages, collapse = ", "))
for (pkg in packages) {
  logger("Building pkgdown site for:", pkg)
  current_wd <- getwd()  
  tryCatch({
    if (!dir.exists(pkg)) {
      stop(paste("Package directory does not exist:", pkg))
    }
    pkg_config_path <- file.path(pkg, "_pkgdown.yml")
    pkg_config <- pkg_config(pkg)
    # If _pkgdown.yml exists, merge with our config, otherwise create new
    if (file.exists(pkg_config_path)) {
      exist_config <- yaml::read_yaml(pkg_config_path)
      # Merge configurations, preserving existing settings
      merged_config <- modifyList(exist_config, pkg_config)
      yaml::write_yaml(merged_config, pkg_config_path)
    } else {
      yaml::write_yaml(pkg_config, pkg_config_path)
    }
    setwd(pkg) 
    pkgdown::build_site() 
    setwd(current_wd) 
    source_docs <- file.path(pkg, "docs")
    if (!dir.exists(source_docs)) {
      warning(paste("No docs folder created for:", pkg))
      next 
    }
    pkgname <- desc::desc_get("Package", pkg)
    dest <- file.path(output_dir, strsplit(pkg, "/")[[1]][1], pkgname)
    if (!dir.exists(dest)) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    }
    file.copy(
      from = list.files(source_docs, full.names = TRUE),
      to = dest,
      recursive = TRUE,
      overwrite = TRUE
    )
    logger("✅ Successfully copied docs from", pkg, "to", dest)
  }, error = function(e) {
    warning(paste("❌ Error building pkgdown site for", pkg, ":", e$message))
  },warning = function(w) {
    warning(paste("⚠️ Warning building pkgdown site for", pkg, ":", w$message))
  }, finally = {
    setwd(current_wd) 
  })
}

logger("Creating index page")

built_pkg_dirs <- list.dirs(output_dir, recursive=FALSE, full.names = FALSE)
html_header <- c(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '  <title>Package-specific documentation for the PEcAn R packages</title>',
  '  <style>',
  '    body { font-family: Arial, sans-serif; margin: 20px; }',
  '    .dir-struct { margin-top: 20px; }',
  '    .dir-group { margin-bottom: 10px; }',
  '    .pkg-list { display: none; margin-left: 20px; list-style-type: none; padding-left: 20px; }',
  '    .top-dir { cursor: pointer; font-weight: bold; color: #333; }',
  '    .top-dir::before { content: "▶"; margin-right: 5px; display: inline-block; }',
  '    .top-dir.expanded::before { content: "▼"; }',
  '    .expanded + .pkg-list { display: block; }',
  '    .pkg-list li { margin: 5px 0; }',
  '    .pkg-list a { text-decoration: none; color: #0366d6; }',
  '    .pkg-list a:hover { text-decoration: underline; }',
  '  </style>',
  '  <script>',
  '    function togglePackages(element) {',
  '      element.classList.toggle("expanded");',
  '    }',
  '  </script>',
  '</head>',
  '<body>',
  '<h1>PEcAn package documentation</h1>',
  '<p>Function documentation and articles for each PEcAn package,',
  '   generated from the package source using <a href="https://pkgdown.r-lib.org/" target="_blank">pkgdown</a> package.</p>',
  '',
  '<div class="dir-struct">'
)
content <- character(0)
for (dir in built_pkg_dirs) {
  content <- c(content,
    sprintf('  <div class="dir-group">'),
    sprintf('    <div class="top-dir" onclick="togglePackages(this)">%s</div>', dir)
  )
  pkg_dirs <- list.dirs(file.path(output_dir, dir), recursive=FALSE, full.names=FALSE)
  content <- c(content, '    <ul class="pkg-list">')
  for (pkg in pkg_dirs) {
    pkg_path <- file.path(dir, pkg, "dev/index.html")
    content <- c(content,
      sprintf('      <li><a href="%s">%s</a></li>', pkg_path, pkg)
    )
  }
  content <- c(content,
    '    </ul>',
    '  </div>'
  )
}
html_footer <- c(
  '</div>',
  '</body>',
  '</html>'
)
writeLines(
  text = c(html_header, content, html_footer),
  con = file.path(output_dir, "index.html")
)

logger("✅ All packages processed.")
