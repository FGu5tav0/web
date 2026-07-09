library(xml2)
library(rvest)
library(fs)

#===============================
# Configuração
#===============================

site_dir <- "_site"
site_url <- "https://gustavofrosi.com.br"

#===============================
# HTMLs do site
#===============================

html_files <-
  dir_ls(
    site_dir,
    recurse = TRUE,
    regexp = "index\\.html$"
  )

#===============================
# Cria XML
#===============================

doc <- xml_new_root(
  "urlset",
  xmlns = "http://www.sitemaps.org/schemas/sitemap/0.9"
)

xml_set_attr(
  doc,
  "xmlns:image",
  "http://www.google.com/schemas/sitemap-image/1.1"
)

#===============================
# Percorre páginas
#===============================

for (file in html_files) {
  html <- read_html(file)
  
  # URL da página
  rel_page <-
    path_rel(file, start = site_dir)
  
  page_url <-
    paste0(
      site_url,
      "/",
      gsub("\\\\", "/", rel_page)
    )
  
  # imagens
  imgs <- html_elements(html, "img")
  
  if (length(imgs) == 0) {
    next
  }
  
  node_url <- xml_add_child(doc, "url")
  
  xml_add_child(
    node_url,
    "loc",
    page_url
  )
  
  for (img in imgs) {
    src <- html_attr(img, "src")
    
    if (is.na(src)) {
      next
    }
    
    # ignora imagens externas
    if (grepl("^https?://", src)) {
      next
    }
    
    # URL absoluta
    image_url <-
      url_absolute(
        src,
        page_url
      )
    
    node_img <-
      xml_add_child(
        node_url,
        "image:image"
      )
    
    xml_add_child(
      node_img,
      "image:loc",
      image_url
    )
    
    alt <- html_attr(img, "alt")
    
    if (!is.na(alt) && nzchar(alt)) {
      xml_add_child(
        node_img,
        "image:caption",
        alt
      )
    }
    
    title <- html_attr(img, "title")
    
    if (!is.na(title) && nzchar(title)) {
      xml_add_child(
        node_img,
        "image:title",
        title
      )
    }
  }
}

#===============================
# Salva
#===============================

write_xml(
  doc,
  file.path(site_dir, "image-sitemap.xml")
)

cat("Image sitemap criado!\n")
