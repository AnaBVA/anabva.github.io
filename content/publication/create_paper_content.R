# Copied from https://www.r-bloggers.com/2018/03/automatically-importing-publications-from-bibtex-to-a-hugo-academic-blog-2/

bibtex_2academic <- function(bibfile,
                             outfold,
                             abstract = FALSE,
                             overwrite = FALSE) {

  require(RefManageR)
  require(dplyr)
  require(stringr)
  require(anytime)

  # Import the bibtex file and convert to data.frame
  mypubs   <- ReadBib(bibfile, check = "warn", .Encoding = "UTF-8") %>%
    as.data.frame()

  # assign "categories" to the different types of publications
  mypubs   <- mypubs %>%
    dplyr::mutate(
      pubtype = dplyr::case_when(bibtype == "Article" ~ "2",
                                 bibtype == "Article in Press" ~ "2",
                                 bibtype == "InProceedings" ~ "1",
                                 bibtype == "Proceedings" ~ "1",
                                 bibtype == "Conference" ~ "1",
                                 bibtype == "Conference Paper" ~ "1",
                                 bibtype == "MastersThesis" ~ "3",
                                 bibtype == "PhdThesis" ~ "3",
                                 bibtype == "Manual" ~ "4",
                                 bibtype == "TechReport" ~ "4",
                                 bibtype == "Book" ~ "5",
                                 bibtype == "InCollection" ~ "6",
                                 bibtype == "InBook" ~ "6",
                                 bibtype == "Misc" ~ "0",
                                 TRUE ~ "0"))

  mypubs$abstract <- lapply(mypubs$pubid, get_abstract, my_id = "G6q6rP0AAAAJ")
  mypubs$url <- lapply(mypubs$pubid, get_link, my_id = "G6q6rP0AAAAJ")

  # create a function which populates the md template based on the info
  # about a publication
  create_md <- function(x) {

    # define a date and create filename by appending date and start of title
    if (!is.na(x[["year"]])) {
      x[["date"]] <- paste0(x[["year"]], "-01-01")
    } else {
      x[["date"]] <- "2999-01-01"
    }

    filename <- paste(x[["date"]], x[["title"]] %>%
                        str_replace_all(fixed(" "), "_") %>%
                        str_remove_all(fixed(":")) %>%
                        str_sub(1, 20) %>%
                        paste0(".md"), sep = "_")
    # start writing
    if (!file.exists(file.path(outfold, filename)) | overwrite) {
      fileConn <- file.path(outfold, filename)
      write("+++", fileConn)

      # Title and date
      write(paste0("title = \"", x[["title"]], "\""), fileConn, append = T)
      write(paste0("date = \"", anydate(x[["date"]]), "\""), fileConn, append = T)

      # Authors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
      auth_hugo <- str_replace_all(x["author"], " and ", "\", \"")
      auth_hugo <- stringi::stri_trans_general(auth_hugo, "latin-ascii")
      write(paste0("authors = [\"", auth_hugo,"\"]"), fileConn, append = T)

      # Publication type. Legend:
      # 0 = Uncategorized, 1 = Conference paper, 2 = Journal article
      # 3 = Manuscript, 4 = Report, 5 = Book,  6 = Book section
      write(paste0("publication_types = [\"", x[["pubtype"]],"\"]"),
            fileConn, append = T)

      # Publication details: journal, volume, issue, page numbers and doi link
      publication <- x[["journaltitle"]]
      # if (!is.na(x[["volume"]])) publication <- paste0(publication,
      #                                                  ", (", x[["volume"]], ")")
      if (!is.na(x[["number"]])) publication <- paste0(publication,
                                                       ", ", x[["number"]])
      # if (!is.na(x[["pages"]])) publication <- paste0(publication,
      #                                                 ", _pp. ", x[["pages"]], "_")
      if (!is.na(x[["url"]])) publication <- paste0(publication,
                                                    ", ",  x[["url"]])

      write(paste0("publication = \"", publication,"\""), fileConn, append = T)
      write(paste0("publication_short = \"", publication,"\""),fileConn, append = T)

      # Abstract and optional shortened version.
      if (abstract) {
        write(paste0("abstract = \"", x[["abstract"]],"\""), fileConn, append = T)
      } else {
        write("abstract = \"\"", fileConn, append = T)
      }
      write(paste0("abstract_short = \"","\""), fileConn, append = T)

      # other possible fields are kept empty. They can be customized later by
      # editing the created md

      write("image_preview = \"\"", fileConn, append = T)
      write("selected = false", fileConn, append = T)
      write("projects = []", fileConn, append = T)
      write("tags = []", fileConn, append = T)
      #links
      write("url_pdf = \"\"", fileConn, append = T)
      write("url_preprint = \"\"", fileConn, append = T)
      write("url_code = \"\"", fileConn, append = T)
      write("url_dataset = \"\"", fileConn, append = T)
      write("url_project = \"\"", fileConn, append = T)
      write("url_slides = \"\"", fileConn, append = T)
      write("url_video = \"\"", fileConn, append = T)
      write("url_poster = \"\"", fileConn, append = T)
      write("url_source = \"\"", fileConn, append = T)
      #other stuff
      write("math = true", fileConn, append = T)
      write("highlight = true", fileConn, append = T)
      # Featured image
      write("[header]", fileConn, append = T)
      write("image = \"\"", fileConn, append = T)
      write("caption = \"\"", fileConn, append = T)

      write("+++", fileConn, append = T)
    }
  }
  # apply the "create_md" function over the publications list to generate
  # the different "md" files.

  apply(mypubs, FUN = function(x) create_md(x), MARGIN = 1)
}

get_abstract = function(pub_id, my_id) {
  pub_id <- gsub(" ", "_",pub_id)
  paper_url = paste0("https://scholar.google.com/citations?view_op=view_citation&hl=fr&user=",
                     my_id, "&citation_for_view=", my_id,":", pub_id)
  paper_page = xml2::read_html(paper_url)
  html_page = XML::htmlTreeParse(paper_page, useInternalNodes=TRUE, encoding="utf-8")
  paper_abstract = XML::xpathSApply(html_page, "//div[@id='gsc_oci_descr']")
  paper_abstract <- XML::readHTMLList(paper_abstract[[1]])
  return(paper_abstract)
}


get_link = function(pub_id, my_id) {
  pub_id <- gsub(" ", "_",pub_id)
  paper_url = paste0("https://scholar.google.com/citations?view_op=view_citation&hl=fr&user=",
                     my_id, "&citation_for_view=", my_id,":", pub_id)
  paper_page = xml2::read_html(paper_url)
  html_page = XML::htmlTreeParse(paper_page, useInternalNodes=TRUE, encoding="utf-8")
  paper_links =  XML::xpathSApply(html_page, "//div[@id='gsc_oci_title_gg']")
  paper_links = XML::getHTMLLinks(paper_links[[1]])[1]
  return(paper_links)
}



# To get bibfile
library(dplyr)
scholar::get_publications("G6q6rP0AAAAJ") %>%
  filter(!is.na(cid)) %>%
  distinct(title, .keep_all = TRUE) %>%
  mutate_all(funs(stringr::str_replace(., "_", " "))) %>%
  arrange(desc(year)) %>%
  transmute(bibtype = "Article", author = as.character(author),
            title = as.character(title),
            journaltitle = as.character(journal), year,
            number = as.character(number),
            cites = as.character(cites),
            pubid = as.character(pubid),
            key = dplyr::row_number()) %>%
  RefManageR::as.BibEntry() %>%
  RefManageR::WriteBib(here::here("old/abva.bib"))

#abs <- get_abstract(mypubs$pubid[1],"G6q6rP0AAAAJ")

bibfile <- here::here("abva.bib")
outfold   <- "../content/publication"
bibtex_2academic(bibfile  = bibfile,
                 outfold   = outfold,
                 abstract  = TRUE)



