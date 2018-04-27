# Scrapes CRAN archives to determine the number of packages per release
# Homogeneous Theme for ggplot
"theme_piss" <- function(size_p = 16,
                         size_c = 12,
                         size_l = 10,
                         theme = theme_bw(),
                         ...) {
  text <-
    function(size, col = "#33666C", ...)
      element_text(size = size,
                   colour = col,
                   face = "bold",
                   ...)
  theme +
    theme(
      plot.title = text(size_p, hjust = 0.5),
      plot.subtitle = text(size_p-5, hjust = 0.5),
      axis.title = text(size_c),
      legend.title = text(size_l + 3),
      legend.text = text(size_l, col = "black")
    ) +
    theme(# legend.background = element_rect(colour = "black"),
      # legend.key = element_rect(fill = "white"),
      ...)
}


# Create a list of pages to scrape, including both archive and current
extract_url <- function(){
  url <- list(
    archive = "https://cran-archive.r-project.org/bin/windows/contrib/",
    active  = "https://cran.r-project.org/bin/windows/contrib/"
  )
  
  get_urls <- function(url){
    txt <- readLines(url)
    idx <- grep("\\d.\\d+/", txt)
    txt[idx]
    versions <- gsub(".*?>(\\d.\\d+(/)).*", "\\1", txt[idx])
    versions
    paste0(url, versions)
  }
  z <- lapply(url, get_urls)
  unname(unlist(z))
}


# Given a CRAN URL, extract the number of packages and date
extract_pkg_info <- function(url){
  extract_date <- function(txt, fun = max){
    txt <- txt[-grep("[(STATUS)|(PACKAGES)](.gz)*", txt)]
    # browser()
    
    pkgs <- grep(".zip", txt)
    txt <- txt[pkgs]
    ptn <- ".*?>(\\d{2}-...-\\d{4}).*"
    idx <- grep(ptn, txt)
    date <- gsub(ptn, "\\1", txt[idx])
    date <- as.Date(date, format = "%d-%b-%Y")
    match.fun(fun)(date)
  }
  
  message(url)
  txt <- readLines(url)
  count <- length(grep(".zip", txt))
  # sum(grepl(".zip", txt))
  
  # head(txt)
  data.frame(
    version = basename(url),
    date = extract_date(txt),
    pkgs = count
  )
}

# Get the list of CRAN URLs
CRAN_urls <- extract_url()

# Extract package information
pkgs <- lapply(CRAN_urls, extract_pkg_info)
pkgs <- do.call(rbind, pkgs)


# Extract major release information
major_releases <- pkgs[grep("\\.0", pkgs$version), ]
major_releases$date <- c(as.Date("2004-10-01"), as.Date("2013-04-01"))
major_releases$version[3] <- '3.5'


pkgs$date <- seq.Date(from = as.Date("2004-01-01"),
                      to = as.Date("2018-01-01"), 
                      length.out = nrow(pkgs))
library(tidyverse)
ggplot(pkgs, aes(x = date, y = pkgs)) + 
  geom_smooth() +
  geom_point() + 
  geom_vline(data = major_releases, 
             aes(xintercept = as.numeric(date)), 
             colour = "grey80") +
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), 
             colour = "grey80") + 
  geom_text(data = major_releases, 
            aes(label = paste("Version", version), y = 8000), 
            angle = 90, 
            colour = "red", 
            hjust = 1, vjust = -1) + 
  geom_text(label = "Version 3.4.4", 
            x = as.numeric(as.Date("2018-01-01")),
            y = 8000, 
            angle = 90, 
            colour = "red",
            hjust = 1, vjust = -1) +
  theme_piss() +
  scale_y_continuous(breaks = seq(0, 15000, by = 2500), 
                     label = seq(0, 15000, by = 2500)) + 
  ggtitle("Number of CRAN packages per R version") +
  xlab("Year") +
  ylab("Number of Packages") 


#__________________________________________________________________________
#__________________________________________________________________________

## Top 20 R packages growth from Stackoverflow
#__________________________________________________________________________
#__________________________________________________________________________


library(lubridate)
library(stringr)
library(scales)

folder <- "Section 1/Video 1.4"
questions <- read_csv(file.path(folder, "Questions.csv"))
answers <- read_csv(file.path(folder, "Answers.csv"))
r_posts <- bind_rows(questions, answers) %>%
  mutate(PostType = ifelse(is.na(Title), "Answer", "Question"))

tags <- read_csv(file.path(folder, "Tags.csv"))


reg <- "(library|require)\\([\"\']?(.*?)[\"\']?\\)|([\\.a-zA-Z\\d]+)::|he [\"\'\`]?([a-zA-Z\\.\\d]+)[\"\'\`]? package"

r_packages <- r_posts %>%
  mutate(Packages = str_match_all(Body, reg),
         Package = map(Packages, ~ c(.[, 3:5]))) %>%
  select(-Packages, -Body) %>%
  unnest(Package) %>%
  filter(!is.na(Package), !Package %in% c("", "R", "r")) %>%
  mutate(Package = str_replace(Package, "'", "")) %>%
  distinct(Id, Package, .keep_all = TRUE)



year_totals <- r_posts %>%
  semi_join(r_packages, by = "Id") %>%
  count(Year = year(CreationDate)) %>%
  rename(YearTotal = n)

package_by_year <- r_packages %>%
  transmute(Id = coalesce(ParentId, Id), Package, Year = year(CreationDate)) %>%
  distinct(Id, Package, Year) %>%
  count(Package, Year) %>%
  group_by(Package) %>%
  mutate(PackageTotal = sum(n)) %>%
  ungroup() %>%
  inner_join(year_totals, by = "Year")


## PLOTS

package_by_year %>%
  filter(PackageTotal >= 2200) %>%
  mutate(Percent = n / YearTotal) %>%
  complete(Package, Year, fill = list(Percent = 0)) %>%
  mutate(Package = reorder(Package, -PackageTotal, mean)) %>%
  ggplot(aes(Year, Percent, color = Package)) +
  geom_point(show.legend = FALSE, size = 1) +
  geom_line(show.legend = FALSE, size = 1) +
  facet_wrap(~ Package) +
  scale_x_continuous(breaks = seq(2009, 2017, 2)) +
  scale_y_continuous(labels = percent_format()) +
  theme_piss() +
  labs(x = "Year",
       y = "% of R questions where either the question or an answer uses package",
       title = "Use of R packages in Stack Overflow Q&A over time",
       subtitle = "For the 9 most-mentioned packages. ")

# PLOT : Tidyverse
package_by_year %>%
  filter(Package == "tidyverse") %>%
  mutate(Percent = n / YearTotal) %>%
  complete(Package, Year, fill = list(Percent = 0)) %>%
  mutate(Package = reorder(Package, -PackageTotal, mean)) %>%
  ggplot(aes(Year, Percent, color = Package)) +
  geom_point(show.legend = FALSE, size = 1) +
  geom_line(show.legend = FALSE, size = 1) +
  facet_wrap(~ Package) +
  scale_x_continuous(breaks = seq(2009, 2017, 2)) +
  scale_y_continuous(labels = percent_format()) +
  theme_piss() + ylab("")


## Total package tagged by Year
package_by_year %>%
  group_by(Year) %>% 
  select(YearTotal) %>% 
  distinct() %>% 
  ggplot(aes(x = Year, y = YearTotal)) + 
  geom_point() + 
  theme_piss()
