proposal<-"Boilerplate ISC Proposal"
proposal.file<-"isc-proposal.Rmd"
author<-"Steph Locke"

rmarkdown::render(proposal.file, output_format="html_document",
                  output_dir="out", quiet=TRUE)
rmarkdown::render(proposal.file, output_format="pdf_document",
                  output_dir="out", quiet=TRUE)