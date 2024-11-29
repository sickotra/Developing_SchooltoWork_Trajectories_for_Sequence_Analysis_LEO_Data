# Code File Types:

There are 4 file types in this folder - `.r`, `.rmd`, `.html`, `.md` 

* `.rmd`  - This is a RMarkdown file which allows interactive programming in RStudio. This file can be used to develop/adapt the code easily

* `.r` - The RMarkdown should be saved as .r once ready to ingest into the ONS SRS Environment. This file type is required for security reasons. It can be easily converted back to an RMarkdown file in the SRS by saving it with the `.rmd` file extension. The RMarkdown setup code chunk has `eval = false` and so will not be evaluated/run, this must be changed to `eval = true` if the user has access to the LEO data and is attempting to run the code within the SRS environment. 

* `.html` - Download this file and open it within your web browser. This enables easy navigation of the code using the contents hyperlinks.

* `.md` - This is a regular markdown file. It is only included within this repository to instantly display the code file with formatting within GitHub.
