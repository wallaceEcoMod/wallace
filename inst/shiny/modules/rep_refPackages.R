# Wallace EcoMod: a flexible platform for reproducible modeling of
# species niches and distributions.
# 
# rep_refPackages.R
# File author: Wallace EcoMod Dev Team. 2023.
# --------------------------------------------------------------------------
# This file is part of the Wallace EcoMod application
# (hereafter “Wallace”).
#
# Wallace is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Wallace is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Wallace. If not, see <http://www.gnu.org/licenses/>.
# --------------------------------------------------------------------------
#
rep_refPackages_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    strong("Download List of References"), br(), br(),
    strong("Select download file type"),
    selectInput('refFileType', label = "",
                choices = list("PDF", "HTML", "Word")),
    downloadButton('dlrefPackages', 'Download References')
  )
}

rep_refPackages_module_server <- function(input, output, session, common) {}
