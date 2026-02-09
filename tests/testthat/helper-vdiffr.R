#' skip tests if R 4.2 gradient support for visual test cases is not available
#' @noRd
skip_if_no_r42_graphics = function() {
  testthat::skip_if_not(getRversion() >= "4.2")
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("svglite", "2.1.0")
  testthat::skip_if_not_installed("fontquiver")
  testthat::skip_if_not_installed("sysfonts")
  testthat::skip_if_not_installed("showtext")
}


#' alternative SVG writer that supports newer graphics engine features.
#' See https://github.com/r-lib/vdiffr/issues/132
#' @noRd
write_svg_r42 = function(plot, file, title = "") {
  # use Liberation Sans and Symbola to avoid platform-specific font differences
  liberation_sans = fontquiver::font_styles("Liberation", "Sans")
  symbola = fontquiver::font("Symbola", "Symbols", "Regular")
  sysfonts::font_add(
    "Liberation Sans",
    regular = liberation_sans$Regular$ttf,
    bold = liberation_sans$Bold$ttf,
    italic = liberation_sans$Italic$ttf,
    bolditalic = liberation_sans$`Bold Italic`$ttf,
    symbol = symbola$ttf
  )

  svglite::svglite(file, width = 10, height = 8, bg = "white", pointsize = 12, standalone = TRUE, always_valid = FALSE)
  showtext::showtext_begin()
  on.exit({
    showtext::showtext_end()
    grDevices::dev.off()
  })

  print(
    plot + ggtitle(title) + theme_test(base_family = "Liberation Sans")
  )
}
