list(
  "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 3),
  "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 3, prepend_p = TRUE),
  "tbl_summary-fn:addnl-fn-to-run" =
    function(x) {
      if (!is.null(x$by)) x <- add_p(x) # add_p if there is a by variable
      bold_labels(x) 
      add_stat_label(x)
      italicize_levels(x)
    }
) |> set_gtsummary_theme()
