#' Simulate Treatment Allocation
#'
#' Assigns patients to treatment arms using fixed ratios. Supports stratified randomization and returns
#' CDISC-compatible columns (`ARM`, `ARMCD`, `ACTARM`, `ACTARMCD`).
#'
#' @param df Data frame with patient information.
#' @param arms Character vector of arm labels (e.g., c("Placebo", "Treatment")).
#' @param ratio Numeric vector of allocation ratios (e.g., c(1,2)).
#' @param stratify_by Optional column name for stratified randomization (e.g., "STRATUM").
#' @param block_size Optional integer for block randomization (must be multiple of sum(ratio)).
#' @param seed Optional integer for reproducibility.
#'
#' @return Data frame with added columns: `ARM`, `ARMCD`, `ACTARM`, `ACTARMCD`.
#'
#' @export
simulate_allocation <- function(df,
                                arms = c("A", "B"),
                                ratio = c(1, 1),
                                stratify_by = NULL,
                                block_size = NULL,
                                seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (length(arms) != length(ratio)) stop("Length of arms and ratio must match.")
  if (!is.null(block_size) && (block_size %% sum(ratio)) != 0) stop("Block size must be multiple of sum(ratio).")

  generate_blocked <- function(n, arms, ratio, block_size = NULL) {
    prob <- ratio / sum(ratio)
    if (is.null(block_size)) {
      sample(arms, n, replace = TRUE, prob = prob)
    } else {
      repeats <- ceiling(n / block_size)
      block_pattern <- rep(arms, times = ratio)
      alloc <- unlist(replicate(repeats, sample(block_pattern), simplify = FALSE))
      alloc[1:n]
    }
  }

  if (!is.null(stratify_by) && stratify_by %in% names(df)) {
    df <- df[order(df[[stratify_by]]), ]
    df$ARM <- unlist(
      tapply(seq_len(nrow(df)), df[[stratify_by]], function(idxs) {
        generate_blocked(length(idxs), arms, ratio, block_size)
      })
    )
  } else {
    df$ARM <- generate_blocked(nrow(df), arms, ratio, block_size)
  }

  df$ARMCD <- as.character(match(df$ARM, arms))  # e.g., "1", "2"
  df$ACTARM <- df$ARM
  df$ACTARMCD <- df$ARMCD

  return(df)
}
