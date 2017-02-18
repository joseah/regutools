#' @title Matrix scan.
#' @description Scan sequences with one or several position-specific scoring matrices (PSSM) to identify instances of the corresponding motifs (putative sites). This function supports a variety of background models (Bernoulli, Markov chains of any order).
#' @author José Alquicira Hernández
#' @param sequence Sequence(s) to scan - all the formats supported in RSAT can be used as input (default: \code{fasta})
#' @param matrix Matrix/ces to scan with. The matrix format is specified with the option \code{matrix.format} (see below) Default format: \code{tab}.
#' @param sequence.format Supported fields:
#' \itemize{
#' \item \code{fasta} (default)
#' \item \code{IG} (Intelligenetics)
#' \item \code{WC} (wconsensus)
#' \item \code{raw}
#' }
#' @param matrix.format Supported fields:
#' \itemize{
#' \item \code{tab}
#' \item \code{cb}
#' \item \code{consensus}
#' \item \code{gibbs}
#' \item \code{meme}
#' \item \code{assembly}
#' }
#' @param quick Delegates scanning to the C program matrix-scan-quick (developed by Matthieu Defrance). Evaluate if the quick mode is compatible with the selected output parameters, otherwise, run in the slower mode. Incompatible with - CRER scanning - window background model.
#' @param n.treatment Treatment of N characters. These characters are often used in DNA sequences to represent undefined or masked nucleotides.
#' \itemize{
#' \item \code{skip}.  N-containing regions are skipped.
#' \item \code{score}. N-containing regions are scored. The probability of an N is 1 for both the background model and the matrix. The N residues will thus contribute neither positively nor negatively to the weight score of the N-containing fragment. This option can be useful to detect sites which are at the border of N-containing regions, or in cases there are isolated N in the sequences.
#' }
#' @param consensus.name Use the motif (degenerate) consensus as matrix name.
#' @param pseudo Pseudo-count for the matrix (default: 1). The pseudo-count reflects the possibility that residues that were not (yet) observed in the model might however be valid for future observations. The pseudo-count is used to compute the corrected residue frequencies.
#' @param equi.pseudo If this option is called,  the pseudo-weight is distributed in an equiprobable way between residues. By default,  the pseudo-weight is distributed proportionally to residue priors,  except for the -window option where equipseudo is default.
#' @param top.matrices Only scan with the top # matrices per matrix file. This option is valid for some file formats containing multiple matrices where top matrices are generally more informative.
#' @param background.model Background model is a tab-delimited specification of oligonucleotide frequencies.
#' @param organism To use a precalculated background model from RSAT, choose the organism corresponding to the background model. Works with background and markov options.
#' @param background To use a precalculated background model from RSAT. Works with organism and markov options. Type of sequences used as background model for estimating expected oligonucleotide frequencies.
#' Supported:
#' \itemize{
#' \item \code{upstream}
#' \item \code{upstream-noorf}
#' \item \code{upstream-noorf-rm}
#' \item \code{intergenic}
#' }
#' @param background.input Calculate background model from the input sequence set. This option requires to specify the order of the background model with the option markov.
#' @param background.window Size of the sliding window for the background model calculation. This option requires to specify the order of the background model with the option markov (suitable for short order model only markov 0 or 1)
#' @param markov Order of the markov chain for the background model. This option is incompatible with the option background.
#' @param background.pseudo Pseudo frequency for the background models. Value must be a real between 0 and 1.
#' If this option is not specified,  the pseudo-frequency value depends on the background calculation.
#' For \code{background.input} and \code{background_window},  the pseudo frequency is automatically calculated with the length (L) of the sequence following this formula:
#' square-root of L divided by L+squareroot of L.
#' For \code{background.model},  default value is 0.01.
#' If the training sequence length (L) is known,  the value can be set by \code{background.pseudo} option to square-root of L divided by L+squareroot of L.
#' @param return.fields List of fields to return.
#' Supported fields:
#' \itemize{
#' \item \code{sites}
#' \item \code{rank}
#' \item \code{limits}
#' \item \code{normw}
#' \item \code{bg_model}
#' \item \code{matrix}
#' \item \code{freq_matrix}
#' \item \code{weight_matrix}
#' \item \code{distrib}
#' }
#' @param sort.distrib Sort score distribution by decreasing value of significance,  if value TRUE. By default,  the score distributions are sorted by score (weight).
#' @param lth Lower threshold on some parameter. Format=list of 'parameter value'.
#' Supported fields:
#' \itemize{
#' \item \code{score}
#' \item \code{pval}
#' \item \code{eval}
#' \item \code{sig}
#' \item \code{normw}
#' \item \code{proba_M}
#' \item \code{proba_B}
#' \item \code{rank}
#' \item \code{crer_sites}
#' \item \code{crer_size}
#' \item \code{occ}
#' \item \code{occ_sum}
#' \item \code{inv_cum}
#' \item \code{exp_occ}
#' \item \code{occ_pval}
#' \item \code{occ_eval}
#' \item \code{occ_sig}
#' \item \code{occ_sig_rank}
#' }
#' @param uth Upper threshold on some parameter. Format=list of 'param value'. Supported parameters: same as \code{lth}.
#' @param str Scan 1 or 2 strands for DNA sequences.
#' @param origin Define the origin for the calculation of positions.
#' \code{origin} -0 defines the end of each sequence as the origin.
#' The matching positions are then negative values, providing the distance between the match and the end of the sequence.
#' @param decimals Number of decimals displayed for the weight score.
#' @param crer_ids Assign one separate feature ID per CRER. This option is convenient to distinguish separate CRERs.
#' @return an R object with results retrieved from RSAT
#' @examples
#'
#' @export

MatrixScan <- function(sequence, matrix, sequence.format = "fasta", matrix.format = "tab",
                       quick = FALSE, n.treatment = NULL, concensus.name = NULL,
                       pseudo = 1, equi.pseudo = FALSE, top.matrices = NULL,
                       background.model = NULL, background = NULL, background.input = NULL,
                       background.window = NULL, markov = NULL, background.pseudo = NULL,
                       return.fields = NULL, sort.distrib = FALSE, lth = NULL, uth = NULL,
                       str = NULL, origin = NULL, decimals = NULL, crer.ids = NULL){

  !missing(sequence) || stop('"sequence" parameter is missing. Indicate an input format')
  !missing(matrix) || stop('Matrix is missing')
  !missing(n.treatment) || stop('"n.treatment" parameter is missing. Must be "score" or "skip"')

  if(quick){quick <- 1}else{quick <- NULL}
  if(equi.pseudo){quick <- 1}else{equi.pseudo <- NULL}
  if(sort.distrib){sort.distrib <- 1}else{sort.distrib <- NULL}


  parameters <- list(sequence = sequence,
                     matrix = matrix,
                     sequence_format = sequence.format,
                     matrix_format = matrix.format,
                     quick = quick,
                     n_treatment = n.treatment,
                     concensus_name = concensus.name,
                     pseudo = pseudo,
                     equi_pseudo = equi.pseudo,
                     top_matrices = top.matrices,
                     background_model = background.model,
                     background = background,
                     background_input = background.input,
                     background_window = background.window,
                     markov = markov,
                     background_pseudo = background.pseudo,
                     return_fields = return.fields,
                     sort_distrib = sort.distrib,
                     lth = lth,
                     uth = uth,
                     str = str,
                     origin = origin,
                     decimals = decimals,
                     crer_ids = crer.ids)

  res <- RSAT(method = "matrix_scan",
              parameters = parameters)
  return(res)
}
