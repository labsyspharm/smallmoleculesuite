optimalKinaseModal <- function() {
  modal(
    id = "modal_optimal_kinase",
    header = h4("LSP-OptimalKinase Library"),
    size = "lg",
    div(
      p("The LSP_OptimalKinase library is a collection of kinase inhibitors",
        "in the public domain that were selected based on criteria described",
        "in Moret et al.",
        tags$a("2019", href = "https://www.cell.com/cell-chemical-biology/fulltext/S2451-9456(19)30073-X#secsectitle0010")
      ),
      img(src = "assets/img/lsp-kinase-query-target.png") %>% 
        margin(l = "auto", r = "auto", b = 2),
      p("The library was developed with the aim to have a pair of inhibitors for each kinase with the members of the pair structurally distinct but similar in selectivity for the intended kinase, along with all approved drugs or clinical candidates that show affinity for said particular kinase (Figure 1). Being structurally distinct has the advantage that the inhibitors are the most likely to have orthogonal target affinity spectra (TAS) and phenotypic fingerprint (PFP). To assess whether two compounds are similar in selectivity, we discretized the selectivity of compounds into four qualitative levels; most selective (MS), semi selective (SS), poly selective (PS) and unknown (UN)."),
      p("The library is dividable into different tiers: Tier A is the minimal library and contains only those compounds (1) binding the specified list of genes with MS selectivity and (2) FDA-approved drugs binding more strongly than the affinity cutoff (which did not result in library redundancy). Tier B adds compounds with SS selectivity that target genes not covered by MS selectivity plus all compounds that bind the genes of interest and are in clinical development (clinical phases I–III). Tier C adds compounds from PS and UN specificity classes to maximally cover the user-specified list."),
      img(src = "assets/img/lsp-kinase-coverage-target.png")
    ) %>% 
      display("flex") %>% 
      flex(direction = "column")
  )
}

moaModal <- function() {
  modal(
    id = "modal_moa",
    header = h4("LSP-MoA Library"),
    size = "lg",
    div(
      p("The LSP_MoA library is a collection of compounds in the public domain that were selected based on criteria described in Moret et al. (2019)"),
      img(src = "assets/img/lsp-moa-query-target.png") %>% 
        margin(l = "auto", r = "auto", b = 2),
      p("The library was developed with the aim to have a pair of inhibitors for each protein target in the liganded genome with the members of the pair structurally distinct but similar in selectivity for the intended target, along with all approved drugs or clinical candidates that show affinity for said particular target (Figure 1). Being structurally distinct has the advantage that the compounds are the most likely to have orthogonal target affinity spectra (TAS) and phenotypic fingerprint (PFP). To assess whether two compounds are similar in selectivity, we discretized the selectivity of compounds into four qualitative levels; most selective (MS), semi selective (SS), poly selective (PS) and unknown (UN)." ),
      p("The library is dividable into different tiers: Tier A is the minimal library and contains only those compounds (1) binding the specified list of genes with MS selectivity and (2) FDA-approved drugs binding more strongly than the affinity cutoff (which did not result in library redundancy). Tier B adds compounds with SS selectivity that target genes not covered by MS selectivity plus all compounds that bind the genes of interest and are in clinical development (clinical phases I–III). Tier C adds compounds from PS and UN specificity classes to maximally cover the user-specified list (Figure 2)."),
      img(src = "assets/img/lsp-moa-coverage-target.png")
    ) %>% 
      display("flex") %>% 
      flex(direction = "column")
  )
}