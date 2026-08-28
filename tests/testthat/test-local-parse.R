context("local preprocessing and parsing")

read_parse_fixture <- function() {
  cassette <- testthat::test_path("..", "fixtures", "tnrs_parse_all.yml")
  skip_if_not_installed("yaml")
  skip_if_not(file.exists(cassette))
  y <- yaml::read_yaml(cassette)
  do.call(rbind, lapply(y$http_interactions, function(i) {
    jsonlite::fromJSON(i$response$body$string)
  }))
}

test_that("tnrs_preprocess pulls off the family prefix", {
  p <- tnrs_preprocess("Arecaceae Mauritia")
  expect_equal(p$family, "Arecaceae")
  expect_equal(p$cleaned, "Mauritia")

  # Family names are title cased however they were submitted
  expect_equal(tnrs_preprocess("HYPOPTERYGIACEAE Hypopterygium")$family, "Hypopterygiaceae")

  # Classical family names are recognised too
  expect_equal(tnrs_preprocess("Compositae Aster")$family, "Compositae")

  # Junk attached to the family token is captured separately
  p <- tnrs_preprocess("Fabaceae-caes Chamaecrista conferta")
  expect_equal(p$family, "Fabaceae")
  expect_equal(p$family_unmatched, "-caes")

  # The literal words "fam" and "family" are not families
  expect_equal(tnrs_preprocess("family Acer rubrum")$family, "")

  # A name with no family prefix is left alone
  expect_equal(tnrs_preprocess("Acer rubrum")$family, "")
  expect_equal(tnrs_preprocess("Acer rubrum")$cleaned, "Acer rubrum")
})

test_that("tnrs_preprocess extracts annotations and indeterminate markers", {
  p <- tnrs_preprocess("Stylosanthes cf. guianensis")
  expect_equal(p$annotations, "cf.")
  expect_equal(p$cleaned, "Stylosanthes guianensis")

  expect_equal(tnrs_preprocess("Acer aff. rubrum")$annotations, "aff.")
  expect_equal(tnrs_preprocess("Acer rubrum?")$annotations, "?")

  # Indeterminate markers are dropped without being recorded
  expect_equal(tnrs_preprocess("Miconia sp.")$cleaned, "Miconia")
  expect_equal(tnrs_preprocess("Miconia indet.")$cleaned, "Miconia")

  # Leading non-alphabetic characters are set aside
  expect_equal(tnrs_preprocess("  123 Acer rubrum")$start_string, "123 ")

  expect_equal(nrow(tnrs_preprocess(character(0))), 0L)
})

test_that("tnrs_parse_internal splits conventional names", {
  q <- tnrs_parse_internal("Solanum bipatens Dunal")
  expect_equal(q$genus, "Solanum")
  expect_equal(q$species, "bipatens")
  expect_equal(q$authorship, "Dunal")

  q <- tnrs_parse_internal("Carex divisa subsp. ammophila")
  expect_equal(q$rank1, "subsp.")
  expect_equal(q$infra1, "ammophila")

  # Rank spellings are standardized
  expect_equal(tnrs_parse_internal("Carex divisa ssp ammophila")$rank1, "subsp.")
  expect_equal(tnrs_parse_internal("Acer rubrum var rubrum")$rank1, "var.")
  expect_equal(tnrs_parse_internal("Acer rubrum f. rubrum")$rank1, "fo.")

  # An authority may precede the infraspecific part
  q <- tnrs_parse_internal("Talisia clathrata Radlk subsp. clathrata")
  expect_equal(q$species, "clathrata")
  expect_equal(q$rank1, "subsp.")
  expect_equal(q$infra1, "clathrata")
  expect_equal(q$authorship, "Radlk")

  # A bare trailing epithet becomes an infraspecific name without a rank
  q <- tnrs_parse_internal("Chamaecrista conferta gurgueiana")
  expect_equal(q$infra1, "gurgueiana")
  expect_equal(q$rank1, "")

  # A wholly upper case name is case-normalized
  q <- tnrs_parse_internal("ARGEMONE INTERMEDIA")
  expect_equal(q$genus, "Argemone")
  expect_equal(q$species, "intermedia")

  # A family name is never taken as a genus
  q <- tnrs_parse_internal("Vochysiaceae Qualea grandiflora")
  expect_equal(q$genus, "Qualea")
  expect_equal(q$species, "grandiflora")

  expect_equal(nrow(tnrs_parse_internal(character(0))), 0L)
  expect_equal(tnrs_parse_internal("")$genus, "")
})

test_that("preprocessing agrees exactly with the API", {
  p <- read_parse_fixture()
  pre <- tnrs_preprocess(p$Name_submitted)

  expect_equal(pre$family, p$Family)
  expect_equal(pre$annotations, p$Annotations)
})

test_that("the internal parser reproduces every name component the API returns", {
  p <- read_parse_fixture()
  q <- tnrs_parse_internal(tnrs_preprocess(p$Name_submitted)$cleaned)

  expect_equal(q$genus, p$Genus)
  expect_equal(q$species, p$Specific_epithet)
  expect_equal(q$rank1, p$Infraspecific_rank)
  expect_equal(q$rank2, p$Infraspecific_rank_2)
  expect_equal(q$infra2, p$Infraspecific_epithet_2)

  # One deliberate divergence, on the wholly upper case name at row 72; see the
  # test below.  Everything else must still agree.
  diverges <- q$infra1 != p$Infraspecific_epithet
  expect_equal(sum(diverges), 1L)
  expect_equal(p$Name_submitted[diverges], "PAPAVERACEAE ARGEMONE INTERMEDIA SWEET")
  expect_equal(q$infra1[!diverges], p$Infraspecific_epithet[!diverges])
})

test_that("a name written wholly in one case does not gain a bogus epithet", {
  # Deliberate divergence from the API.  "Sweet" is Robert Sweet, the naming
  # authority; the API returns Infraspecific_epithet = "sweet" for the upper
  # case form but Author = "Sweet" for the correctly cased form of the same
  # name, so the upper case answer is an artefact of the lost capitalisation.
  q <- tnrs_parse_internal("ARGEMONE INTERMEDIA SWEET")
  expect_equal(q$genus, "Argemone")
  expect_equal(q$species, "intermedia")
  expect_equal(q$infra1, "")
  expect_equal(q$authorship, "SWEET")

  # The same applies to wholly lower case names
  q <- tnrs_parse_internal("argemone intermedia sweet")
  expect_equal(q$infra1, "")
  expect_equal(q$authorship, "sweet")

  # Mixed case still carries the signal, so a bare lower case epithet is kept
  q <- tnrs_parse_internal("Chamaecrista conferta gurgueiana")
  expect_equal(q$infra1, "gurgueiana")
  expect_equal(q$authorship, "")

  # An explicit rank indicator is unambiguous and still works without case
  q <- tnrs_parse_internal("ACER RUBRUM VAR RUBRUM")
  expect_equal(q$rank1, "var.")
  expect_equal(q$infra1, "rubrum")

  # Authorities are reported as submitted, not case-normalized
  expect_equal(tnrs_parse_internal("QUERCUS ALBA L.")$authorship, "L.")
})

test_that("the internal parser recovers most authorities", {
  # Authorities are where the internal parser and GNparser diverge: the server
  # routes trailing text it cannot place into Unmatched_terms, whereas the
  # internal parser keeps it as an authority.  That split is settled downstream
  # by the aggregator, which subtracts the matched components from the
  # submitted text, so exact agreement is not expected here.
  p <- read_parse_fixture()
  q <- tnrs_parse_internal(tnrs_preprocess(p$Name_submitted)$cleaned)

  agreement <- mean(q$authorship == p$Author)
  expect_gt(agreement, 0.9)
})

test_that("the parser dispatcher falls back when gnparser is unavailable", {
  expect_equal(tnrs_parse("Acer rubrum L.", parser = "internal")$parser, "internal")

  # "auto" must never error, whether or not gnparser is installed
  auto <- tnrs_parse("Acer rubrum L.", parser = "auto")
  expect_true(auto$parser %in% c("internal", "gnparser"))
  expect_equal(auto$genus, "Acer")

  skip_if(tnrs_gnparser_available(), "gnparser is installed")
  expect_error(tnrs_parse("Acer rubrum", parser = "gnparser"), "rgnparser")
})

test_that("the gnparser path also reproduces every name component", {
  skip_if_not(tnrs_gnparser_available(), "gnparser is not installed")

  p <- read_parse_fixture()
  q <- tnrs_parse_gnparser(tnrs_preprocess(p$Name_submitted)$cleaned)

  expect_equal(q$genus, p$Genus)
  expect_equal(q$species, p$Specific_epithet)
  expect_equal(q$rank1, p$Infraspecific_rank)
  expect_equal(q$rank2, p$Infraspecific_rank_2)
  expect_equal(q$infra2, p$Infraspecific_epithet_2)

  # Same single deliberate divergence as the internal parser, since the
  # component split is shared
  diverges <- q$infra1 != p$Infraspecific_epithet
  expect_equal(sum(diverges), 1L)
  expect_equal(q$infra1[!diverges], p$Infraspecific_epithet[!diverges])

  # GNparser places authorities slightly better than the internal parser
  expect_gt(mean(q$authorship == p$Author), 0.9)
})

test_that("the gnparser path copes with names GNparser itself rejects", {
  skip_if_not(tnrs_gnparser_available(), "gnparser is not installed")

  # GNparser requires a capitalised genus and returns nothing for names that are
  # wholly upper or lower case; those rows fall back to the internal parser
  q <- tnrs_parse_gnparser(c("ixora heterodoxa", "ARGEMONE INTERMEDIA"))
  expect_equal(q$genus, c("Ixora", "Argemone"))
  expect_equal(q$species, c("heterodoxa", "intermedia"))
  expect_equal(q$parser, c("internal", "internal"))

  # A repeated family name is not the genus
  q <- tnrs_parse_gnparser("Vochysiaceae Qualea grandiflora")
  expect_equal(q$genus, "Qualea")
  expect_equal(q$species, "grandiflora")

  # Normal names are handled by GNparser itself
  q <- tnrs_parse_gnparser("Acer rubrum L.")
  expect_equal(q$parser, "gnparser")
  expect_equal(q$genus, "Acer")
})
