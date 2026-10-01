
# ---------------------------------------------------------------
# OPTIE A: Een bestaande GitHub-repository naar je computer halen
# ---------------------------------------------------------------
usethis::create_from_github(
  "gebruikersnaam/repo-naam",
  destdir = dirname(getwd()),
  fork = FALSE
)

# ---------------------------------------------------------------
# OPTIE B: Een bestaand RStudio-project naar GitHub zetten
# (open eerst het project in RStudio)
# ---------------------------------------------------------------
usethis::use_git()      # zet Git aan voor dit project (RStudio herstart)
usethis::use_github()   # maakt de repo aan op GitHub en uploadt alles