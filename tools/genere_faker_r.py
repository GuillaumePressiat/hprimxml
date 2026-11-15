"""Génère R/faker_noms.R à partir de pyhprimxml/faker_noms.py (mêmes listes, même ordre)."""
import sys
from pathlib import Path

sys.path.insert(0, sys.argv[1] if len(sys.argv) > 1 else "../pyhprimxml")
from pyhprimxml.faker_noms import NOMS, PRENOMS_F, PRENOMS_M  # noqa: E402


def vecteur(nom, valeurs):
    lignes, ligne = [], "  "
    for v in valeurs:
        morceau = f'"{v}", '
        if len(ligne) + len(morceau) > 98:
            lignes.append(ligne.rstrip())
            ligne = "  "
        ligne += morceau
    lignes.append(ligne.rstrip().rstrip(","))
    return f"{nom} <- c(\n" + "\n".join(lignes) + "\n)\n"


Path("R/faker_noms.R").write_text(
    "# Listes de noms et prénoms français courants pour le faker de l'anonymisation.\n"
    "# Fichier généré par tools/genere_faker_r.py depuis pyhprimxml/faker_noms.py :\n"
    "# mêmes listes, même ordre, pour des fichiers anonymisés identiques en R et en Python.\n\n"
    + vecteur("PRENOMS_F", PRENOMS_F) + "\n" + vecteur("PRENOMS_M", PRENOMS_M) + "\n"
    + vecteur("NOMS", NOMS))
print("R/faker_noms.R :", len(PRENOMS_F), len(PRENOMS_M), len(NOMS))
