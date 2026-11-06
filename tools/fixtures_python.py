"""Génère les fixtures de référence du test croisé R <-> Python.

Usage (depuis la racine du package R, pyhprimxml installé ou dans le PYTHONPATH) :
    python tools/fixtures_python.py

Pour chaque fichier d'exemple et chaque variante d'options, écrit la sortie de
pyhprimxml.to_xml(parse_xml(fichier), ...). Le package R doit produire les mêmes
octets à partir de hprim_parse_xml(fichier).
"""
from pathlib import Path

import xmltodict

import pyhprimxml as hx

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "tests" / "testthat" / "fixtures" / "python"
EXT = ROOT / "inst" / "extdata"

VARIANTES = {
    "brut": dict(drop_empty=False),
    "defaut": dict(),
    "nons": dict(xmlns=None),
    "vide": dict(xmlns=""),
    "latin1": dict(encoding="ISO-8859-1"),
    "compact": dict(pretty=False),
}


def special() -> bytes:
    """Message contenant tous les caractères à échapper, en texte et en attribut."""
    t = xmltodict.parse((EXT / "pmsi_00001.xml").read_bytes())
    t["evenementsPMSI"]["enteteMessage"]["commentaireMessage"] = 'A & B < C > D "q" \'s\' é € œ\ttab'
    pp = t["evenementsPMSI"]["evenementPMSI"]["patient"]["personnePhysique"]
    pp["nomUsuel"] = 'O\'NEIL & "Co" <x> Ø'
    t["evenementsPMSI"]["evenementPMSI"]["patient"]["@confidentiel"] = 'a"b<c>&d\'e\tf\ng'
    return hx.to_xml(t, drop_empty=False)


def exemples(pyhprimxml_dir: Path) -> None:
    """Exécute les scripts examples/ de pyhprimxml et conserve leurs fichiers produits."""
    import os
    import runpy
    import shutil
    import tempfile
    import warnings
    dest = OUT / "exemples"
    shutil.rmtree(dest, ignore_errors=True)
    for script in sorted((pyhprimxml_dir / "examples").glob("exemple_*.py")):
        cwd = os.getcwd()
        with tempfile.TemporaryDirectory() as d, warnings.catch_warnings():
            warnings.simplefilter("ignore")
            os.chdir(d)
            try:
                runpy.run_path(str(script))
            finally:
                os.chdir(cwd)
            for f in sorted(Path(d, "sortie").rglob("*")):
                if f.is_file():
                    cible = dest / script.stem / f.relative_to(Path(d, "sortie"))
                    cible.parent.mkdir(parents=True, exist_ok=True)
                    cible.write_bytes(f.read_bytes())


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    sources = sorted(EXT.glob("*_00001.xml"))
    (EXT / "special.xml").write_bytes(special())
    sources.append(EXT / "special.xml")
    for src in sources:
        tree = hx.parse_xml(src)
        for nom, opts in VARIANTES.items():
            (OUT / f"{src.stem}_{nom}.xml").write_bytes(hx.to_xml(tree, **opts))
    exemples(Path(hx.__file__).resolve().parents[1])
    print(len(list(OUT.rglob("*.*"))), "fixtures écrites dans", OUT)


if __name__ == "__main__":
    main()
