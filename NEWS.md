# hprimxml 0.3.0

## Nouveautés

- Atomicité des messages : un message par RUM (`par_rum`), par acte (`par_acte`) ou par
  diagnostic (`par_diagnostic`).
- Actes NGAP dans le serveur d'actes (table `actes_ngap`) : consultations, forfaits
  techniques, biologie avec codes NABM ; un message peut mêler CCAM et NGAP.
- Repère `recueil` (date et UF d'observation) pour les états patient ; un seul repère par
  message, contrôlé avec un message d'erreur explicite.
- Anonymisation locale de fichiers pour les tests (`hprim_anonymiser()`) : faux noms
  plausibles, identifiants masqués au même format, dates décalées, une transformation
  aléatoire par fichier, fichiers toujours conformes au XSD.
- Écriture du XML par xml2 (libxml2), à la place d'un sérialiseur interne ; sorties
  toujours identiques, octet pour octet, à celles de pyhprimxml.
- Anciennes versions : valeurs énumérées accordées aux accents près (`création` en 1.0x,
  `creation` en 2.x) ; une colonne facultative vide n'empêche plus d'écrire dans une version
  qui ne la connaît pas.
- Cinq vignettes (découverte, écriture, lecture, validation, arbres intermédiaires) ; les
  scripts d'exemple servent d'exemples aux pages d'aide.
- Workflows GitHub : site pkgdown et releases (pré-releases pour les étiquettes de test).

## Changements d'API

- La table des actes CCAM s'appelle `actes_ccam`, dans le PMSI comme dans le serveur d'actes,
  à côté de `actes_ngap` : argument `actes_ccam` des fonctions d'écriture, élément
  `actes_ccam` des tables relues (et de l'argument `extra`). L'ancien argument `actes` reste
  accepté, avec un avertissement, jusqu'à une prochaine version.
- `write_evenements_serveur_actes(evenements, actes_ccam = NULL, actes_ngap = NULL, entete = NULL, ...)` :
  `actes_ccam` devient facultatif et `actes_ngap` s'insère avant `entete` ; nommer `entete`
  dans les appels.

## Corrections

- Le XSD officiel PMSI 1.06, lui-même invalide, lève désormais une erreur explicite (classe
  `hprim_schema_error`) au lieu d'un faux diagnostic de validation.

## Licence

- Licence : GNU AGPL (>= 3), avec le texte complet dans `LICENSE.md`.

# hprimxml 0.2.0

- Couche tabulaire : écriture et lecture des messages PMSI, serveur d'actes et états patient
  depuis et vers des tibbles, mêmes tables et mêmes options que pyhprimxml.

# hprimxml 0.1.0

- Couche de base : introspection des XSD, écriture et lecture des messages sous forme de
  listes, validation.
