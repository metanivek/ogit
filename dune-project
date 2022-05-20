(lang dune 3.0)

(name ogit)

(generate_opam_files true)

(source
 (github metanivek/ogit))

(authors "metanivek")

(maintainers "metanivek")

(license LICENSE)

(package
 (name ogit)
 (synopsis "Git data playground")
 (description "Git data playground")
 (depends ocaml dune)
 (tags
  (git playground)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
