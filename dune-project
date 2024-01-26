(lang dune 3.12)

(name ascii_camel)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ascii_camel)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune re bos cmdliner)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
