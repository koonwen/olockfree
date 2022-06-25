(lang dune 3.1)

(name olockfree)

(generate_opam_files true)

(source
 (github koonwen/Olockfree))

(authors "Lee Koonwen")

(maintainers "Lee Koonwen")

(license AGPL-3.0-only)

(documentation https://github.com/koonwen/Olockfree)

(package
 (name olockfree)
 (synopsis "lock free data structure library")
 (description
  "Library interface and implementation for fine grained concurrent data structures")
 (depends
  (ocaml-variants
   (>= 5.0.0+trunk))
  domainslib
  dune
  alcotest)
 (tags
  ("lock-free" "fine-grain concurrency" "ocaml" "multicore")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
