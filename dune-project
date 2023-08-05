(lang dune 3.7)

(name SolarSystem)

(generate_opam_files true)

(source
 (github am2257/JAMS))

(authors
 "Akaash Mahinth (am2257), Jacob Dentes (jmd477), Melvin Van Cleave (mav85), Samira Begum (sb926)")

(maintainers
 "Akaash Mahinth (am2257), Jacob Dentes (jmd477), Melvin Van Cleave (mav85), Samira Begum (sb926)")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name SolarSystem)
 (synopsis "Solar System Simulator")
 (description "A longer description")
 (depends ocaml dune graphics yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
