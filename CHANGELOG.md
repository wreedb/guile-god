# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased [2025-07-10]

### Added
- src/god.scm: main module file to import submodules
- src/util.scm: separated helper functions from other modules
- doc/manual.texi: texinfo manual document
- doc/version.texi: variable data for manual.texi
- doc/fdl-1.3.text: license for documentation
- Makefile.in: GNU Makefile template
- configure: Guile script to define default make variables

### Changed
- README.md: clarify manual license, update example
- INSTALL.md: update install instructions

### Removed
- build.ninja: too verbose, don't want to generate

## 0.1.1 [2025-07-07]

### Added
- test/*.scm: rudimentary test suite
- test/samples/*.god: test suite comparison samples
- build.ninja: alternative build tooling

## 0.1.0 [2025-07-01]

### Added
- src/: source files
- example/: sample *god* data files
- GNUmakefile: makefile for building/installing
- INSTALL.md: install/build instructions
- AUTHORS.md: author information
- CHANGELOG.md: this changelog
- LICENSE.md: license text (LGPL-3.0-or-later)
- README.md: repository summary
