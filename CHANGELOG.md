# `walrus` Change Log

--------------------------------------------------------------------------------

## Unreleased

Released YYYY-MM-DD.

### Added

* TODO (or remove section if none)

### Changed

* TODO (or remove section if none)

### Deprecated

* TODO (or remove section if none)

### Removed

* TODO (or remove section if none)

### Fixed

* TODO (or remove section if none)

### Security

* TODO (or remove section if none)

--------------------------------------------------------------------------------

## 0.7.0

Released 2019-05-17.

### Added

* Added the `walrus::ModuleCustomSections` API for working with arbitrary custom
  sections, including and especially custom sections that `walrus` itself has no
  special knowledge of. This is exposed as the `customs` field of a
  `walrus::Module`.

* Added the `Module::with_config` constructor method to create a default, empty
  module that uses the given configuration.

### Removed

* The `walrus::Module::custom` vector of raw custom modules has been removed and
  is superceded by the new `walrus::ModuleCustomSections` API. If you were using
  this and the old `CustomSection` type, switch to use the `RawCustomSection`
  type with `ModuleCustomSections`.

--------------------------------------------------------------------------------

## 0.6.0

Released 2019-05-02.

### Added

* `ModuleConfig::parse_file` and `Module::parse_file_with_config` helper
  functions to easily parse a Wasm file from disk with a given configuration.

### Changed

* `ModuleConfig::parse` takes `&self` instead of `&mut self` now. This was just
  an oversight / copy-past error before.

--------------------------------------------------------------------------------

## 0.5.0

--------------------------------------------------------------------------------

## 0.4.0

--------------------------------------------------------------------------------

## 0.3.0

Released 2019-02-19.

### Added

* Added support for the [reference
  types](https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md)
  wasm proposal. [#50](https://github.com/rustwasm/walrus/pull/50)
* Can finish a `FunctionBuilder` with the relevant `&mut` parts of a module,
  rather than a whole `&mut Module`. This is useful when some parts of the
  module are mutably borrowed
  elsewhere. [#56](https://github.com/rustwasm/walrus/pull/56)
* Can get a `FunctionBuilder` from an existing `LocalFunction` so you can build
  new expressions for the
  function. [#54](https://github.com/rustwasm/walrus/pull/54)
* Added the ability to delete functions, imports, exports, etc. Usually it is
  easier to just let the builtin GCing emit only the necessary bits of the wasm
  binary, but manually deleting will remove the item from iterators over the
  module's parts. If you delete a thing, you are responsible for ensuring that
  nothing else is referencing it (eg there are no remaining calls to a function
  that you are deleting, etc). [#58](https://github.com/rustwasm/walrus/pull/58)
* Added an `id` getter for
  `Import`s. [#59](https://github.com/rustwasm/walrus/pull/59)
* Added a mutable iterator for tables in a
  module. [#59](https://github.com/rustwasm/walrus/pull/59)
* Added a convenience function for getting the main function table for a
  module. [#57](https://github.com/rustwasm/walrus/pull/57)

### Changed

* The `WithSideEffects` expression variant can have arbitrary stack-neutral side
  effects before its value now, in addition to after its
  value. [#55](https://github.com/rustwasm/walrus/pull/55)

--------------------------------------------------------------------------------

## 0.2.1

Released 2019-02-14.

### Added

* Added configuration options for controlling emission of the producers section

--------------------------------------------------------------------------------

## 0.2.0

Released 2019-02-14.

### Added

* Added configuration options for controlling emission of the DWARF and name
  custom sections.

### Changed

* Changed the synthetic naming option from "generate_names" to
  "generate_synthetic_names_for_anonymous_items" to more accurately reflect what
  it does.

--------------------------------------------------------------------------------

## 0.1.0

Released 2019-02-12.

### Added

* Initial release!
