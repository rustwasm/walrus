use crate::error::Result;
use crate::module::Module;

/// Configuration for a `Module` which currently affects parsing.
#[derive(Clone, Debug, Default)]
pub struct ModuleConfig {
    pub(crate) generate_names: bool,
    pub(crate) skip_strict_validate: bool,
}

impl ModuleConfig {
    /// Creates a fresh new configuration with default settings.
    pub fn new() -> ModuleConfig {
        ModuleConfig::default()
    }

    /// Sets a flag to whether debugging names are generated for
    /// locals/functions/etc when parsing and running passes for this module.
    ///
    /// By default this flag is `false`, and it will generate quite a few names
    /// if enabled!
    pub fn generate_names(&mut self, generate: bool) -> &mut ModuleConfig {
        self.generate_names = generate;
        self
    }

    /// Indicates whether the module, after parsing, performs strict validation
    /// of the wasm module to adhere with the current version of the wasm
    /// specification.
    ///
    /// This can be expensive for some modules and strictly isn't required to
    /// create a `Module` from a wasm file. This includes checks such as "atomic
    /// instructions require a shared memory".
    ///
    /// By default this flag is `true`
    pub fn strict_validate(&mut self, strict: bool) -> &mut ModuleConfig {
        self.skip_strict_validate = !strict;
        self
    }

    /// Parses an in-memroy WebAssembly file into a `Module` using this
    /// configuration.
    pub fn parse(&mut self, wasm: &[u8]) -> Result<Module> {
        Module::parse(wasm, self)
    }
}
