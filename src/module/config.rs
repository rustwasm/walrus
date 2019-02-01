use crate::error::Result;
use crate::module::Module;

/// Configuration for a `Module` which currently affects parsing.
#[derive(Clone, Debug, Default)]
pub struct ModuleConfig {
    pub(crate) generate_names: bool,
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

    /// Parses an in-memroy WebAssembly file into a `Module` using this
    /// configuration.
    pub fn parse(&mut self, wasm: &[u8]) -> Result<Module> {
        Module::parse(wasm, self)
    }
}
