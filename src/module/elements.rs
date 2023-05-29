//! Table elements within a wasm module.

use crate::emit::{Emit, EmitContext};
use crate::parse::IndicesToIds;
use crate::tombstone_arena::{Id, Tombstone, TombstoneArena};
use crate::{ir::Value, FunctionId, InitExpr, Module, Result, TableId, ValType};
use anyhow::{bail, Context};

/// A passive element segment identifier
pub type ElementId = Id<Element>;

/// A passive segment which contains a list of functions
#[derive(Debug)]
pub struct Element {
    id: Id<Element>,
    /// Whether this segment is passive or active.
    pub kind: ElementKind,
    /// The type of elements in this segment
    pub ty: ValType,
    /// The function members of this passive elements segment.
    pub members: Vec<Option<FunctionId>>,
    /// The name of this element, used for debugging purposes in the `name`
    /// custom section.
    pub name: Option<String>,
}

#[allow(missing_docs)]
#[derive(Debug, Copy, Clone)]
pub enum ElementKind {
    Passive,
    Declared,
    Active { table: TableId, offset: InitExpr },
}

impl Element {
    /// Get this segment's id
    pub fn id(&self) -> Id<Element> {
        self.id
    }
}

impl Tombstone for Element {
    fn on_delete(&mut self) {
        self.members = Vec::new();
    }
}

/// All element segments of a wasm module, used to initialize `anyfunc` tables,
/// used as function pointers.
#[derive(Debug, Default)]
pub struct ModuleElements {
    arena: TombstoneArena<Element>,
}

impl ModuleElements {
    /// Get an element associated with an ID
    pub fn get(&self, id: ElementId) -> &Element {
        &self.arena[id]
    }

    /// Get an element associated with an ID
    pub fn get_mut(&mut self, id: ElementId) -> &mut Element {
        &mut self.arena[id]
    }

    /// Delete an elements entry from this module.
    ///
    /// It is up to you to ensure that all references to this deleted element
    /// are removed.
    pub fn delete(&mut self, id: ElementId) {
        self.arena.delete(id);
    }

    /// Get a shared reference to this module's elements.
    pub fn iter(&self) -> impl Iterator<Item = &Element> {
        self.arena.iter().map(|(_, f)| f)
    }

    /// Get a mutable reference to this module's elements.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Element> {
        self.arena.iter_mut().map(|(_, f)| f)
    }

    /// Add an element segment
    pub fn add(
        &mut self,
        kind: ElementKind,
        ty: ValType,
        members: Vec<Option<FunctionId>>,
    ) -> ElementId {
        let id = self.arena.next_id();
        let id2 = self.arena.alloc(Element {
            id,
            kind,
            ty,
            members,
            name: None,
        });
        debug_assert_eq!(id, id2);
        id
    }
}

impl Module {
    /// Parses a raw was section into a fully-formed `ModuleElements` instance.
    pub(crate) fn parse_elements(
        &mut self,
        section: wasmparser::ElementSectionReader,
        ids: &mut IndicesToIds,
    ) -> Result<()> {
        log::debug!("parse element section");
        for (i, segment) in section.into_iter().enumerate() {
            let segment = segment?;
            let ty = ValType::parse(&segment.ty)?;
            match ty {
                ValType::Funcref => {}
                _ => bail!("only funcref type allowed in element segments"),
            }
            let members = segment
                .items
                .get_items_reader()?
                .into_iter()
                .map(|e| -> Result<_> {
                    Ok(match e? {
                        wasmparser::ElementItem::Func(f) => Some(ids.get_func(f)?),
                        wasmparser::ElementItem::Null(_) => None,
                    })
                })
                .collect::<Result<_>>()?;
            let id = self.elements.arena.next_id();

            let kind = match segment.kind {
                wasmparser::ElementKind::Passive => ElementKind::Passive,
                wasmparser::ElementKind::Declared => ElementKind::Declared,
                wasmparser::ElementKind::Active {
                    table_index,
                    init_expr,
                } => {
                    let table = ids.get_table(table_index)?;
                    self.tables.get_mut(table).elem_segments.insert(id);

                    let offset = InitExpr::eval(&init_expr, ids)
                        .with_context(|| format!("in segment {}", i))?;
                    match offset {
                        InitExpr::Value(Value::I32(_)) => {}
                        InitExpr::Global(global) if self.globals.get(global).ty == ValType::I32 => {
                        }
                        _ => bail!("non-i32 constant in segment {}", i),
                    }
                    ElementKind::Active { table, offset }
                }
            };
            self.elements.arena.alloc(Element {
                id,
                ty,
                kind,
                members,
                name: None,
            });
            ids.push_element(id);
        }
        Ok(())
    }
}

impl Emit for ModuleElements {
    fn emit(&self, cx: &mut EmitContext) {
        if self.arena.len() == 0 {
            return;
        }

        let mut wasm_element_section = wasm_encoder::ElementSection::new();
        // element_section.active(
        //     None,
        //     &ConstExpr::i32_const(1),
        //     RefType::FUNCREF,
        //     Elements::Functions(&element_section_tablefns[0..]),
        // );

        for (id, element) in self.arena.iter() {
            cx.indices.push_element(id);

            if element.members.iter().any(|i| i.is_none()) {
                let els_vec: Vec<wasm_encoder::ConstExpr> = element
                    .members
                    .iter()
                    .map(|func| match func {
                        Some(func) => {
                            wasm_encoder::ConstExpr::ref_func(cx.indices.get_func_index(*func))
                        }
                        None => wasm_encoder::ConstExpr::ref_null(wasm_encoder::HeapType::Func),
                    })
                    .collect();
                let els = wasm_encoder::Elements::Expressions(els_vec.as_slice());

                match &element.kind {
                    ElementKind::Active { table, offset } => {
                        wasm_element_section.active(
                            Some(cx.indices.get_table_index(*table)),
                            &offset.to_wasmencoder_type(&cx),
                            wasm_encoder::RefType::FUNCREF,
                            els,
                        );
                    }
                    ElementKind::Passive => {
                        wasm_element_section.passive(wasm_encoder::RefType::FUNCREF, els);
                    }
                    ElementKind::Declared => {
                        wasm_element_section.declared(wasm_encoder::RefType::FUNCREF, els);
                    }
                }
            } else {
                let els_vec: Vec<u32> = element
                    .members
                    .iter()
                    .map(|func| cx.indices.get_func_index(func.unwrap()))
                    .collect();
                let els = wasm_encoder::Elements::Functions(els_vec.as_slice());

                match &element.kind {
                    ElementKind::Active { table, offset } => {
                        wasm_element_section.active(
                            Some(cx.indices.get_table_index(*table)),
                            &offset.to_wasmencoder_type(&cx),
                            wasm_encoder::RefType::FUNCREF,
                            els,
                        );
                    }
                    ElementKind::Passive => {
                        wasm_element_section.passive(wasm_encoder::RefType::FUNCREF, els);
                    }
                    ElementKind::Declared => {
                        wasm_element_section.declared(wasm_encoder::RefType::FUNCREF, els);
                    }
                }
            }
        }
    }
}
