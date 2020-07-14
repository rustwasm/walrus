//! Table elements within a wasm module.

use crate::emit::{Emit, EmitContext, Section};
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
        let mut cx = cx.start_section(Section::Element);
        cx.encoder.usize(self.arena.len());

        for (id, element) in self.arena.iter() {
            cx.indices.push_element(id);
            let exprs = element.members.iter().any(|i| i.is_none());
            let exprs_bit = if exprs { 0x04 } else { 0x00 };
            let mut encode_ty = true;
            match &element.kind {
                ElementKind::Active { table, offset } => {
                    let table_index = cx.indices.get_table_index(*table);
                    if table_index == 0 {
                        cx.encoder.byte(0x00 | exprs_bit);
                        offset.emit(&mut cx);
                        encode_ty = false;
                    } else {
                        cx.encoder.byte(0x02 | exprs_bit);
                        cx.encoder.u32(table_index);
                        offset.emit(&mut cx);
                    }
                }
                ElementKind::Passive => {
                    cx.encoder.byte(0x01 | exprs_bit);
                }
                ElementKind::Declared => {
                    cx.encoder.byte(0x03 | exprs_bit);
                }
            };
            if encode_ty {
                if exprs {
                    element.ty.emit(&mut cx.encoder);
                } else {
                    cx.encoder.byte(0x00);
                }
            }

            cx.encoder.usize(element.members.len());
            for func in element.members.iter() {
                match func {
                    Some(id) => {
                        let index = cx.indices.get_func_index(*id);
                        if exprs {
                            cx.encoder.byte(0xd2);
                            cx.encoder.u32(index);
                            cx.encoder.byte(0x0b);
                        } else {
                            cx.encoder.u32(index);
                        }
                    }
                    None => {
                        assert!(exprs);
                        cx.encoder.byte(0xd0);
                        element.ty.emit(&mut cx.encoder);
                        cx.encoder.byte(0x0b);
                    }
                }
            }
        }
    }
}
