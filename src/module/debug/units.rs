use gimli::write::{DebuggingInformationEntry, Unit, UnitEntryId};

pub(crate) struct DebuggingInformationCursor<'a> {
    entry_id_stack: Vec<UnitEntryId>,

    unit: &'a mut Unit,

    called_next_dfs: bool,
}

impl<'a> DebuggingInformationCursor<'a> {
    pub fn new(unit: &'a mut Unit) -> Self {
        Self {
            unit,
            entry_id_stack: Vec::new(),
            called_next_dfs: false,
        }
    }

    pub fn current(&mut self) -> Option<&mut DebuggingInformationEntry> {
        if self.entry_id_stack.len() > 0 {
            Some(self.unit.get_mut(*self.entry_id_stack.last().unwrap()))
        } else {
            None
        }
    }

    pub fn next_dfs(&mut self) -> Option<&mut DebuggingInformationEntry> {
        if !self.called_next_dfs {
            let root = self.unit.root();
            self.entry_id_stack.push(root);
            self.called_next_dfs = true;
            return None;
        }

        if self.entry_id_stack.len() == 0 {
            return None;
        }

        let last_element_id = self.entry_id_stack.pop().unwrap();
        let last_element = self.unit.get_mut(last_element_id);

        self.entry_id_stack.append(
            &mut last_element
                .children()
                .map(UnitEntryId::clone)
                .rev()
                .collect(),
        );

        self.current()
    }
}
