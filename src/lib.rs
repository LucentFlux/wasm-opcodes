#![feature(macro_metavar_expr)]
#![recursion_limit = "8192"]

//! Implementing something on top of [`wasmparser`] is slightly tedious due to the number of operations that it can parse, usually
//! resulting in either a huge match statement, or several match statements which each process some subset of the operations and
//! then have a catchall `unreachable!()` statement. This crate provides a middleground: a hierarchy of operations, split by proposal,
//! allowing you to exhaustively match firstly on the proposal, and then secondly on the operation.
//!
//! To start, use [`OperatorByProposal::from`]`(op: wasmparser::Operator)`, or the equivalent `Into` implementation.
//!
//! # Versioning
//!
//! This crate aims to match versions with [`wasmparser`], since the code in this crate is generated from wasmparser's supported
//! operations.

use wasmparser::for_each_operator;

macro_rules! define_opcode {
    ($(@$proposal:ident $op:ident $({ $($payload:tt)* })? => $visit:ident)*) => {
        /// All possible WebAssembly instruction opcodes across all proposals supported by [`wasmparser`].
        ///
        /// Use either [`OpCode::from_operator`] to extract the opcode from a [`wasmparser::Operator`], or
        /// [`OperatorByProposal::opcode`] to extract the opcode from an [`OperatorByProposal`].
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
        #[allow(missing_docs)]
        pub enum OpCode {
            $(
                $op,
            )*
        }

        impl OpCode {
            pub fn from_operator<'a>(op: &wasmparser::Operator<'a>) -> Self {
                match op {
                    $(
                        wasmparser::Operator::$op{..} => Self::$op,
                    )*
                }
            }
        }
    }
}
for_each_operator!(define_opcode);

/// A hierarchy of all WebAssembly operations, split by proposal.
pub enum OperatorByProposal<'a> {
    ControlFlow(proposals::ControlFlowOperator<'a>),
    MVP(proposals::MVPOperator),
    Exceptions(proposals::ExceptionsOperator),
    TailCall(proposals::TailCallOperator),
    ReferenceTypes(proposals::ReferenceTypesOperator),
    SignExtension(proposals::SignExtensionOperator),
    SaturatingFloatToInt(proposals::SaturatingFloatToIntOperator),
    BulkMemory(proposals::BulkMemoryOperator),
    Threads(proposals::ThreadsOperator),
    SIMD(proposals::SIMDOperator),
    RelaxedSIMD(proposals::RelaxedSIMDOperator),
    FunctionReferences(proposals::FunctionReferencesOperator),
    MemoryControl(proposals::MemoryControlOperator),
    GC(proposals::GCOperator),
}

macro_rules! impl_op_by_proposal {
    ($(@$proposal:ident $op:ident $({ $($field:ident : $field_ty:ty),* $(,)? })? => $visit:ident)*) => {
        impl<'a> From<wasmparser::Operator<'a>> for OperatorByProposal<'a> {
            fn from(op: wasmparser::Operator<'a>) -> Self {
                match op {
                    $(
                        wasmparser::Operator::$op $({ $($field),* })* => paste::paste!{ proposals::[< make_op_by_proposal_ $op:snake >] ( $($($field),* )* ) },
                    )*
                }
            }
        }
    }
}
for_each_operator!(impl_op_by_proposal);

impl<'a> From<OperatorByProposal<'a>> for wasmparser::Operator<'a> {
    fn from(op: OperatorByProposal<'a>) -> Self {
        match op {
            OperatorByProposal::ControlFlow(op) => op.into(),
            OperatorByProposal::MVP(op) => op.into(),
            OperatorByProposal::Exceptions(op) => op.into(),
            OperatorByProposal::TailCall(op) => op.into(),
            OperatorByProposal::ReferenceTypes(op) => op.into(),
            OperatorByProposal::SignExtension(op) => op.into(),
            OperatorByProposal::SaturatingFloatToInt(op) => op.into(),
            OperatorByProposal::BulkMemory(op) => op.into(),
            OperatorByProposal::Threads(op) => op.into(),
            OperatorByProposal::SIMD(op) => op.into(),
            OperatorByProposal::RelaxedSIMD(op) => op.into(),
            OperatorByProposal::FunctionReferences(op) => op.into(),
            OperatorByProposal::MemoryControl(op) => op.into(),
            OperatorByProposal::GC(op) => op.into(),
        }
    }
}

pub mod proposals {
    use super::*;

    /// Defines a new macro that filters operations by a proposal
    macro_rules! filter_operators {
        ($macro_name:ident (@ $filter_token:tt $(, !$filter_op:tt)* $(,)?) | $called_macro:ident($($args:tt)*)) => {
            macro_rules! $macro_name {
                ((munch) { $$($$filtered:tt)* }) => {
                    $called_macro!{$($args)* $$($$filtered)*}
                };
                $(
                    ((munch) { $$($$filtered:tt)* } @$$proposal:ident $filter_op $$({ $$($$payload:tt)* })? => $$visit:ident $$($$others:tt)*) => {
                        $macro_name!{(munch) { $$($$filtered)* } $$($$others)*}
                    };
                )*
                ((munch) { $$($$filtered:tt)* } @$filter_token $$op:ident $$({ $$($$payload:tt)* })? => $$visit:ident $$($$others:tt)*) => {
                    $macro_name!{(munch) { $$($$filtered)* @$filter_token $$op $$({ $$($payload)* })? => $$visit } $$($$others)*}
                };
                ((munch) { $$($$filtered:tt)* } @$$proposal:ident $$op:ident $$({ $$($$payload:tt)* })? => $$visit:ident $$($$others:tt)*) => {
                    $macro_name!{(munch) { $$($$filtered)* } $$($$others)*}
                };
                ($$($$others:tt)*) => {
                    $macro_name!{(munch) { } $$($$others)*}
                }
            }
        }
    }

    macro_rules! define_make_operator_fn {
        ($enum_name:ident( $struct_name:ident :: $op:ident $({ $($field:ident : $field_ty:ty),* $(,)? })? )) => {
            paste::paste!{
                #[inline(always)]
                pub(crate) fn [< make_op_by_proposal_ $op:snake >] <'a> ( $($($field : $field_ty),* )* ) -> OperatorByProposal<'a> {
                    OperatorByProposal::$enum_name(
                        $struct_name::$op {$($($field),* )*}
                    )
                }
            }
        }
    }

    /// Defines a struct with some identity, to be used with the filter to have a set of only some opcodes
    macro_rules! define_proposal_operator {
        ($struct_name:ident, $enum_name:ident $(@$proposal:ident $op:ident $({ $($field:ident : $field_ty:ty),* $(,)? })? => $visit:ident)*) => {
            #[derive(Clone, Debug)]
            #[doc = concat!("A subset of WebAssembly operations given by the ", stringify!($enum_name), " proposal")]
            pub enum $struct_name {
                $(
                    $op $({ $($field : $field_ty,)* })*,
                )*
            }

            impl $struct_name {
                pub fn opcode(&self) -> OpCode {
                    match &self {
                        $(
                            Self::$op { .. } => OpCode::$op,
                        )*
                    }
                }
            }

            impl<'a> From<$struct_name> for wasmparser::Operator<'a> {
                fn from(op: $struct_name) -> Self {
                    match op {
                        $(
                            $struct_name::$op $({ $($field,)* })* => wasmparser::Operator::$op $({ $($field,)* })*,
                        )*
                    }
                }
            }

            $(
                define_make_operator_fn! { $enum_name ( $struct_name :: $op $({ $($field : $field_ty),* })* ) }
            )*
        }
    }

    filter_operators!(filter_define_mvp(@mvp, 
        !End,
        !Block,
        !Loop,
        !If,
        !Else,
        !Br,
        !BrIf,
        !BrTable,
        !Return,
        !Call,
        !CallIndirect,
    ) | define_proposal_operator(MVPOperator, MVP));
    for_each_operator!(filter_define_mvp);
    filter_operators!(filter_define_exceptions(@exceptions) | define_proposal_operator(ExceptionsOperator, Exceptions));
    for_each_operator!(filter_define_exceptions);
    filter_operators!(filter_define_tail_call(@tail_call) | define_proposal_operator(TailCallOperator, TailCall));
    for_each_operator!(filter_define_tail_call);
    filter_operators!(filter_define_reference_types(@reference_types) | define_proposal_operator(ReferenceTypesOperator, ReferenceTypes));
    for_each_operator!(filter_define_reference_types);
    filter_operators!(filter_define_sign_extension(@sign_extension) | define_proposal_operator(SignExtensionOperator, SignExtension));
    for_each_operator!(filter_define_sign_extension);
    filter_operators!(filter_define_saturating_float_to_int(@saturating_float_to_int) | define_proposal_operator(SaturatingFloatToIntOperator, SaturatingFloatToInt));
    for_each_operator!(filter_define_saturating_float_to_int);
    filter_operators!(filter_define_bulk_memory(@bulk_memory) | define_proposal_operator(BulkMemoryOperator, BulkMemory));
    for_each_operator!(filter_define_bulk_memory);
    filter_operators!(filter_define_threads(@threads) | define_proposal_operator(ThreadsOperator, Threads));
    for_each_operator!(filter_define_threads);
    filter_operators!(filter_define_simd(@simd) | define_proposal_operator(SIMDOperator, SIMD));
    for_each_operator!(filter_define_simd);
    filter_operators!(filter_define_relaxed_simd(@relaxed_simd) | define_proposal_operator(RelaxedSIMDOperator, RelaxedSIMD));
    for_each_operator!(filter_define_relaxed_simd);
    filter_operators!(filter_define_function_references(@function_references) | define_proposal_operator(FunctionReferencesOperator, FunctionReferences));
    for_each_operator!(filter_define_function_references);
    filter_operators!(filter_define_memory_control(@memory_control) | define_proposal_operator(MemoryControlOperator, MemoryControl));
    for_each_operator!(filter_define_memory_control);
    filter_operators!(filter_define_gc(@gc) | define_proposal_operator(GCOperator, GC));
    for_each_operator!(filter_define_gc);

    impl<'a> OperatorByProposal<'a> {
        pub fn opcode(&self) -> OpCode {
            match self {
                Self::ControlFlow(op) => op.opcode(),
                Self::MVP(op) => op.opcode(),
                Self::Exceptions(op) => op.opcode(),
                Self::TailCall(op) => op.opcode(),
                Self::ReferenceTypes(op) => op.opcode(),
                Self::SignExtension(op) => op.opcode(),
                Self::SaturatingFloatToInt(op) => op.opcode(),
                Self::BulkMemory(op) => op.opcode(),
                Self::Threads(op) => op.opcode(),
                Self::SIMD(op) => op.opcode(),
                Self::RelaxedSIMD(op) => op.opcode(),
                Self::FunctionReferences(op) => op.opcode(),
                Self::MemoryControl(op) => op.opcode(),
                Self::GC(op) => op.opcode(),
            }
        }
    }

    /// MVP operators which do some kind of control flow.
    pub enum ControlFlowOperator<'a> {
        End,
        Block {
            blockty: wasmparser::BlockType,
        },
        Loop {
            blockty: wasmparser::BlockType,
        },
        If {
            blockty: wasmparser::BlockType,
        },
        Else,
        Br {
            relative_depth: u32,
        },
        BrIf {
            relative_depth: u32,
        },
        BrTable {
            targets: wasmparser::BrTable<'a>,
        },
        Return,
        Call {
            function_index: u32,
        },
        CallIndirect {
            type_index: u32,
            table_index: u32,
            table_byte: u8,
        },
    }

    impl<'a> ControlFlowOperator<'a> {
        pub fn opcode(&self) -> OpCode {
            match self {
                ControlFlowOperator::End => OpCode::End,
                ControlFlowOperator::Block { .. } => OpCode::Block,
                ControlFlowOperator::Loop { .. } => OpCode::Loop,
                ControlFlowOperator::If { .. } => OpCode::If,
                ControlFlowOperator::Else => OpCode::Else,
                ControlFlowOperator::Br { .. } => OpCode::Br,
                ControlFlowOperator::BrIf { .. } => OpCode::BrIf,
                ControlFlowOperator::BrTable { .. } => OpCode::BrTable,
                ControlFlowOperator::Return => OpCode::Return,
                ControlFlowOperator::Call { .. } => OpCode::Call,
                ControlFlowOperator::CallIndirect { .. } => OpCode::CallIndirect,
            }
        }
    }

    impl<'a> From<ControlFlowOperator<'a>> for wasmparser::Operator<'a> {
        fn from(op: ControlFlowOperator<'a>) -> Self {
            match op {
                ControlFlowOperator::End => wasmparser::Operator::End,
                ControlFlowOperator::Block { blockty } => wasmparser::Operator::Block { blockty },
                ControlFlowOperator::Loop { blockty } => wasmparser::Operator::Loop { blockty },
                ControlFlowOperator::If { blockty } => wasmparser::Operator::If { blockty },
                ControlFlowOperator::Else => wasmparser::Operator::Else,
                ControlFlowOperator::Br { relative_depth } => {
                    wasmparser::Operator::Br { relative_depth }
                }
                ControlFlowOperator::BrIf { relative_depth } => {
                    wasmparser::Operator::BrIf { relative_depth }
                }
                ControlFlowOperator::BrTable { targets } => {
                    wasmparser::Operator::BrTable { targets }
                }
                ControlFlowOperator::Return => wasmparser::Operator::Return,
                ControlFlowOperator::Call { function_index } => {
                    wasmparser::Operator::Call { function_index }
                }
                ControlFlowOperator::CallIndirect {
                    type_index,
                    table_index,
                    table_byte,
                } => wasmparser::Operator::CallIndirect {
                    type_index,
                    table_index,
                    table_byte,
                },
            }
        }
    }

    // Custom add control flow operations to make processing easier
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::End)}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::Block {
        blockty: wasmparser::BlockType,
    })}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::Loop {
        blockty: wasmparser::BlockType,
    })}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::If {
        blockty: wasmparser::BlockType,
    })}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::Else)}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::Br {
        relative_depth: u32,
    })}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::BrIf {
        relative_depth: u32,
    })}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::Return)}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::Call {
        function_index: u32,
    })}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::CallIndirect {
        type_index: u32,
        table_index: u32,
        table_byte: u8,
    })}
    define_make_operator_fn! {ControlFlow(ControlFlowOperator::BrTable {
        targets: wasmparser::BrTable<'a>,
    })}
}
