#![feature(macro_metavar_expr)]
#![recursion_limit = "8192"]

mod operation;

pub use operation::BulkMemoryOperator;
pub use operation::ControlFlowOperator;
pub use operation::ExceptionsOperator;
pub use operation::MVPOperator;
pub use operation::OpCode;
pub use operation::OperatorByProposal;
pub use operation::ReferenceTypesOperator;
pub use operation::RelaxedSIMDOperator;
pub use operation::SIMDOperator;
pub use operation::SaturatingFloatToIntOperator;
pub use operation::SignExtensionOperator;
pub use operation::TailCallOperator;
pub use operation::ThreadsOperator;
