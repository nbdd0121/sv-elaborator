mod diag;
mod span;
mod src;

pub use self::diag::{Severity, Diagnostic, Note, DiagMgr};
pub use self::span::{Pos, Span, Spanned, FatPos, FatSpan};
pub use self::src::{Source, LineMap, SrcMgr};