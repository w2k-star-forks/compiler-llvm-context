//!
//! Translates the comparison operations.
//!

use inkwell::values::BasicValue;

use crate::context::Context;
use crate::Dependency;

///
/// Translates the comparison operations.
///
pub fn compare<'ctx, D>(
    context: &mut Context<'ctx, D>,
    operand_1: inkwell::values::IntValue<'ctx>,
    operand_2: inkwell::values::IntValue<'ctx>,
    operation: inkwell::IntPredicate,
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    let result =
        context
            .builder()
            .build_int_compare(operation, operand_1, operand_2, "comparison_result");
    let result = context.builder().build_int_z_extend_or_bit_cast(
        result,
        context.field_type(),
        "comparison_result_extended",
    );
    Ok(Some(result.as_basic_value_enum()))
}
