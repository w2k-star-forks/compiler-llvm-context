//!
//! Translates the transaction return operations.
//!

use crate::context::address_space::AddressSpace;
use crate::context::code_type::CodeType;
use crate::context::function::intrinsic::Intrinsic as IntrinsicFunction;
use crate::context::Context;
use crate::Dependency;

///
/// Translates the normal return.
///
pub fn r#return<'ctx, 'dep, D>(
    context: &mut Context<'ctx, 'dep, D>,
    arguments: [inkwell::values::BasicValueEnum<'ctx>; 2],
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    match context.code_type() {
        CodeType::Deploy => {
            let immutables_offset_pointer = context.access_memory(
                context.field_const(0),
                AddressSpace::Heap,
                "immutables_offset_pointer",
            );
            context.build_store(
                immutables_offset_pointer,
                context.field_const(compiler_common::SIZE_FIELD as u64),
            );

            let immutables_length_pointer = context.access_memory(
                context.field_const(compiler_common::SIZE_FIELD as u64),
                AddressSpace::Heap,
                "immutables_length_pointer",
            );
            context.build_store(immutables_length_pointer, context.field_const(0));

            context.build_exit(
                IntrinsicFunction::Return,
                context.field_const(0),
                context.field_const((compiler_common::SIZE_FIELD * 2) as u64),
            );
        }
        CodeType::Runtime => {
            let offset = arguments[0].into_int_value();
            let size = arguments[1].into_int_value();

            context.build_exit(IntrinsicFunction::Return, offset, size);
        }
    }

    Ok(None)
}

///
/// Translates the revert.
///
pub fn revert<'ctx, 'dep, D>(
    context: &mut Context<'ctx, 'dep, D>,
    arguments: [inkwell::values::BasicValueEnum<'ctx>; 2],
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    let offset = arguments[0].into_int_value();
    let size = arguments[1].into_int_value();

    context.build_exit(IntrinsicFunction::Revert, offset, size);
    Ok(None)
}

///
/// Translates the stop.
///
pub fn stop<'ctx, 'dep, D>(
    context: &mut Context<'ctx, 'dep, D>,
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    context.build_exit(
        IntrinsicFunction::Return,
        context.field_const(0),
        context.field_const(0),
    );
    Ok(None)
}

///
/// Translates the invalid.
///
pub fn invalid<'ctx, 'dep, D>(
    context: &mut Context<'ctx, 'dep, D>,
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    context.build_exit(
        IntrinsicFunction::Revert,
        context.field_const(0),
        context.field_const(0),
    );
    Ok(None)
}
