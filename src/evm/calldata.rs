//!
//! Translates the calldata instructions.
//!

use inkwell::values::BasicValue;

use crate::context::address_space::AddressSpace;
use crate::context::function::intrinsic::Intrinsic as IntrinsicFunction;
use crate::context::Context;
use crate::Dependency;

///
/// Translates the calldata load.
///
pub fn load<'ctx, D>(
    context: &mut Context<'ctx, D>,
    offset: inkwell::values::IntValue<'ctx>,
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    let parent_offset_pointer = context.access_memory(
        context.field_const(
            (compiler_common::ABI_MEMORY_OFFSET_CALLDATA_OFFSET * compiler_common::SIZE_FIELD)
                as u64,
        ),
        AddressSpace::Heap,
        "calldata_parent_offset_pointer",
    );
    let parent_offset = context.build_load(parent_offset_pointer, "calldata_parent_offset");
    let offset =
        context
            .builder()
            .build_int_add(offset, parent_offset.into_int_value(), "calldata_offset");

    let pointer = context.access_memory(offset, AddressSpace::Parent, "calldata_pointer");
    let value = context.build_load(pointer, "calldata_value");

    Ok(Some(value))
}

///
/// Translates the calldata size.
///
pub fn size<'ctx, D>(
    context: &mut Context<'ctx, D>,
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    let length_pointer = context.access_memory(
        context.field_const(
            (compiler_common::ABI_MEMORY_OFFSET_CALLDATA_LENGTH * compiler_common::SIZE_FIELD)
                as u64,
        ),
        AddressSpace::Heap,
        "calldata_size_pointer",
    );
    let length = context.build_load(length_pointer, "calldata_size");

    Ok(Some(length.as_basic_value_enum()))
}

///
/// Translates the calldata copy.
///
pub fn copy<'ctx, D>(
    context: &mut Context<'ctx, D>,
    destination_offset: inkwell::values::IntValue<'ctx>,
    source_offset: inkwell::values::IntValue<'ctx>,
    size: inkwell::values::IntValue<'ctx>,
) -> anyhow::Result<Option<inkwell::values::BasicValueEnum<'ctx>>>
where
    D: Dependency,
{
    let memory_zero_block = context.append_basic_block("calldata_copy_memory_zero_block");
    let default_block = context.append_basic_block("calldata_copy_default_block");
    let join_block = context.append_basic_block("calldata_copy_join_block");

    let destination = context.access_memory(
        destination_offset,
        AddressSpace::Heap,
        "calldata_copy_destination_pointer",
    );

    let parent_offset_pointer = context.access_memory(
        context.field_const(
            (compiler_common::ABI_MEMORY_OFFSET_CALLDATA_OFFSET * compiler_common::SIZE_FIELD)
                as u64,
        ),
        AddressSpace::Heap,
        "calldata_copy_parent_offset_pointer",
    );
    let parent_offset = context.build_load(parent_offset_pointer, "calldata_copy_parent_offset");
    let combined_offset = context.builder().build_int_add(
        source_offset,
        parent_offset.into_int_value(),
        "calldata_copy_combined_offset",
    );

    let calldata_size = self::size(context)?
        .expect("Always exists")
        .into_int_value();
    let is_source_calldata_size = context.builder().build_int_compare(
        inkwell::IntPredicate::EQ,
        source_offset,
        calldata_size,
        "calldata_copy_is_source_calldata_size",
    );
    context.build_conditional_branch(is_source_calldata_size, memory_zero_block, default_block);

    context.set_basic_block(default_block);
    let source = context.access_memory(
        combined_offset,
        AddressSpace::Parent,
        "calldata_copy_source_pointer",
    );
    context.build_memcpy(
        IntrinsicFunction::MemoryCopyFromParent,
        destination,
        source,
        size,
        "calldata_copy_memcpy_from_child",
    );
    context.build_unconditional_branch(join_block);

    context.set_basic_block(memory_zero_block);
    let destination_pointer = context.access_memory(
        destination_offset,
        AddressSpace::Heap,
        "calldata_copy_memset_destination_pointer",
    );
    context.build_call(
        context.runtime.memset_uma_heap,
        &[
            destination_pointer.as_basic_value_enum(),
            context.field_const(0).as_basic_value_enum(),
            size.as_basic_value_enum(),
        ],
        "calldata_copy_memset_call",
    );
    context.build_unconditional_branch(join_block);

    context.set_basic_block(join_block);
    Ok(None)
}
