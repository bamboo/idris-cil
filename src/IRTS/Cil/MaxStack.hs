module IRTS.Cil.MaxStack where

import Language.Cil.Syntax

maxStackFor :: [MethodDecl] -> Int
maxStackFor _ = 16 -- double the default stack size

-- TODO: properly estimate stack size based on instruction behaviour
{-
data OpCode
  = Add                -- ^ Pops 2 values, adds the values, pushes result.
  | Add_ovf            -- ^ Pops 2 values, adds the values with a signed overflow check, pushes result.
  | Add_ovf_un         -- ^ Pops 2 values, adds the values with an unsigned overflow check, pushes result.
  | And                -- ^ Pops 2 values, do bitwise AND between the values, pushes result.
  | Beq Label          -- ^ Pops 2 values, if first value is equal to second value, jump to specified label.
  | Bge Label          -- ^ Pops 2 values, if first value is greater or equal to second value, jump to specified label.
  | Bgt Label          -- ^ Pops 2 values, if first value is greater than second value, jump to specified label.
  | Ble Label          -- ^ Pops 2 values, if first value is less or equal to second value, jump to specified label.
  | Blt Label          -- ^ Pops 2 values, if first value is less than second value, jump to specified label.
  | Box PrimitiveType  -- ^ Pops 1 value, boxes value type, pushes object reference.
  | Br Label           -- ^ Unconditionally jump to specified label.
  | Break              -- ^ Inform a debugger that a breakpoint has been reached.
  | Brfalse Label      -- ^ Pops 1 value, if value is false, null reference or zero, jump to specified label.
  | Brtrue Label       -- ^ Pops 1 value, if value is true, not null or non-zero, jump to specified label.
  | Call
      { callConv     :: [CallConv]      -- ^ Method is associated with class or instance.
      , returnType   :: PrimitiveType   -- ^ Return type of the method.
      , assemblyName :: AssemblyName    -- ^ Name of the assembly where the method resides.
      , typeName     :: TypeName        -- ^ Name of the type of which the method is a member.
      , methodName   :: MethodName      -- ^ Name of the method.
      , paramTypes   :: [PrimitiveType] -- ^ Types of the formal parameters of the method.
      } -- ^ Pops /n/ values, calls specified method, pushes return value. (where /n/ is the number of formal parameters of the method).
  | CallVirt
      { returnType   :: PrimitiveType   -- ^ Return type of the method.
      , assemblyName :: AssemblyName    -- ^ Name of the assembly where the method resides.
      , typeName     :: TypeName        -- ^ Name of the type of which the method is a member.
      , methodName   :: MethodName      -- ^ Name of the method.
      , paramTypes   :: [PrimitiveType] -- ^ Types of the formal parameters of the method.
      } -- ^ Pops /n/ values, calls specified virtual method, pushes return value. (where /n/ is the number of formal parameters of the method).
  | Castclass PrimitiveType -- ^ Pops 1 value, attempts to cast it to the specified type. If this succeeds, pushes the cast value. Otherwise throws an InvalidCastException.
  | Ceq                -- ^ Pops 2 values, if they are equal, pushes 1 to stack; otherwise, pushes 0.
  | Cgt                -- ^ Pops 2 values and compares them.
  | Ckfinite           -- ^ Pops a float or double. Throws an ArithmeticException if the popped value is NaN or +/- infinity. Pushes the popped value.
  | Clt                -- ^ Pops 2 values and compares them.
  | Dup                -- ^ Pops 1 value, copies it, pushes the same value twise.
  | Div                -- ^ Pops 2 values, divides the first by the second, pushes the result.
  | Div_un             -- ^ Pops 2 integers, divides the first by the second when consider as unsigned integers, pushes the result.
  | Isinst TypeName    -- ^ Tests if an object reference is an instance of class, returning either a null reference or an instance of that class or interface.
  | Ldarg Offset       -- ^ Loads /n/-th argument to current method onto stack.
  | Ldarg_0            -- ^ Loads 0th argument to current method onto stack.
  | Ldarg_1            -- ^ Loads 1th argument to current method onto stack.
  | Ldarg_2            -- ^ Loads 2th argument to current method onto stack.
  | Ldarg_3            -- ^ Loads 3th argument to current method onto stack.
  | LdargN DottedName  -- ^ Loads named argument to current method onto stack.
  | Ldarga Offset      -- ^ Loads address of the /n/-th argument to current method onto stack.
  | LdargaN DottedName -- ^ Loads address of the named argument to current method onto stack.
  | Ldc_i4 Integer     -- ^ Loads the supplied 32-bit integer onto the stack.
  | Ldc_i4_0           -- ^ Loads the value 0 onto the stack.
  | Ldc_i4_1           -- ^ Loads the value 1 onto the stack.
  | Ldc_i4_2           -- ^ Loads the value 2 onto the stack.
  | Ldc_i4_3           -- ^ Loads the value 3 onto the stack.
  | Ldc_i4_4           -- ^ Loads the value 4 onto the stack.
  | Ldc_i4_5           -- ^ Loads the value 5 onto the stack.
  | Ldc_i4_6           -- ^ Loads the value 6 onto the stack.
  | Ldc_i4_7           -- ^ Loads the value 7 onto the stack.
  | Ldc_i4_8           -- ^ Loads the value 8 onto the stack.
  | Ldc_i4_m1          -- ^ Loads the value -1 onto the stack.
  | Ldc_i4_s Int       -- ^ Loads the supplied 8-bit integer onto the stack as 32-bit integer (short form).
  | Ldc_i8 Integer     -- ^ Loads the supplied 64-bit integer onto the stack.
  | Ldc_r4 Float       -- ^ Loads the supplied 32-bit float onto the stack.
  | Ldc_r8 Double      -- ^ Loads the supplied 64-bit double onto the stack.
  | Ldelem_i           -- ^ Pops an array reference and an index. Pushes the native integer in the specified slot of the array.
  | Ldelem_i1          -- ^ Pops an array reference and an index. Pushes the 8-bit integer in the specified slot of the array.
  | Ldelem_i2          -- ^ Pops an array reference and an index. Pushes the 16-bit integer in the specified slot of the array.
  | Ldelem_i4          -- ^ Pops an array reference and an index. Pushes the 32-bit integer in the specified slot of the array.
  | Ldelem_i8          -- ^ Pops an array reference and an index. Pushes the 64-bit integer in the specified slot of the array.
  | Ldelem_u1          -- ^ Pops an array reference and an index. Pushes the unsigned 8-bit integer in the specified slot of the array.
  | Ldelem_u2          -- ^ Pops an array reference and an index. Pushes the unsigned 16-bit integer in the specified slot of the array.
  | Ldelem_u4          -- ^ Pops an array reference and an index. Pushes the unsigned 32-bit integer in the specified slot of the array.
  | Ldelem_u8          -- ^ Pops an array reference and an index. Pushes the unsigned 64-bit integer in the specified slot of the array.
  | Ldelem_r4          -- ^ Pops an array reference and an index. Pushes the float in the specified slot of the array.
  | Ldelem_r8          -- ^ Pops an array reference and an index. Pushes the double in the specified slot of the array.
  | Ldelem_ref         -- ^ Pops an array reference and an index. Pushes the object reference in the specified slot of the array.
  | Ldelema PrimitiveType -- ^ Pops an array reference and an index. Pushes the address of the specified slot of the array.
  | Ldfld
      { fieldType    :: PrimitiveType  -- ^ Type of the field.
      , assemblyName :: AssemblyName   -- ^ Name of the assembly where the field resides.
      , typeName     :: TypeName       -- ^ Name of the type of which the field is a member.
      , fieldName    :: FieldName      -- ^ Name of the field.
      } -- ^ Pops object reference, find value of specified field on object, pushes value to the stack.
  | Ldflda
      { fieldType    :: PrimitiveType  -- ^ Type of the field.
      , assemblyName :: AssemblyName   -- ^ Name of the assembly where the field resides.
      , typeName     :: TypeName       -- ^ Name of the type of which the field is a member.
      , fieldName    :: FieldName      -- ^ Name of the field.
      } -- ^ Pops object reference, find address of specified field on the object, pushes address to the stack.
  | Ldftn
      { callConv     :: [CallConv]       -- ^ Method is associated with class or instance.
      , returnType   :: PrimitiveType    -- ^ Return type of the method.
      , assemblyName :: AssemblyName     -- ^ Name of the assembly where the method resides.
      , typeName     :: TypeName         -- ^ Name of the type of which the method is a member.
      , methodName   :: MethodName       -- ^ Name of the method.
      , paramTypes   :: [PrimitiveType]  -- ^ Types of the formal parameters of the method.
      } -- ^ Pops object reference, finds address of specified method, pushes address as native int to the stack.
  | Ldind_i            -- ^ Pops an address, pushes the native integer stored at the address.
  | Ldind_i1           -- ^ Pops an address, pushes the 8-bit integer stored at the address as a 32-bit integer.
  | Ldind_i2           -- ^ Pops an address, pushes the 16-bit integer stored at the address as a 32-bit integer.
  | Ldind_i4           -- ^ Pops an address, pushes the 32-bit integer stored at the address.
  | Ldind_i8           -- ^ Pops an address, pushes the 64-bit integer stored at the address as a 64-bit integer.
  | Ldind_r4           -- ^ Pops an address, pushes the 32-bit float stored at the address.
  | Ldind_r8           -- ^ Pops an address, pushes the 64-bit double stored at the address.
  | Ldind_ref          -- ^ Pops an address, pushes the object reference specified at the address.
  | Ldind_u1           -- ^ Pops an address, pushes the 8-bit unsigned integer stored at the address as a 32-bit integer.
  | Ldind_u2           -- ^ Pops an address, pushes the 16-bit unsigned integer stored at the address as a 32-bit integer.
  | Ldind_u4           -- ^ Pops an address, pushes the 32-bit unsigned integer stored at the address as a 32-bit integer.
  | Ldlen              -- ^ Pops an array reference, pushes the native unsigned integer length of the array.
  | Ldloc Offset       -- ^ Pushes value of local variable, specified by index, to the stack.
  | Ldloc_0            -- ^ Pushes 0th local variable to the stack.
  | Ldloc_1            -- ^ Pushes 1th local variable to the stack.
  | Ldloc_2            -- ^ Pushes 2th local variable to the stack.
  | Ldloc_3            -- ^ Pushes 3th local variable to the stack.
  | LdlocN DottedName  -- ^ Pushes value of local variable, specified by name, to the stack.
  | Ldloca Offset      -- ^ Pushes address of local variable, specified by index, to the stack.
  | LdlocaN DottedName -- ^ Pushes address of local variable, specified by name, to the stack.
  | Ldnull             -- ^ Pushes a size-agnostic null reference on the stack.
  | Ldsfld
      { fieldType    :: PrimitiveType  -- ^ Type of the field.
      , assemblyName :: AssemblyName   -- ^ Name of the assembly where the field resides.
      , typeName     :: TypeName       -- ^ Name of the type of which the field is a member.
      , fieldName    :: FieldName      -- ^ Name of the field.
      } -- ^ Pops type reference, find value of specified field on the type, pushes value to the stack.
  | Ldsflda
      { fieldType    :: PrimitiveType  -- ^ Type of the field.
      , assemblyName :: AssemblyName   -- ^ Name of the assembly where the field resides.
      , typeName     :: TypeName       -- ^ Name of the type of which the field is a member.
      , fieldName    :: FieldName      -- ^ Name of the field.
      } -- ^ Pops type reference, find address of specified field on the type, pushes address to the stack.
  | Ldstr String       -- ^ Pushes an object reference to the specified string constant.
  | Ldtoken PrimitiveType -- ^ Pushes the RuntimeTypeHandle of the specified type.
  | Ldobj PrimitiveType -- ^ Copies the value type object pointed to by an address to the top of the evaluation stack.
  | Mul                -- ^ Pops 2 values, multiplies the values, pushes result.
  | Mul_ovf            -- ^ Pops 2 values, multiplies the values with a signed overflow check, pushes result.
  | Mul_ovf_un         -- ^ Pops 2 values, multiplies the values with an unsigned overflow check, pushes result.
  | Neg                -- ^ Pops 1 value, negates the value, pushes the value.
  | Newobj
      { returnType   :: PrimitiveType    -- ^ Return type of the constructor (almost alway Void).
      , assemblyName :: AssemblyName     -- ^ Name of the assembly where the constructor resides.
      , typeName     :: TypeName         -- ^ Name of the type of which the constructor is a member.
      , paramTypes   :: [PrimitiveType]  -- ^ Types of the formal paramters of the constructor.
      } -- ^ Creates a new object or instance of a value type. Pops /n/ values, calls the specified constructor, pushes a new object reference onto the stack (where /n/ is the number of formal parameters of the constructor).
  | Newarr PrimitiveType -- ^ Creates a new one-dimensional array. Pops a native int or int32, pushes a new array with that length.
  | Nop                -- ^ No operation is performed.
  | Not                -- ^ Pops 1 value, does a bitwise complement, pushes result.
  | Or                 -- ^ Pops 2 values, do bitwise OR between the values, pushes result.
  | Pop                -- ^ Pops the top of the stack.
  | Rem                -- ^ Pops 2 values, divides the first value by the second value, pushes the remainder.
  | Rem_un             -- ^ Pops 2 integers, divides the first by the second when considered as unsigned integers, pushes the remainder.
  | Ret                -- ^ Returns from the current method. Pushes top of the stack to the top of the callers stack (if stack is not empty).
  | Shl                -- ^ Pops 2 values, shifts the first to the left by the number of bits specified by the second, pushes the result.
  | Shr                -- ^ Pops 2 values, shifts the first to the right by the number of bits specified by the second (with sign extension), pushes the result.
  | Shr_un             -- ^ Pops 2 values, shifts the first to the right by the number of bits specified by the second (without sign extension), pushes the result.
  | Stelem_i           -- ^ Pops an array reference, an index, and a native integer. Stores the integer in the array.
  | Stelem_i1          -- ^ Pops an array reference, an index, and an 8-bit integer. Stores the integer in the array.
  | Stelem_i2          -- ^ Pops an array reference, an index, and a 16-bit integer. Stores the integer in the array.
  | Stelem_i4          -- ^ Pops an array reference, an index, and a 32-bit integer. Stores the integer in the array.
  | Stelem_i8          -- ^ Pops an array reference, an index, and a 64-bit integer. Stores the integer in the array.
  | Stelem_r4          -- ^ Pops an array reference, an index, and a float. Stores the float in the array.
  | Stelem_r8          -- ^ Pops an array reference, an index, and a double integer. Stores the double in the array.
  | Stelem_ref          -- ^ Pops an array reference, an index, and an object reference. Stores the object reference in the array.
  | Stfld
      { fieldType    :: PrimitiveType  -- ^ Type of the field.
      , assemblyName :: AssemblyName   -- ^ Name of the assembly where the field resides.
      , typeName     :: TypeName       -- ^ Name of the type of which the field is a member.
      , fieldName    :: FieldName      -- ^ Name of the field.
      } -- ^ Replaces the value stored in the field of an object reference or pointer with a new value.
  | Stind_i            -- ^ Pops an address and a native integer, stores the integer at the address.
  | Stind_i1           -- ^ Pops an address and a 8-bit integer, stores the integer at the address.
  | Stind_i2           -- ^ Pops an address and a 16-bit integer, stores the integer at the address.
  | Stind_i4           -- ^ Pops an address and a 32-bit integer, stores the integer at the address.
  | Stind_i8           -- ^ Pops an address and a 64-bit integer, stores the integer at the address.
  | Stind_r4           -- ^ Pops an address and a 32-bit float, stores the float at the address.
  | Stind_r8           -- ^ Pops an address and a 64-bit double, stores the double at the address.
  | Stind_ref          -- ^ Pops an address and an object reference, stores the object reference at the address.
  | Stloc Offset       -- ^ Pops 1 value, stores it in the local variable specified by index.
  | Stloc_0            -- ^ Pops 1 value, stores it in the 0th local variable.
  | Stloc_1            -- ^ Pops 1 value, stores it in the 1th local variable.
  | Stloc_2            -- ^ Pops 1 value, stores it in the 2th local variable.
  | Stloc_3            -- ^ Pops 1 value, stores it in the 3th local variable.
  | StlocN DottedName  -- ^ Pops 1 value, stores it in the local variable specified by name.
  | Stobj PrimitiveType -- ^ Copies a value of a specified type from the evaluation stack into a supplied memory address.
  | Stsfld
      { fieldType    :: PrimitiveType  -- ^ Type of the field.
      , assemblyName :: AssemblyName   -- ^ Name of the assembly where the field resides.
      , typeName     :: TypeName       -- ^ Name of the type of which the field is a member.
      , fieldName    :: FieldName      -- ^ Name of the field.
      } -- ^ Replaces the value stored in the static field of a type with a new value.
  | Sub                -- ^ Pops 2 values, subtracts second value from the first value, pushes result.
  | Sub_ovf            -- ^ Pops 2 values, subtracts second value from the first value with a signed overflow check, pushes result.
  | Sub_ovf_un         -- ^ Pops 2 values, subtracts second value from the first value with an unsigned overflow check, pushes result.
  | Switch [Label]     -- ^ Pops an unsigned integer value and transfers control to label with matching index.
  | Tail               -- ^ Performs subsequent call as a tail call, by replacing current stack frame with callee stack frame.
  | Tailcall OpCode    -- ^ Performs provided call as a tail call, by replacing current stack frame with callee stack frame.
  | Throw              -- ^ Pops an object reference from the stack and throws it as an exception.
  | Unaligned Alignment -- ^ Performs subsequent load or store operation under a weaker-than-usual alignment precondition.
  | UnalignedPtr  Alignment OpCode -- ^ Performs provided load or store operation under a weaker-than-usual alignment precondition.
  | Unbox PrimitiveType  -- ^ Pops 1 value, unboxes object reference, pushes value type.
  | Unbox_any PrimitiveType  -- ^ Pops 1 value, unboxes and loads value, equivalent to unbox followed by ldobj
  | Volatile           -- ^ Marks subsequent pointer reference as volatile, i.e. the value it points at can be modified from another thread.
  | VolatilePtr OpCode -- ^ Marks provided pointer reference as volatile, i.e. the value it points at can be modified from another thread.
  | Xor                -- ^ Pops 2 values, do bitwise XOR between the values, pushes result.

-}
