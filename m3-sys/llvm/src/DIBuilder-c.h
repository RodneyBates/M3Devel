//===--- llvm/DIBuilder-c.h - Debug Information Builder ---------*- C   -*-===//
//       A C binding to things declared in llvm's DIBuilder.h,                //
//       from which it is derived.                                            //
//                                                                            //


//===--- llvm/DIBuilder.h - Debug Information Builder -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a DIBuilder that is useful for creating debugging
// information entries in LLVM IR form.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_DIBUILDER_C_H
#define LLVM_DIBUILDER_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/ValueHandle.h"

/* The debug info nodes themselves: */ 
/// NOTE: Unlike regular llvm IR nodes, these are very small classes that 
/// contain a pointer to an MDNode.  They are passed in and returned by value, 
/// as structs, with no pointer thereto. 

struct DIBasicType;
struct DICompileUnit;
struct DICompositeType;
struct DIDerivedType;
struct DIDescriptor;
struct DIFile;
struct DIEnumerator;
struct DIType;
struct DIArray;
struct DIGlobalVariable;
struct DIImportedEntity;
struct DINameSpace;
struct DIVariable;
struct DISubrange;
struct DILexicalBlockFile;
struct DILexicalBlock;
struct DIScope;
struct DISubprogram;
struct DITemplateTypeParameter;
struct DITemplateValueParameter;
struct DIObjCProperty;

/* Other llvm types needed as parameters to functions bound here. */ 

struct BasicBlock;
struct Instruction;
struct Function;
struct Module;
struct Value;
struct MDNode;
struct LLVMStringRef {char *Data; size_t Length; };
struct ArrayRefOfValueRef {Value *Data; size_t Length; };
enum ComplexAddrKind { OpPlus=1, OpDeref };


/* The debug info builder class, whose member functions are bound here as
   ordinary C functions. */ 
struct DIBuilder; 

DIBuilder DIBBuilderCreate(ModuleRef Mod) {%1\n};

/* The debug info node constructors: */ 

/* FIXME: This is an overload, not done in M3DIB */
DITemplateValueParameter
DIBcreateTemplateValueParameter(DIBuilder *Builder, 
                                unsigned Tag,  
                                DIDescriptor Scope,
                                LLVMStringRef Name,  
                                DIType Ty,  
                                Value *Val,
                                MDNode *File = 0,  
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) {%1\n}; 

/// finalize - Construct any deferred debug info descriptors.
void DIBfinalize(DIBuilder *Builder) ;

/// createCompileUnit - A CompileUnit provides an anchor for all debugging
/// information generated during this instance of compilation.
/// @param Lang     Source programming language, eg. dwarf::DW_LANG_C99
/// @param File     File name
/// @param Dir      Directory
/// @param Producer String identify producer of debugging information.
///                 Usually this is a compiler version string.
/// @param isOptimized A boolean flag which indicates whether optimization
///                    is ON or not.
/// @param Flags    This string lists command line options. This string is
///                 directly embedded in debug info output which may be used
///                 by a tool analyzing generated debugging information.
/// @param RV       This indicates runtime version for languages like
///                 Objective-C.
/// @param SplitName The name of the file that we'll split debug info out
///                  into.
DICompileUnit DIBcreateCompileUnit(DIBuilder *Builder,  
                                unsigned Lang,  
                                LLVMStringRef File,
                                LLVMStringRef Dir,  
                                LLVMStringRef Producer,
                                bool isOptimized,  
                                LLVMStringRef Flags,
                                unsigned RV,
                                LLVMStringRef SplitName) ;

/// createFile - Create a file descriptor to hold debugging information
/// for a file.
DIFile DIBcreateFile(DIBuilder *Builder,  
                                LLVMStringRef Filename,  
                                LLVMStringRef Directory) ;

/// createEnumerator - Create a single enumerator value.
DIEnumerator DIBcreateEnumerator(DIBuilder *Builder,
                                LLVMStringRef Name,
                                int64_t Val) ;

/// \brief Create a DWARF unspecified type.
DIBasicType DIBcreateUnspecifiedType(DIBuilder *Builder
                                LLVMStringRef Name) ;

/// \brief Create C++11 nullptr type.
DIBasicType DIBcreateNullPtrType(DIBuilder *Builder) ;

/// createBasicType - Create debugging information entry for a basic
/// type.
/// @param Name        Type name.
/// @param SizeInBits  Size of the type.
/// @param AlignInBits Type alignment.
/// @param Encoding    DWARF encoding code, e.g. dwarf::DW_ATE_float.
DIBasicType DIBcreateBasicType(DIBuilder *Builder,
                           LLVMStringRef Name,
                           uint64_t SizeInBits,
                           uint64_t AlignInBits,
                           unsigned Encoding) ;

/// createQualifiedType - Create debugging information entry for a qualified
/// type, e.g. 'const int'.
/// @param Tag         Tag identifing type, e.g. dwarf::TAG_volatile_type
/// @param FromTy      Base Type.
DIDerivedType DIBcreateQualifiedType(DIBuilder *Builder,
                                unsigned Tag,
                                DIType FromTy) ;

/// createPointerType - Create debugging information entry for a pointer.
/// @param PointeeTy   Type pointed by this pointer.
/// @param SizeInBits  Size.
/// @param AlignInBits Alignment. (optional)
/// @param Name        Pointer type name. (optional)
DIDerivedType DIBcreatePointerType(DIType PointeeTy,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits = 0,
                                LLVMStringRef Name = StringRef()) ;

/// \brief Create debugging information entry for a pointer to member.
/// @param PointeeTy Type pointed to by this pointer.
/// @param Class Type for which this pointer points to members of.
DIDerivedType DIBcreateMemberPointerType(DIBuilder *Builder,
                                DIType PointeeTy,
                                DIType Class) ;

/// createReferenceType - Create debugging information entry for a c++
/// style reference or rvalue reference type.
DIDerivedType DIBcreateReferenceType(DIBuilder *Builder,
                                unsigned Tag,
                                DIType RTy) ;

/// createTypedef - Create debugging information entry for a typedef.
/// @param Ty          Original type.
/// @param Name        Typedef name.
/// @param File        File where this type is defined.
/// @param LineNo      Line number.
/// @param Context     The surrounding context for the typedef.
DIDerivedType DIBcreateTypedef( DIBuilder *Builder,
                                DIType Ty,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNo,
                                DIDescriptor Context) ;

/// createFriend - Create debugging information entry for a 'friend'.
DIDerivedType DIBcreateFriend(  DIBuilder *Builder,
                                DIType Ty,
                                DIType FriendTy) ;

/// createInheritance - Create debugging information entry to establish
/// inheritance relationship between two types.
/// @param Ty           Original type.
/// @param BaseTy       Base type. Ty is inherits from base.
/// @param BaseOffset   Base offset.
/// @param Flags        Flags to describe inheritance attribute,
///                     e.g. private
DIDerivedType DIBcreateInheritance(DIBuilder *Builder,
                                DIType Ty,
                                DIType BaseTy,
                                uint64_t BaseOffset,
                                unsigned Flags) ;

/// createMemberType - Create debugging information entry for a member.
/// @param Scope        Member scope.
/// @param Name         Member name.
/// @param File         File where this member is defined.
/// @param LineNo       Line number.
/// @param SizeInBits   Member size.
/// @param AlignInBits  Member alignment.
/// @param OffsetInBits Member offset.
/// @param Flags        Flags to encode member attribute, e.g. private
/// @param Ty           Parent type.
DIDerivedType DIBcreateMemberType(DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNo,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                DIType Ty) ;

/// createStaticMemberType - Create debugging information entry for a
/// C++ static data member.
/// @param Scope      Member scope.
/// @param Name       Member name.
/// @param File       File where this member is declared.
/// @param LineNo     Line number.
/// @param Ty         Type of the static member.
/// @param Flags      Flags to encode member attribute, e.g. private.
/// @param Val        Const initializer of the member.
DIDerivedType DIBcreateStaticMemberType(DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNo,
                                DIType Ty,
                                unsigned Flags,
                                llvm::Value *Val) ;

/// createObjCIVar - Create debugging information entry for Objective-C
/// instance variable.
/// @param Name         Member name.
/// @param File         File where this member is defined.
/// @param LineNo       Line number.
/// @param SizeInBits   Member size.
/// @param AlignInBits  Member alignment.
/// @param OffsetInBits Member offset.
/// @param Flags        Flags to encode member attribute, e.g. private
/// @param Ty           Parent type.
/// @param PropertyName Name of the Objective C property associated with
///                     this ivar.
/// @param PropertyGetterName Name of the Objective C property getter
///                           selector.
/// @param PropertySetterName Name of the Objective C property setter
///                           selector.
/// @param PropertyAttributes Objective C property attributes.
DIDerivedType DIBcreateObjCIVar(DIBuilder *Builder,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNo,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                DIType Ty,
                                LLVMStringRef PropertyName = StringRef(),
                                LLVMStringRef PropertyGetterName = StringRef(),
                                LLVMStringRef PropertySetterName = StringRef(),
                                unsigned PropertyAttributes = 0) ;

/// createObjCIVar - Create debugging information entry for Objective-C
/// instance variable.
/// @param Name         Member name.
/// @param File         File where this member is defined.
/// @param LineNo       Line number.
/// @param SizeInBits   Member size.
/// @param AlignInBits  Member alignment.
/// @param OffsetInBits Member offset.
/// @param Flags        Flags to encode member attribute, e.g. private
/// @param Ty           Parent type.
/// @param PropertyNode Property associated with this ivar.
DIDerivedType DIBcreateObjCIVar(DIBuilder *Builder,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNo,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                DIType Ty,
                                MDNode *PropertyNode) ;

/// createObjCProperty - Create debugging information entry for Objective-C
/// property.
/// @param Name         Property name.
/// @param File         File where this property is defined.
/// @param LineNumber   Line number.
/// @param GetterName   Name of the Objective C property getter selector.
/// @param SetterName   Name of the Objective C property setter selector.
/// @param PropertyAttributes Objective C property attributes.
/// @param Ty           Type.
DIObjCProperty DIBcreateObjCProperty(DIBuilder *Builder,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNumber,
                                LLVMStringRef GetterName,
                                LLVMStringRef SetterName,
                                unsigned PropertyAttributes,
                                DIType Ty) ;

/// createClassType - Create debugging information entry for a class.
/// @param Scope        Scope in which this class is defined.
/// @param Name         class name.
/// @param File         File where this member is defined.
/// @param LineNumber   Line number.
/// @param SizeInBits   Member size.
/// @param AlignInBits  Member alignment.
/// @param OffsetInBits Member offset.
/// @param Flags        Flags to encode member attribute, e.g. private
/// @param Elements     class members.
/// @param VTableHolder Debug info of the base class that contains vtable
///                     for this type. This is used in
///                     DW_AT_containing_type. See DWARF documentation
///                     for more info.
/// @param TemplateParms Template type parameters.
/// @param UniqueIdentifier A unique identifier for the class.
DICompositeType DIBcreateClassType(DIBuilder *Builder,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                DIType DerivedFrom,
                                DIArray Elements,
                                DIType VTableHolder = DIType(),
                                MDNode *TemplateParms = 0,
                                LLVMStringRef UniqueIdentifier = StringRef()) ;

/// createStructType - Create debugging information entry for a struct.
/// @param Scope        Scope in which this struct is defined.
/// @param Name         Struct name.
/// @param File         File where this member is defined.
/// @param LineNumber   Line number.
/// @param SizeInBits   Member size.
/// @param AlignInBits  Member alignment.
/// @param Flags        Flags to encode member attribute, e.g. private
/// @param Elements     Struct elements.
/// @param RunTimeLang  Optional parameter, Objective-C runtime version.
/// @param UniqueIdentifier A unique identifier for the struct.
DICompositeType DIBcreateStructType(DIBuilder *Builder,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                unsigned Flags,
                                DIType DerivedFrom,
                                DIArray Elements,
                                unsigned RunTimeLang = 0,
                                DIType VTableHolder = DIType(),
                                LLVMStringRef UniqueIdentifier = StringRef()) ;

/// createUnionType - Create debugging information entry for an union.
/// @param Scope        Scope in which this union is defined.
/// @param Name         Union name.
/// @param File         File where this member is defined.
/// @param LineNumber   Line number.
/// @param SizeInBits   Member size.
/// @param AlignInBits  Member alignment.
/// @param Flags        Flags to encode member attribute, e.g. private
/// @param Elements     Union elements.
/// @param RunTimeLang  Optional parameter, Objective-C runtime version.
/// @param UniqueIdentifier A unique identifier for the union.
DICompositeType DIBcreateUnionType(DIBuilder *Builder,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                unsigned Flags,
                                DIArray Elements,
                                unsigned RunTimeLang = 0,
                                LLVMStringRef UniqueIdentifier = StringRef()) ;

/// createTemplateTypeParameter - Create debugging information for template
/// type parameter.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Type parameter name.
/// @param Ty           Parameter type.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
DITemplateTypeParameter DIBcreateTemplateTypeParameter(DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIType Ty,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) ;

/// createTemplateValueParameter - Create debugging information for template
/// value parameter.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Value parameter name.
/// @param Ty           Parameter type.
/// @param Val          Constant parameter value.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
DITemplateValueParameter DIBcreateTemplateValueParameter(DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIType Ty,
                                Value *Val,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) ;

/// \brief Create debugging information for a template template parameter.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Value parameter name.
/// @param Ty           Parameter type.
/// @param Val          The fully qualified name of the template.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
DITemplateValueParameter DIBcreateTemplateTemplateParameter(DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIType Ty,
                                LLVMStringRef Val,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) ;

/// \brief Create debugging information for a template parameter pack.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Value parameter name.
/// @param Ty           Parameter type.
/// @param Val          An array of types in the pack.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
DITemplateValueParameter DIBcreateTemplateParameterPack(DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIType Ty,
                                DIArray Val,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) ;

/// createArrayType - Create debugging information entry for an array.
/// @param Size         Array size.
/// @param AlignInBits  Alignment.
/// @param Ty           Element type.
/// @param Subscripts   Subscripts.
DICompositeType DIBcreateArrayType(DIBuilder *Builder,
                                uint64_t Size,
                                uint64_t AlignInBits,
                                DIType Ty,
                                DIArray Subscripts) ;

/// createVectorType - Create debugging information entry for a vector type.
/// @param Size         Array size.
/// @param AlignInBits  Alignment.
/// @param Ty           Element type.
/// @param Subscripts   Subscripts.
DICompositeType DIBcreateVectorType(DIBuilder *Builder,
                                uint64_t Size,
                                uint64_t AlignInBits,
                                DIType Ty,
                                DIArray Subscripts) ;

/// createEnumerationType - Create debugging information entry for an
/// enumeration.
/// @param Scope          Scope in which this enumeration is defined.
/// @param Name           Union name.
/// @param File           File where this member is defined.
/// @param LineNumber     Line number.
/// @param SizeInBits     Member size.
/// @param AlignInBits    Member alignment.
/// @param Elements       Enumeration elements.
/// @param UnderlyingType Underlying type of a C++11/ObjC fixed enum.
/// @param UniqueIdentifier A unique identifier for the enum.
DICompositeType DIBcreateEnumerationType(DIBuilder *Builder,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                DIArray Elements,
                                DIType UnderlyingType,
                                LLVMStringRef UniqueIdentifier = StringRef()) ;

/// createSubroutineType - Create subroutine type.
/// @param File           File in which this subroutine is defined.
/// @param ParameterTypes An array of subroutine parameter types. This
///                       includes return type at 0th index.
DICompositeType DIBcreateSubroutineType(DIBuilder *Builder,
                                DIFile File,
                                DIArray ParameterTypes) ;

/// createArtificialType - Create a new DIType with "artificial" flag set.
DIType DIBcreateArtificialType( DIBuilder *Builder,
                                DIType Ty) ;

/// createObjectPointerType - Create a new DIType with the "object pointer"
/// flag set.
DIType DIBcreateObjectPointerType(DIBuilder *Builder,
                                DIType Ty) ;

/// createForwardDecl - Create a temporary forward-declared type.
DICompositeType DIBcreateForwardDecl(DIBuilder *Builder,
                                unsigned Tag,
                                LLVMStringRef Name,
                                DIDescriptor Scope,
                                DIFile F,
                                unsigned Line,
                                unsigned RuntimeLang = 0,
                                uint64_t SizeInBits = 0,
                                uint64_t AlignInBits = 0,
                                LLVMStringRef UniqueIdentifier = StringRef()) ;

/// retainType - Retain DIType in a module even if it is not referenced
/// through debug info anchors.
void DIBretainType(DIBuilder *Builder,DIType T) ;

/// createUnspecifiedParameter - Create unspeicified type descriptor
/// for a subroutine type.
DIDescriptor DIBcreateUnspecifiedParameter(DIBuilder *Builder) ;

/// getOrCreateArray - Get a DIArray, create one if required.
DIArray DIBgetOrCreateArray(DIBuilder *Builder, 
                                ArrayRefOfValueRef Elements) ;

/// getOrCreateSubrange - Create a descriptor for a value range.  This
/// implicitly uniques the values returned.
DISubrange DIBgetOrCreateSubrange(DIBuilder *Builder,
                                int64_t Lo,
                                int64_t Count) ;

/// createGlobalVariable - Create a new descriptor for the specified global.
/// @param Name        Name of the variable.
/// @param File        File where this variable is defined.
/// @param LineNo      Line number.
/// @param Ty          Variable Type.
/// @param isLocalToUnit Boolean flag indicate whether this variable is
///                      externally visible or not.
/// @param Val         llvm::Value of the variable.
DIGlobalVariable DIBcreateGlobalVariable(DIBuilder *Builder,
                                LLVMStringRef Name,
                                DIFile File,
\                               unsigned LineNo,
                                DIType Ty,
                                bool isLocalToUnit,
                                llvm::Value *Val) ;

/// \brief Create a new descriptor for the specified global.
/// @param Name        Name of the variable.
/// @param LinkageName Mangled variable name.
/// @param File        File where this variable is defined.
/// @param LineNo      Line number.
/// @param Ty          Variable Type.
/// @param isLocalToUnit Boolean flag indicate whether this variable is
///                      externally visible or not.
/// @param Val         llvm::Value of the variable.
DIGlobalVariable DIBcreateGlobalVariable(DIBuilder *Builder,
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                DIFile File,
                                unsigned LineNo,
                                DIType Ty,
                                bool isLocalToUnit,
                                llvm::Value *Val) ;

/// createStaticVariable - Create a new descriptor for the specified
/// variable.
/// @param Context     Variable scope.
/// @param Name        Name of the variable.
/// @param LinkageName Mangled  name of the variable.
/// @param File        File where this variable is defined.
/// @param LineNo      Line number.
/// @param Ty          Variable Type.
/// @param isLocalToUnit Boolean flag indicate whether this variable is
///                      externally visible or not.
/// @param Val         llvm::Value of the variable.
/// @param Decl        Reference to the corresponding declaration.
DIGlobalVariable DIBcreateStaticVariable(DIBuilder *Builder,
                                DIDescriptor Context,
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                DIFile File,
                                unsigned LineNo,
                                DIType Ty,
                                bool isLocalToUnit,
                                llvm::Value *Val,
                                MDNode *Decl = NULL) ;


/// createLocalVariable - Create a new descriptor for the specified
/// local variable.
/// @param Tag         Dwarf TAG. Usually DW_TAG_auto_variable or
///                    DW_TAG_arg_variable.
/// @param Scope       Variable scope.
/// @param Name        Variable name.
/// @param File        File where this variable is defined.
/// @param LineNo      Line number.
/// @param Ty          Variable Type
/// @param AlwaysPreserve Boolean. Set to true if debug info for this
///                       variable should be preserved in optimized build.
/// @param Flags          Flags, e.g. artificial variable.
/// @param ArgNo       If this variable is an argument then this argument's
///                    number. 1 indicates 1st argument.
DIVariable DIBcreateLocalVariable(DIBuilder *Builder,
                                unsigned Tag,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNo,
                                DIType Ty,
                                bool AlwaysPreserve = false,
                                unsigned Flags = 0,
                                unsigned ArgNo = 0) ;


/// createComplexVariable - Create a new descriptor for the specified
/// variable which has a complex address expression for its address.
/// @param Tag         Dwarf TAG. Usually DW_TAG_auto_variable or
///                    DW_TAG_arg_variable.
/// @param Scope       Variable scope.
/// @param Name        Variable name.
/// @param F           File where this variable is defined.
/// @param LineNo      Line number.
/// @param Ty          Variable Type
/// @param Addr        An array of complex address operations.
/// @param ArgNo       If this variable is an argument then this argument's
///                    number. 1 indicates 1st argument.
DIVariable DIBcreateComplexVariable(DIBuilder *Builder,
                                unsigned Tag,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile F,
                                unsigned LineNo,
                                DIType Ty,
                                ArrayRefOfValueRef Addr,
                                unsigned ArgNo = 0) ;

/// createFunction - Create a new descriptor for the specified subprogram.
/// See comments in DISubprogram for descriptions of these fields.
/// @param Scope         Function scope.
/// @param Name          Function name.
/// @param LinkageName   Mangled function name.
/// @param File          File where this variable is defined.
/// @param LineNo        Line number.
/// @param Ty            Function type.
/// @param isLocalToUnit True if this function is not externally visible..
/// @param isDefinition  True if this is a function definition.
/// @param ScopeLine     Set to the beginning of the scope this starts
/// @param Flags         e.g. is this function prototyped or not.
///                      This flags are used to emit dwarf attributes.
/// @param isOptimized   True if optimization is ON.
/// @param Fn            llvm::Function pointer.
/// @param TParam        Function template parameters.
DISubprogram DIBcreateFunction( DIBuilder *Builder,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                DIFile File,
                                unsigned LineNo,
                                DICompositeType Ty,
                                bool isLocalToUnit,
                                bool isDefinition,
                                unsigned ScopeLine,
                                unsigned Flags = 0,
                                bool isOptimized = false,
                                Function *Fn = 0,
                                MDNode *TParam = 0,
                                MDNode *Decl = 0) ;

/// FIXME: this is added for dragonegg. Once we update dragonegg
/// to call resolve function, this will be removed.
DISubprogram DIBcreateFunction(DIBuilder *Builder,
                            DIScopeRef Scope,
                            LLVMStringRef Name,
                            LLVMStringRef LinkageName,
                            DIFile File, unsigned LineNo,
                            DICompositeType Ty, bool isLocalToUnit,
                            bool isDefinition,
                            unsigned ScopeLine,
                            unsigned Flags = 0,
                            bool isOptimized = false,
                            Function *Fn = 0,
                            MDNode *TParam = 0,
                            MDNode *Decl = 0) ;

/// createMethod - Create a new descriptor for the specified C++ method.
/// See comments in DISubprogram for descriptions of these fields.
/// @param Scope         Function scope.
/// @param Name          Function name.
/// @param LinkageName   Mangled function name.
/// @param File          File where this variable is defined.
/// @param LineNo        Line number.
/// @param Ty            Function type.
/// @param isLocalToUnit True if this function is not externally visible..
/// @param isDefinition  True if this is a function definition.
/// @param Virtuality    Attributes describing virtualness. e.g. pure
///                      virtual function.
/// @param VTableIndex   Index no of this method in virtual table.
/// @param VTableHolder  Type that holds vtable.
/// @param Flags         e.g. is this function prototyped or not.
///                      This flags are used to emit dwarf attributes.
/// @param isOptimized   True if optimization is ON.
/// @param Fn            llvm::Function pointer.
/// @param TParam        Function template parameters.
DISubprogram DIBcreateMethod(   DIBuilder *Builder, 
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                DIFile File,
                                unsigned LineNo,
                                DICompositeType Ty,
                                bool isLocalToUnit,
                                bool isDefinition,
                                unsigned Virtuality = 0,
                                unsigned VTableIndex = 0,
                                DIType VTableHolder = DIType(),
                                unsigned Flags = 0,
                                bool isOptimized = false,
                                Function *Fn = 0,
                                MDNode *TParam = 0) ;

/// createNameSpace - This creates new descriptor for a namespace
/// with the specified parent scope.
/// @param Scope       Namespace scope
/// @param Name        Name of this namespace
/// @param File        Source file
/// @param LineNo      Line number
DINameSpace DIBcreateNameSpace( DIBuilder *Builder,
                                DIDescriptor Scope,
                                LLVMStringRef Name,
                                DIFile File,
                                unsigned LineNo) ;


/// createLexicalBlockFile - This creates a descriptor for a lexical
/// block with a new file attached. This merely extends the existing
/// lexical block as it crosses a file.
/// @param Scope       Lexical block.
/// @param File        Source file.
DILexicalBlockFile DIBcreateLexicalBlockFile(DIBuilder *Builder,
                                DIDescriptor Scope,
                                DIFile File) ;

/// createLexicalBlock - This creates a descriptor for a lexical block
/// with the specified parent context.
/// @param Scope       Parent lexical scope.
/// @param File        Source file
/// @param Line        Line number
/// @param Col         Column number
DILexicalBlock DIBcreateLexicalBlock(DIBuilder *Builder,
                                DIDescriptor Scope,
                                DIFile File,
                                unsigned Line,
                                unsigned Col) ;

/// \brief Create a descriptor for an imported module.
/// @param Context The scope this module is imported into
/// @param NS The namespace being imported here
/// @param Line Line number
DIImportedEntity DIBcreateImportedModule(DIBuilder *Builder,
                                DIScope Context,
                                DINameSpace NS,
                                unsigned Line,
                                LLVMStringRef Name = StringRef()) ;

/// \brief Create a descriptor for an imported module.
/// @param Context The scope this module is imported into
/// @param NS An aliased namespace
/// @param Line Line number
DIImportedEntity DIBcreateImportedModule(DIBuilder *Builder,
                                DIScope Context,
                                DIImportedEntity NS,
                                unsigned Line,
                                LLVMStringRef Name) ;

/// \brief Create a descriptor for an imported function.
/// @param Context The scope this module is imported into
/// @param Decl The declaration (or definition) of a function, type, or
///             variable
/// @param Line Line number
DIImportedEntity DIBcreateImportedDeclaration(DIBuilder *Builder,
                                DIScope Context,
                                DIDescriptor Decl,
                                unsigned Line) ;

/// insertDeclare - Insert a new llvm.dbg.declare intrinsic call.
/// @param Storage     llvm::Value of the variable
/// @param VarInfo     Variable's debug info descriptor.
/// @param InsertAtEnd Location for the new intrinsic.
Instruction *DIBinsertDeclare(DIBuilder *Builder,
                                llvm::Value *Storage,
                                DIVariable VarInfo,
                                BasicBlock *InsertAtEnd) ;

/// insertDeclare - Insert a new llvm.dbg.declare intrinsic call.
/// @param Storage      llvm::Value of the variable
/// @param VarInfo      Variable's debug info descriptor.
/// @param InsertBefore Location for the new intrinsic.
Instruction *DIBinsertDeclare(DIBuilder *Builder,
                                llvm::Value *Storage,
                                DIVariable VarInfo,
                                Instruction *InsertBefore) ;


/// insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
/// @param Val          llvm::Value of the variable
/// @param Offset       Offset
/// @param VarInfo      Variable's debug info descriptor.
/// @param InsertAtEnd Location for the new intrinsic.
Instruction *DIBinsertDbgValueIntrinsic(DIBuilder *Builder,
                                llvm::Value *Val,
                                uint64_t Offset,
                                DIVariable VarInfo,
                                BasicBlock *InsertAtEnd) ;

/// insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
/// @param Val          llvm::Value of the variable
/// @param Offset       Offset
/// @param VarInfo      Variable's debug info descriptor.
/// @param InsertBefore Location for the new intrinsic.
Instruction *DIBinsertDbgValueIntrinsic(DIBuilder *Builder,
                                llvm::Value *Val,
                                uint64_t Offset,
                                DIVariable VarInfo,
                                Instruction *InsertBefore) ;


#ifdef __cplusplus
}
#endif /* !defined(__cplusplus) */

#endif /* !defined(LLVM_C_CORE_H) */
