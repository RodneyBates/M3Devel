//===--- llvm/DIBuilder-c.cpp - Debug Information Builder -------*- C++ -*-===//
//       C-callable wrappers of member functions in llvm's  DIBuilder.cpp,    //
//       Derived from DIBuilder.h.                                            //
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

#include "DIBuilder-c.h" 

//#include "llvm/ADT/ArrayRef.h"
//#include "llvm/ADT/StringRef.h"
//#include "llvm/DebugInfo.h"
//#include "llvm/Support/DataTypes.h"
//#include "llvm/Support/ValueHandle.h"

/* The debug info builder class, whose member functions are bound here as
   ordinary C functions. */ 

/// Constructor of a DIBuilder.  
LLVMDIBuilder DIBBuilderCreate(ModuleRef Mod) {%1
    return wrap(llvm::DIBuilder::DIBuilder(unwrap(Mod))); 
};

/// finalize - Construct any deferred debug info descriptors.
void DIBfinalize(LLVMDIBuilder *Builder) {%1
  unwrap(Builder)-> finalize();
};

/* The debug info node constructors: */ 

/// NOTE: Unlike regular llvm IR nodes, the DI* types are small classes that 
/// contain a pointer to an MDNode.  They are passed in and returned by value, 
/// as structs, with no pointer thereto.  Thus, they require no [un]wrap. 


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
LLVMDICompileUnit DIBcreateCompileUnit(LLVMDIBuilder *Builder,  
                                unsigned Lang,  
                                LLVMStringRef File,
                                LLVMStringRef Dir,  
                                LLVMStringRef Producer,
                                bool isOptimized,  
                                LLVMStringRef Flags,
                                unsigned RV,
                                LLVMStringRef SplitName = StringRef()) {%1
  return unwrap(Builder)-> createCompileUnit(
                                /*unsigned*/ Lang,  
                                /*LLVMStringRef*/ File,
                                /*LLVMStringRef*/ Dir,  
                                /*LLVMStringRef*/ Producer,
                                /*bool*/ isOptimized,  
                                /*LLVMStringRef*/ Flags,
                                /*unsigned*/ RV,
                                /*LLVMStringRef*/ SplitName);
};

/// createFile - Create a file descriptor to hold debugging information
/// for a file.
LLVMDIFile DIBcreateFile(LLVMDIBuilder *Builder,  
                                LLVMStringRef Filename,  
                                LLVMStringRef Directory) {%1
  return unwrap(Builder)-> createFile(
                                /*LLVMStringRef*/ Filename,  
                                /*LLVMStringRef*/ Directory);
};

/// createEnumerator - Create a single enumerator value.
LLVMDIEnumerator DIBcreateEnumerator(LLVMDIBuilder *Builder,
                                LLVMStringRef Name,
                                int64_t Val) {%1
  return unwrap(Builder)-> createEnumerator(
                                /*LLVMStringRef*/ Name,
                                /*int64_t*/ Val);
};

/// \brief Create a DWARF unspecified type.
LLVMDIBasicType DIBcreateUnspecifiedType(LLVMDIBuilder *Builder
                                LLVMStringRef Name) {%1
  return unwrap(Builder)-> createUnspecifiedType(
                                /*LLVMStringRef*/ Name);
};

/// \brief Create C++11 nullptr type.
LLVMDIBasicType DIBcreateNullPtrType(LLVMDIBuilder *Builder) {%1
  return unwrap(Builder)-> createNullPtrType();
};

/// createBasicType - Create debugging information entry for a basic
/// type.
/// @param Name        Type name.
/// @param SizeInBits  Size of the type.
/// @param AlignInBits Type alignment.
/// @param Encoding    DWARF encoding code, e.g. dwarf::DW_ATE_float.
LLVMDIBasicType DIBcreateBasicType(LLVMDIBuilder *Builder,
                           LLVMStringRef Name,
                           uint64_t SizeInBits,
                           uint64_t AlignInBits,
                           unsigned Encoding) {%1
  return unwrap(Builder)-> createBasicType(
                           /*LLVMStringRef*/ Name,
                           /*uint64_t*/ SizeInBits,
                           /*uint64_t*/ AlignInBits,
                           /*unsigned*/ Encoding);
};

/// createQualifiedType - Create debugging information entry for a qualified
/// type, e.g. 'const int'.
/// @param Tag         Tag identifing type, e.g. dwarf::TAG_volatile_type
/// @param FromTy      Base Type.
LLVMDIDerivedType DIBcreateQualifiedType(LLVMDIBuilder *Builder,
                                unsigned Tag,
                                LLVMDIType FromTy) {%1
  return unwrap(Builder)-> createQualifiedType(
                                /*unsigned*/ Tag,
                                /*LLVMDIType*/ FromTy);
};

/// createPointerType - Create debugging information entry for a pointer.
/// @param PointeeTy   Type pointed by this pointer.
/// @param SizeInBits  Size.
/// @param AlignInBits Alignment. (optional)
/// @param Name        Pointer type name. (optional)
LLVMDIDerivedType DIBcreatePointerType(LLVMDIBuilder *Builder,
                                LLVMDIType PointeeTy,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits = 0,
                                LLVMStringRef Name = StringRef()) {%1
  return unwrap(Builder)-> createPointerType(
                                /*LLVMDIType*/ PointeeTy,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits = 0,
                                /*LLVMStringRef*/ Name);
};

/// \brief Create debugging information entry for a pointer to member.
/// @param PointeeTy Type pointed to by this pointer.
/// @param Class Type for which this pointer points to members of.
LLVMDIDerivedType DIBcreateMemberPointerType(LLVMDIBuilder *Builder,
                                LLVMDIType PointeeTy,
                                LLVMDIType Class) {%1
  return unwrap(Builder)-> createMemberPointerType(
                                /*LLVMDIType*/ PointeeTy,
                                /*LLVMDIType*/ Class);
};

/// createReferenceType - Create debugging information entry for a c++
/// style reference or rvalue reference type.
LLVMDIDerivedType DIBcreateReferenceType(LLVMDIBuilder *Builder,
                                unsigned Tag,
                                LLVMDIType RTy) {%1
  return unwrap(Builder)-> createReferenceType(
                                /*unsigned*/ Tag,
                                /*LLVMDIType*/ RTy);
};

/// createTypedef - Create debugging information entry for a typedef.
/// @param Ty          Original type.
/// @param Name        Typedef name.
/// @param File        File where this type is defined.
/// @param LineNo      Line number.
/// @param Context     The surrounding context for the typedef.
LLVMDIDerivedType DIBcreateTypedef( LLVMDIBuilder *Builder,
                                LLVMDIType Ty,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDIDescriptor Context) {%1
  return unwrap(Builder)-> createTypedef(
                                /*LLVMDIType*/ Ty,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDIDescriptor*/ Context);
};

/// createFriend - Create debugging information entry for a 'friend'.
LLVMDIDerivedType DIBcreateFriend(  LLVMDIBuilder *Builder,
                                LLVMDIType Ty,
                                LLVMDIType FriendTy) {%1
  return unwrap(Builder)-> createFriend(
                                /*LLVMDIType*/ Ty,
                                /*LLVMDIType*/ FriendTy);
};

/// createInheritance - Create debugging information entry to establish
/// inheritance relationship between two types.
/// @param Ty           Original type.
/// @param BaseTy       Base type. Ty is inherits from base.
/// @param BaseOffset   Base offset.
/// @param Flags        Flags to describe inheritance attribute,
///                     e.g. private
LLVMDIDerivedType DIBcreateInheritance(LLVMDIBuilder *Builder,
                                LLVMDIType Ty,
                                LLVMDIType BaseTy,
                                uint64_t BaseOffset,
                                unsigned Flags) {%1
  return unwrap(Builder)-> createInheritance(
                                /*LLVMDIType*/ Ty,
                                /*LLVMDIType*/ BaseTy,
                                /*uint64_t*/ BaseOffset,
                                /*unsigned*/ Flags);
}:


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
LLVMDIDerivedType DIBcreateMemberType(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                LLVMDIType Ty) {%1
  return unwrap(Builder)-> createMemberType(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                /*uint64_t*/ OffsetInBits,
                                /*unsigned*/ Flags,
                                /*LLVMDIType*/ Ty);
};


/// createStaticMemberType - Create debugging information entry for a
/// C++ static data member.
/// @param Scope      Member scope.
/// @param Name       Member name.
/// @param File       File where this member is declared.
/// @param LineNo     Line number.
/// @param Ty         Type of the static member.
/// @param Flags      Flags to encode member attribute, e.g. private.
/// @param Val        Const initializer of the member.
LLVMDIDerivedType DIBcreateStaticMemberType(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDIType Ty,
                                unsigned Flags,
                                Value *Val) {%1
  return unwrap(Builder)-> createStaticMemberType(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDIType*/ Ty,
                                /*unsigned*/ Flags,
                                unwrap(/*Value **/ Val));
};

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
LLVMDIDerivedType DIBcreateObjCIVarFromNames(LLVMDIBuilder *Builder,
              /* ^Renamed to eliminate overload. */
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                LLVMDIType Ty,
                                LLVMStringRef PropertyName = StringRef(),
                                LLVMStringRef PropertyGetterName = StringRef(),
                                LLVMStringRef PropertySetterName = StringRef(),
                                unsigned PropertyAttributes = 0) {%1
  return unwrap(Builder)-> createObjCIVar(
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                /*uint64_t*/ OffsetInBits,
                                /*unsigned*/ Flags,
                                /*LLVMDIType*/ Ty,
                                /*LLVMStringRef*/ PropertyName,
                                /*LLVMStringRef*/ PropertyGetterName,
                                /*LLVMStringRef*/ PropertySetterName,
                                /*unsigned*/ PropertyAttributes);
};


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
LLVMDIDerivedType DIBcreateObjCIVarFromPropertyNode(LLVMDIBuilder *Builder,
              /* ^Renamed to eliminate overload. */
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                LLVMDIType Ty,
                                MDNode *PropertyNode) {%1
  return unwrap(Builder)-> createObjCIVar(
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                /*uint64_t*/ OffsetInBits,
                                /*unsigned*/ Flags,
                                /*LLVMDIType*/ Ty,
                                unwrap(/*MDNode **/ PropertyNode);
};


/// createObjCProperty - Create debugging information entry for Objective-C
/// property.
/// @param Name         Property name.
/// @param File         File where this property is defined.
/// @param LineNumber   Line number.
/// @param GetterName   Name of the Objective C property getter selector.
/// @param SetterName   Name of the Objective C property setter selector.
/// @param PropertyAttributes Objective C property attributes.
/// @param Ty           Type.
LLVMDIObjCProperty DIBcreateObjCProperty(LLVMDIBuilder *Builder,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNumber,
                                LLVMStringRef GetterName,
                                LLVMStringRef SetterName,
                                unsigned PropertyAttributes,
                                LLVMDIType Ty) {%1
  return unwrap(Builder)-> createObjCProperty(
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNumber,
                                /*LLVMStringRef*/ GetterName,
                                /*LLVMStringRef*/ SetterName,
                                /*unsigned*/ PropertyAttributes,
                                /*LLVMDIType*/ Ty);
};


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
LLVMDICompositeType DIBcreateClassType(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                uint64_t OffsetInBits,
                                unsigned Flags,
                                LLVMDIType DerivedFrom,
                                DIArray Elements,
                                LLVMDIType VTableHolder = LLVMDIType(),
                                MDNode *TemplateParms = 0,
                                LLVMStringRef UniqueIdentifier = StringRef()) {%1
  return unwrap(Builder)-> createClassType(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNumber,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                /*uint64_t*/ OffsetInBits,
                                /*unsigned*/ Flags,
                                /*LLVMDIType*/ DerivedFrom,
                                DIArray Elements,
                                /*LLVMDIType*/ VTableHolder,
                                unwrap(/*MDNode **/ TemplateParms,
                                /*LLVMStringRef*/ UniqueIdentifier);
};


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
LLVMDICompositeType DIBcreateStructType(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                unsigned Flags,
                                LLVMDIType DerivedFrom,
                                DIArray Elements,
                                unsigned RunTimeLang = 0,
                                LLVMDIType VTableHolder = LLVMDIType(),
                                LLVMStringRef UniqueIdentifier = StringRef()) {%1
  return unwrap(Builder)-> createStructType(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNumber,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                /*unsigned*/ Flags,
                                /*LLVMDIType*/ DerivedFrom,
                                DIArray Elements,
                                /*unsigned*/ RunTimeLang,
                                /*LLVMDIType*/ VTableHolder,
                                /*LLVMStringRef*/ UniqueIdentifier);
};


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
LLVMDICompositeType DIBcreateUnionType(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                unsigned Flags,
                                DIArray Elements,
                                unsigned RunTimeLang = 0,
                                LLVMStringRef UniqueIdentifier = StringRef()) {%1
  return unwrap(Builder)-> createUnionType(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNumber,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                /*unsigned*/ Flags,
                                DIArray Elements,
                                /*unsigned*/ RunTimeLang,
                                /*LLVMStringRef*/ UniqueIdentifier);
};


/// createTemplateTypeParameter - Create debugging information for template
/// type parameter.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Type parameter name.
/// @param Ty           Parameter type.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
LLVMDITemplateTypeParameter DIBcreateTemplateTypeParameter(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIType Ty,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) {%1
  return unwrap(Builder)-> createTemplateTypeParameter(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIType*/ Ty,
                                unwrap(/*MDNode **/ File,
                                /*unsigned*/ LineNo,
                                /*unsigned*/ ColumnNo);
};


/// createTemplateValueParameter - Create debugging information for template
/// value parameter.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Value parameter name.
/// @param Ty           Parameter type.
/// @param Val          Constant parameter value.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
LLVMDITemplateValueParameter DIBcreateTemplateValueParameterNoTag(LLVMDIBuilder *Builder,
                         /* ^Renamed to eliminate overload. */
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIType Ty,
                                Value *Val,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) {%1
  return unwrap(Builder)-> createTemplateValueParameter(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIType*/ Ty,
                                unwrap(/*Value **/ Val),
                                unwrap(/*MDNode **/ File,
                                /*unsigned*/ LineNo,
                                /*unsigned*/ ColumnNo);
};

LLVMDITemplateValueParameter DIBcreateTemplateValueParameterWTag(LLVMDIBuilder *Builder, 
                         /* ^Renamed to eliminate overload. */
                                unsigned Tag,  
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,  
                                LLVMDIType Ty,  
                                Value *Val,
                                MDNode *File = 0,  
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) {%1
  return unwrap(Builder)-> createTemplateValueParameter(
                                /*unsigned*/ Tag,  
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,  
                                /*LLVMDIType*/ Ty,  
                                unwrap(/*Value */ Val),
                                unwrap(/*MDNode **/ File,
                                /*unsigned*/ LineNo,
                                /*unsigned*/ ColumnNo);
}; 



/// \brief Create debugging information for a template template parameter.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Value parameter name.
/// @param Ty           Parameter type.
/// @param Val          The fully qualified name of the template.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
LLVMDITemplateValueParameter DIBcreateTemplateTemplateParameter(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIType Ty,
                                LLVMStringRef Val,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) {%1
  return unwrap(Builder)-> createTemplateTemplateParameter(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIType*/ Ty,
                                /*LLVMStringRef*/ Val,
                                unwrap(/*MDNode **/ File,
                                /*unsigned*/ LineNo,
                                /*unsigned*/ ColumnNo);
};


/// \brief Create debugging information for a template parameter pack.
/// @param Scope        Scope in which this type is defined.
/// @param Name         Value parameter name.
/// @param Ty           Parameter type.
/// @param Val          An array of types in the pack.
/// @param File         File where this type parameter is defined.
/// @param LineNo       Line number.
/// @param ColumnNo     Column Number.
LLVMDITemplateValueParameter DIBcreateTemplateParameterPack(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIType Ty,
                                DIArray Val,
                                MDNode *File = 0,
                                unsigned LineNo = 0,
                                unsigned ColumnNo = 0) {%1
  return unwrap(Builder)-> createTemplateParameterPack(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIType*/ Ty,
                                DIArray Val,
                                unwrap(/*MDNode **/ File,
                                /*unsigned*/ LineNo,
                                /*unsigned*/ ColumnNo);
};


/// createArrayType - Create debugging information entry for an array.
/// @param Size         Array size.
/// @param AlignInBits  Alignment.
/// @param Ty           Element type.
/// @param Subscripts   Subscripts.
LLVMDICompositeType DIBcreateArrayType(LLVMDIBuilder *Builder,
                                uint64_t Size,
                                uint64_t AlignInBits,
                                LLVMDIType Ty,
                                DIArray Subscripts) {%1
  return unwrap(Builder)-> createArrayType(
                                /*uint64_t*/ Size,
                                /*uint64_t*/ AlignInBits,
                                /*LLVMDIType*/ Ty,
                                DIArray Subscripts);
};


/// createVectorType - Create debugging information entry for a vector type.
/// @param Size         Array size.
/// @param AlignInBits  Alignment.
/// @param Ty           Element type.
/// @param Subscripts   Subscripts.
LLVMDICompositeType DIBcreateVectorType(LLVMDIBuilder *Builder,
                                uint64_t Size,
                                uint64_t AlignInBits,
                                LLVMDIType Ty,
                                DIArray Subscripts) {%1
  return unwrap(Builder)-> createVectorType(
                                /*uint64_t*/ Size,
                                /*uint64_t*/ AlignInBits,
                                /*LLVMDIType*/ Ty,
                                DIArray Subscripts);
};


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
LLVMDICompositeType DIBcreateEnumerationType(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNumber,
                                uint64_t SizeInBits,
                                uint64_t AlignInBits,
                                DIArray Elements,
                                LLVMDIType UnderlyingType,
                                LLVMStringRef UniqueIdentifier = StringRef()) {%1
  return unwrap(Builder)-> createEnumerationType(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNumber,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                DIArray Elements,
                                /*LLVMDIType*/ UnderlyingType,
                                /*LLVMStringRef*/ UniqueIdentifier);
};


/// createSubroutineType - Create subroutine type.
/// @param File           File in which this subroutine is defined.
/// @param ParameterTypes An array of subroutine parameter types. This
///                       includes return type at 0th index.
LLVMDICompositeType DIBcreateSubroutineType(LLVMDIBuilder *Builder,
                                LLVMDIFile File,
                                DIArray ParameterTypes) {%1
  return unwrap(Builder)-> createSubroutineType(
                                /*LLVMDIFile*/ File,
                                DIArray ParameterTypes);
};


/// createArtificialType - Create a new LLVMDIType with "artificial" flag set.
LLVMDIType DIBcreateArtificialType( LLVMDIBuilder *Builder,
                                LLVMDIType Ty) {%1
  return unwrap(Builder)-> createArtificialType( /*LLVMDIType*/ Ty);
};


/// createObjectPointerType - Create a new LLVMDIType with the "object pointer"
/// flag set.
LLVMDIType DIBcreateObjectPointerType(LLVMDIBuilder *Builder,
                                LLVMDIType Ty) {%1
  return unwrap(Builder)-> createObjectPointerType( /*LLVMDIType*/ Ty);
};


/// createForwardDecl - Create a temporary forward-declared type.
LLVMDICompositeType DIBcreateForwardDecl(LLVMDIBuilder *Builder,
                                unsigned Tag,
                                LLVMStringRef Name,
                                LLVMDIDescriptor Scope,
                                LLVMDIFile F,
                                unsigned Line,
                                unsigned RuntimeLang = 0,
                                uint64_t SizeInBits = 0,
                                uint64_t AlignInBits = 0,
                                LLVMStringRef UniqueIdentifier = StringRef()) {%1
  return unwrap(Builder)-> createForwardDecl(
                                /*unsigned*/ Tag,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMDIFile*/ F,
                                /*unsigned*/ Line,
                                /*unsigned*/ RuntimeLang,
                                /*uint64_t*/ SizeInBits,
                                /*uint64_t*/ AlignInBits,
                                /*LLVMStringRef*/ UniqueIdentifier);
};


/// retainType - Retain DIType in a module even if it is not referenced
/// through debug info anchors.
void DIBretainType(LLVMDIBuilder *Builder, LLVMDIType T) {%1
  return unwrap(Builder)-> retainType( /*LLVMDIType*/ T);
};


/// createUnspecifiedParameter - Create unspeicified type descriptor
/// for a subroutine type.
LLVMDIDescriptor DIBcreateUnspecifiedParameter(LLVMDIBuilder *Builder) {%1
  return unwrap(Builder)-> createUnspecifiedParameter();
};


/// getOrCreateArray - Get a DIArray, create one if required.
DIArray DIBgetOrCreateArray(LLVMDIBuilder *Builder, 
                                ArrayRefOfValueRef Elements) {%1
  return unwrap(Builder)-> getOrCreateArray( /*ArrayRefOfValueRef*/ Elements);
};


/// getOrCreateSubrange - Create a descriptor for a value range.  This
/// implicitly uniques the values returned.
LLVMDISubrange DIBgetOrCreateSubrange(LLVMDIBuilder *Builder,
                                int64_t Lo,
                                int64_t Count) {%1
  return unwrap(Builder)-> getOrCreateSubrange(
                                /*int64_t*/ Lo,
                                /*int64_t*/ Count);
};


/// createGlobalVariable - Create a new descriptor for the specified global.
/// @param Name        Name of the variable.
/// @param File        File where this variable is defined.
/// @param LineNo      Line number.
/// @param Ty          Variable Type.
/// @param isLocalToUnit Boolean flag indicate whether this variable is
///                      externally visible or not.
/// @param Val         llvm::Value of the variable.
LLVMDIGlobalVariable DIBcreateGlobalVariableNoLinkageName(LLVMDIBuilder *Builder,
                 /* ^Renamed to eliminate overload. */
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDIType Ty,
                                bool isLocalToUnit,
                                Value *Val) {%1
  return unwrap(Builder)-> createGlobalVariable(
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDIType*/ Ty,
                                /*bool*/ isLocalToUnit,
                                unwrap(/*Value **/ Val));
};


/// \brief Create a new descriptor for the specified global.
/// @param Name        Name of the variable.
/// @param LinkageName Mangled variable name.
/// @param File        File where this variable is defined.
/// @param LineNo      Line number.
/// @param Ty          Variable Type.
/// @param isLocalToUnit Boolean flag indicate whether this variable is
///                      externally visible or not.
/// @param Val         llvm::Value of the variable.
LLVMDIGlobalVariable DIBcreateGlobalVariableWithLinkageName(LLVMDIBuilder *Builder,
                 /* ^Renamed to eliminate overload. */
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDIType Ty,
                                bool isLocalToUnit,
                                Value *Val) {%1
  return unwrap(Builder)-> createGlobalVariable(
                                /*LLVMStringRef*/ Name,
                                /*LLVMStringRef*/ LinkageName,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDIType*/ Ty,
                                /*bool*/ isLocalToUnit,
                                unwrap(/*Value **/ Val));
};


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
LLVMDIGlobalVariable DIBcreateStaticVariable(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Context,
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDIType Ty,
                                bool isLocalToUnit,
                                Value *Val,
                                MDNode *Decl = NULL) {%1
  return unwrap(Builder)-> createStaticVariable(
                                /*LLVMDIDescriptor*/ Context,
                                /*LLVMStringRef*/ Name,
                                /*LLVMStringRef*/ LinkageName,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDIType*/ Ty,
                                /*bool*/ isLocalToUnit,
                                unwrap(/*Value **/ Val),
                                unwrap(/*MDNode **/ Decl);
};


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
LLVMDIVariable DIBcreateLocalVariable(LLVMDIBuilder *Builder,
                                unsigned Tag,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDIType Ty,
                                bool AlwaysPreserve = false,
                                unsigned Flags = 0,
                                unsigned ArgNo = 0) {%1
  return unwrap(Builder)-> createLocalVariable(
                                /*unsigned*/ Tag,
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDIType*/ Ty,
                                /*bool*/ AlwaysPreserve = false,
                                /*unsigned*/ Flags = 0,
                                /*unsigned*/ ArgNo = 0);
};



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
LLVMDIVariable DIBcreateComplexVariable(LLVMDIBuilder *Builder,
                                unsigned Tag,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile F,
                                unsigned LineNo,
                                LLVMDIType Ty,
                                ArrayRefOfValueRef Addr,
                                unsigned ArgNo = 0) {%1
  return unwrap(Builder)-> createComplexVariable(
                                /*unsigned*/ Tag,
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ F,
                                /*unsigned*/ LineNo,
                                /*LLVMDIType*/ Ty,
                                /*ArrayRefOfValueRef*/ Addr,
                                /*unsigned*/ ArgNo);
};


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
LLVMDISubprogram DIBcreateFunctionFromDescriptor( LLVMDIBuilder *Builder,
             /* ^Renamed to eliminate overload. */
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDICompositeType Ty,
                                bool isLocalToUnit,
                                bool isDefinition,
                                unsigned ScopeLine,
                                unsigned Flags = 0,
                                bool isOptimized = false,
                                Function *Fn = 0,
                                MDNode *TParam = 0,
                                MDNode *Decl = 0) {%1
  return unwrap(Builder)-> createFunction(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMStringRef*/ LinkageName,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDICompositeType*/ Ty,
                                /*bool*/ isLocalToUnit,
                                /*bool*/ isDefinition,
                                /*unsigned*/ ScopeLine,
                                /*unsigned*/ Flags,
                                /*bool*/ isOptimized,
                                unwrap(/*Function **/ Fn,
                                unwrap(/*MDNode **/ TParam,
                                unwrap(/*MDNode **/ Decl);
};


/// FIXME: this is added for dragonegg. Once we update dragonegg
/// to call resolve function, this will be removed.
LLVMDISubprogram DIBcreateFunctionFromScope(LLVMDIBuilder *Builder,
             /* ^Renamed to eliminate overload. */
                            LLVMDIScopeRef Scope,
                            LLVMStringRef Name,
                            LLVMStringRef LinkageName,
                            LLVMDIFile File, 
                            unsigned LineNo,
                            LLVMDICompositeType Ty, 
                            bool isLocalToUnit,
                            bool isDefinition,
                            unsigned ScopeLine,
                            unsigned Flags = 0,
                            bool isOptimized = false,
                            Function *Fn = 0,
                            MDNode *TParam = 0,
                            MDNode *Decl = 0) {%1
  return unwrap(Builder)-> createFunction(
                            /*LLVMDIScopeRef*/ Scope,
                            /*LLVMStringRef*/ Name,
                            /*LLVMStringRef*/ LinkageName,
                            /*LLVMDIFile*/ File, 
                            /*unsigned*/ LineNo,
                            /*LLVMDICompositeType*/ Ty, 
                            /*bool*/ isLocalToUnit,
                            /*bool*/ isDefinition,
                            /*unsigned*/ ScopeLine,
                            /*unsigned*/ Flags,
                            /*bool*/ isOptimized,
                            unwrap(/*Function **/ Fn,
                            unwrap(/*MDNode **/ TParam,
                            unwrap(/*MDNode **/ Decl);
};


/// createMethod - Create a new descriptor for the specified C++ method.
/// See comments in LLVMDISubprogram for descriptions of these fields.
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
LLVMDISubprogram DIBcreateMethod(   LLVMDIBuilder *Builder, 
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMStringRef LinkageName,
                                LLVMDIFile File,
                                unsigned LineNo,
                                LLVMDICompositeType Ty,
                                bool isLocalToUnit,
                                bool isDefinition,
                                unsigned Virtuality = 0,
                                unsigned VTableIndex = 0,
                                LLVMDIType VTableHolder = LLVMDIType(),
                                unsigned Flags = 0,
                                bool isOptimized = false,
                                Function *Fn = 0,
                                MDNode *TParam = 0) {%1
  return unwrap(Builder)-> createMethod(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMStringRef*/ LinkageName,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo,
                                /*LLVMDICompositeType*/ Ty,
                                /*bool*/ isLocalToUnit,
                                /*bool*/ isDefinition,
                                /*unsigned*/ Virtuality,
                                /*unsigned*/ VTableIndex,
                                /*LLVMDIType*/ VTableHolder,
                                /*unsigned*/ Flags,
                                /*bool*/ isOptimized,
                                unwrap(/*Function **/ Fn,
                                unwrap(/*MDNode **/ TParam);
};


/// createNameSpace - This creates new descriptor for a namespace
/// with the specified parent scope.
/// @param Scope       Namespace scope
/// @param Name        Name of this namespace
/// @param File        Source file
/// @param LineNo      Line number
LLVMDINameSpace DIBcreateNameSpace( LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMStringRef Name,
                                LLVMDIFile File,
                                unsigned LineNo) {%1
  return unwrap(Builder)-> createNameSpace(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMStringRef*/ Name,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ LineNo);
};



/// createLexicalBlockFile - This creates a descriptor for a lexical
/// block with a new file attached. This merely extends the existing
/// lexical block as it crosses a file.
/// @param Scope       Lexical block.
/// @param File        Source file.
LLVMDILexicalBlockFile DIBcreateLexicalBlockFile(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMDIFile File) {%1
  return unwrap(Builder)-> createLexicalBlockFile(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMDIFile*/ File);
};


/// createLexicalBlock - This creates a descriptor for a lexical block
/// with the specified parent context.
/// @param Scope       Parent lexical scope.
/// @param File        Source file
/// @param Line        Line number
/// @param Col         Column number
LLVMDILexicalBlock DIBcreateLexicalBlock(LLVMDIBuilder *Builder,
                                LLVMDIDescriptor Scope,
                                LLVMDIFile File,
                                unsigned Line,
                                unsigned Col) {%1
  return unwrap(Builder)-> createLexicalBlock(
                                /*LLVMDIDescriptor*/ Scope,
                                /*LLVMDIFile*/ File,
                                /*unsigned*/ Line,
                                /*unsigned*/ Col);
};


/// \brief Create a descriptor for an imported module.
/// @param Context The scope this module is imported into
/// @param NS The namespace being imported here
/// @param Line Line number
LLVMDIImportedEntity DIBcreateImportedModuleFromNamespace(LLVMDIBuilder *Builder,
                 /* ^Renamed to eliminate overload. */
                                LLVMDIScope Context,
                                LLVMDINameSpace NS,
                                unsigned Line,
                                LLVMStringRef Name = StringRef()) {%1
  return unwrap(Builder)-> createImportedModule(
                                /*LLVMDIScope*/ Context,
                                /*LLVMDINameSpace*/ NS,
                                /*unsigned*/ Line,
                                /*LLVMStringRef*/ Name);
};


/// \brief Create a descriptor for an imported module.
/// @param Context The scope this module is imported into
/// @param NS An aliased namespace
/// @param Line Line number
LLVMDIImportedEntity DIBcreateImportedModuleFromImportedEntity(LLVMDIBuilder *Builder,
                 /* ^Renamed to eliminate overload. */
                                LLVMDIScope Context,
                                LLVMDIImportedEntity NS,
                                unsigned Line,
                                LLVMStringRef Name) {%1
  return unwrap(Builder)-> createImportedModule(
                                /*LLVMDIScope*/ Context,
                                /*LLVMDIImportedEntity*/ NS,
                                /*unsigned*/ Line,
                                /*LLVMStringRef*/ Name);
};


/// \brief Create a descriptor for an imported function.
/// @param Context The scope this module is imported into
/// @param Decl The declaration (or definition) of a function, type, or
///             variable
/// @param Line Line number
LLVMDIImportedEntity DIBcreateImportedDeclaration(LLVMDIBuilder *Builder,
                                LLVMDIScope Context,
                                LLVMDIDescriptor Decl,
                                unsigned Line) {%1
  return unwrap(Builder)-> createImportedDeclaration(
                                /*LLVMDIScope*/ Context,
                                /*LLVMDIDescriptor*/ Decl,
                                /*unsigned*/ Line);
};


/// insertDeclare - Insert a new llvm.dbg.declare intrinsic call.
/// @param Storage     llvm::Value of the variable
/// @param VarInfo     Variable's debug info descriptor.
/// @param InsertAtEnd Location for the new intrinsic.
Instruction *DIBinsertDeclareAtEnd(LLVMDIBuilder *Builder,
            /* ^Renamed to eliminate overload. */
                                Value *Storage,
                                LLVMDIVariable VarInfo,
                                BasicBlock *InsertAtEnd) {%1
    return wrap(unwrap(Builder)-> insertDeclare(
                                unwrap(/*Value **/ Storage),
                                /*LLVMDIVariable*/ VarInfo,
                                Unwrap(/*BasicBlock **/ InsertAtEnd)));
};


/// insertDeclare - Insert a new llvm.dbg.declare intrinsic call.
/// @param Storage      llvm::Value of the variable
/// @param VarInfo      Variable's debug info descriptor.
/// @param InsertBefore Location for the new intrinsic.
Instruction *DIBinsertDeclareBefore(LLVMDIBuilder *Builder,
            /* ^Renamed to eliminate overload. */
                                Value *Storage,
                                LLVMDIVariable VarInfo,
                                Instruction *InsertBefore) {%1
    return wrap(unwrap(Builder)-> insertDeclgare(
                                unwrap(/*Value **/ Storage),
                                /*LLVMDIVariable*/ VarInfo,
                                unwrap(/*Instruction **/ InsertBefore)));
};



/// insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
/// @param Val          llvm::Value of the variable
/// @param Offset       Offset
/// @param VarInfo      Variable's debug info descriptor.
/// @param InsertAtEnd Location for the new intrinsic.
Instruction *DIBinsertDbgValueIntrinsic(LLVMDIBuilder *Builder,
                                Value *Val,
                                uint64_t Offset,
                                LLVMDIVariable VarInfo,
                                BasicBlock *InsertAtEnd) {%1
    return wrap(unwrap(Builder)-> insertDbgValueIntrinsic(
                                unwrap(/*Value **/ Val),
                                /*uint64_t*/ Offset,
                                /*LLVMDIVariable*/ VarInfo,
                                Unwrap(/*BasicBlock **/ InsertAtEnd)));
};


/// insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
/// @param Val          llvm::Value of the variable
/// @param Offset       Offset
/// @param VarInfo      Variable's debug info descriptor.
/// @param InsertBefore Location for the new intrinsic.
Instruction *DIBinsertDbgValueIntrinsic(LLVMDIBuilder *Builder,
                                Value *Val,
                                uint64_t Offset,
                                LLVMDIVariable VarInfo,
                                Instruction *InsertBefore) {%1
    return wrap(unwrap(Builder)-> insertDbgValueIntrinsic(
                                unwrap(/*Value **/ Val),
                                /*uint64_t*/ Offset,
                                /*LLVMDIVariable*/ VarInfo,
                                unwrap(/*Instruction **/ InsertBefore)));
};


