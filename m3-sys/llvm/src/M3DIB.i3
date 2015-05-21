<*EXTERNAL*> INTERFACE M3DIB

(*===--- M3DIB.i3 --------- Debug Information Builder -----*- Modula3 -*-===//*)
(* Derived from:                                                              *)
(*===--- llvm/DIBuilder.h - Debug Information Builder ---------*- C++ -*-===//*)
(**)
(*                     The LLVM Compiler Infrastructure*)
(**)
(* This file is distributed under the University of Illinois Open Source*)
(* License. See LICENSE.TXT for details.*)
(**)
(*===--------------------------------------------------------------------===//*)
(**)
(* This file defines a DIBuilder that is useful for creating debugging*)
(* information entries in LLVM IR form.*)
(**)
(*===--------------------------------------------------------------------===//*)

(*
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/DataTypes.h"
#include "llvm/Support/ValueHandle.h"
*)

; IMPORT LLVMTypes 
; FROM LLVMTypes IMPORT int64_t , uint64_t , unsigned 
; FROM LLVMTypes IMPORT bool , false  
; FROM LLVMTypes IMPORT StringRef , StringRefNull 
(* All parameters of type StringRef are passed READONLY, which means the
   M3 compiler will pass them by reference.  This is necessary, because the
   parameter convention for passing a two-word RECORD by VALUE differs between
   the M3 backend (derived from gcc 4.7 or older and gcc 4.8.1 *) 
; FROM LLVMTypes IMPORT ArrayRefOfValueRef 
; FROM LLVMTypes IMPORT MDNodeRef , FunctionRef , InstructionRef   
; IMPORT LLVM 

(* The DIBuilder itself.  In llvm C++ code, this is a class, with the 
   various debug info node constructors as member function. 
*) 

; TYPE DIBuilderRef = UNTRACED BRANDED "M3DIBOpaqueDIBuilder" REF Opaque

; PROCEDURE DIBBuilderCreate ( Module : LLVM . ModuleRef ) : DIBuilderRef
  (* Return a new, initialized builder. *) 

(** finalize - Construct any deferred debug info descriptors.*)
; PROCEDURE DIBfinalize ( Builder : DIBuilderRef )

(* C++ pointer-to-MDNode types, all treated as opaque here. *)

; TYPE Opaque = RECORD END

; TYPE LLVMDIBasicTypeRef
    = UNTRACED BRANDED "M3DIBOpaqueDIBasicType" REF Opaque
; TYPE LLVMDICompileUnitRef
    = UNTRACED BRANDED "M3DIBOpaqueDICompileUnit" REF Opaque
; TYPE LLVMDICompositeTypeRef
    = UNTRACED BRANDED "M3DIBOpaqueDICompositeType" REF Opaque
; TYPE LLVMDIDerivedTypeRef
    = UNTRACED BRANDED "M3DIBOpaqueDIDerivedType" REF Opaque
; TYPE LLVMDIDescriptorRef
    = UNTRACED BRANDED "M3DIBOpaqueDIDescriptor" REF Opaque
; TYPE LLVMDIFileRef = UNTRACED BRANDED "M3DIBOpaqueDIFile" REF Opaque
; TYPE LLVMDIEnumeratorRef
    = UNTRACED BRANDED "M3DIBOpaqueDIEnumerator" REF Opaque
; TYPE LLVMDITypeRef 
    = UNTRACED BRANDED "M3DIBOpaqueDIType" REF Opaque
; TYPE DIArrayRef 
    = UNTRACED BRANDED "M3DIBOpaqueDIArray" REF Opaque
; TYPE LLVMDIGlobalVariableRef
    = UNTRACED BRANDED "M3DIBOpaqueDIGlobalVariable" REF Opaque
; TYPE LLVMDIImportedEntityRef
    = UNTRACED BRANDED "M3DIBOpaqueDIImportedEntity" REF Opaque
; TYPE LLVMDINameSpaceRef
    = UNTRACED BRANDED "M3DIBOpaqueDINameSpace" REF Opaque
; TYPE LLVMDIVariableRef 
    = UNTRACED BRANDED "M3DIBOpaqueDIVariable" REF Opaque
; TYPE LLVMDISubrangeRef 
    = UNTRACED BRANDED "M3DIBOpaqueDISubrange" REF Opaque
; TYPE LLVMDILexicalBlockFileRef
    = UNTRACED BRANDED "M3DIBOpaqueDILexicalBlockFile" REF Opaque
; TYPE LLVMDILexicalBlockRef
    = UNTRACED BRANDED "M3DIBOpaqueDILexicalBlock" REF Opaque
; TYPE LLVMDIScopeRef 
    = UNTRACED BRANDED "M3DIBOpaqueDIScope" REF Opaque
; TYPE LLVMDISubprogramRef
    = UNTRACED BRANDED "M3DIBOpaqueDISubprogram" REF Opaque
; TYPE LLVMDITemplateTypeParameterRef
    = UNTRACED BRANDED "M3DIBOpaqueDITemplateTypeParameter" REF Opaque
; TYPE LLVMDITemplateValueParameterRef
    = UNTRACED BRANDED "M3DIBOpaqueDITemplateValueParameter" REF Opaque
; TYPE LLVMDIObjCPropertyRef
    = UNTRACED BRANDED "M3DIBOpaqueDIObjCProperty" REF Opaque

(* NOTE: Each of these types named "DI*" (but not ending in "Ref"), in llvm 
         C++ code is a one-field class object whose one field is a pointer to 
         a subclass of MDNode.  They are passed into and returned from the C++ 
         functions by value, but they behave like pointers.  Their classes do 
         add a lot of methods, but no data members.  
*) 

; TYPE LLVMDIBasicType = RECORD MDNode : LLVMDIBasicTypeRef := NIL END 
; TYPE LLVMDICompileUnit = RECORD MDNode : LLVMDICompileUnitRef := NIL END 
; TYPE LLVMDICompositeType = RECORD MDNode : LLVMDICompositeTypeRef := NIL END 
; TYPE LLVMDIDerivedType = RECORD MDNode : LLVMDIDerivedTypeRef := NIL END 
; TYPE LLVMDIDescriptor = RECORD MDNode : LLVMDIDescriptorRef := NIL END 
; TYPE LLVMDIFile = RECORD MDNode : LLVMDIFileRef := NIL END 
; TYPE LLVMDIEnumerator = RECORD MDNode : LLVMDIEnumeratorRef := NIL END 
; TYPE LLVMDIType = RECORD MDNode : LLVMDITypeRef := NIL END 
; CONST LLVMDITypeEmpty = LLVMDIType { MDNode := NIL } 
; TYPE DIArray = RECORD MDNode : DIArrayRef := NIL END 
; TYPE LLVMDIGlobalVariable = RECORD MDNode : LLVMDIGlobalVariableRef := NIL END
; TYPE LLVMDIImportedEntity = RECORD MDNode : LLVMDIImportedEntityRef := NIL END
; TYPE LLVMDINameSpace = RECORD MDNode : LLVMDINameSpaceRef := NIL END 
; TYPE LLVMDIVariable = RECORD MDNode : LLVMDIVariableRef := NIL END 
; TYPE LLVMDISubrange = RECORD MDNode : LLVMDISubrangeRef := NIL END 
; TYPE LLVMDILexicalBlockFile = RECORD MDNode : LLVMDILexicalBlockFileRef := NIL END 
; TYPE LLVMDILexicalBlock = RECORD MDNode : LLVMDILexicalBlockRef := NIL END 
; TYPE LLVMDIScope = RECORD MDNode : LLVMDIScopeRef := NIL END 
; TYPE LLVMDISubprogram = RECORD MDNode : LLVMDISubprogramRef := NIL END 
; TYPE LLVMDITemplateTypeParameter 
    = RECORD MDNode : LLVMDITemplateTypeParameterRef := NIL END 
; TYPE LLVMDITemplateValueParameter 
    = RECORD MDNode : LLVMDITemplateValueParameterRef := NIL END 
; TYPE LLVMDIObjCProperty = RECORD MDNode : LLVMDIObjCPropertyRef := NIL END 

; TYPE ComplexAddrDom = { OpInvalid , OpPlus , OpDeref }
; TYPE ComplexAddrKind = [ ComplexAddrDom . OpPlus .. ComplexAddrDom . OpDeref ]

(* The procedure bindings to member functions of a DIBuilder class. *) 

(** createCompileUnit - A CompileUnit provides an anchor for all debugging*)
(** information generated during this instance of compilation.*)
(** @param Lang     Source programming language, eg. dwarf::DW_LANG_C99*)
(** @param File     File name*)
(** @param Dir      Directory*)
(** @param Producer String identify producer of debugging information.*)
(**                 Usually, this is a compiler version string.*)
(** @param isOptimized A boolean flag which indicates whether optimization*)
(**                    is ON or not.*)
(** @param Flags    This string lists command line options. This string is*)
(**                 directly embedded in debug info output which may be used*)
(**                 by a tool analyzing generated debugging information.*)
(** @param RV       This indicates runtime version for languages like*)
(**                 Objective-C.*)
(** @param SplitName The name of the file that we'll split debug info out*)
(**                  into.*)
; PROCEDURE DIBcreateCompileUnit
    ( Builder : DIBuilderRef
    ; Lang : unsigned
    ; READONLY File : StringRef
    ; READONLY Dir : StringRef
    ; READONLY Producer : StringRef
    ; isOptimized : bool
    ; READONLY Flags : StringRef
    ; RV : unsigned
    ; READONLY SplitName : StringRef
    )
  : LLVMDICompileUnit

(** createFile - Create a file descriptor to hold debugging information*)
(** for a file.*)
; PROCEDURE DIBcreateFile
    ( Builder : DIBuilderRef 
    ; READONLY Filename : StringRef 
    ; READONLY Directory : StringRef 
    )
  : LLVMDIFile

(** createEnumerator - Create a single enumerator value.*)
; PROCEDURE DIBcreateEnumerator
    ( Builder : DIBuilderRef ; READONLY Name : StringRef ; Val : int64_t )
  : LLVMDIEnumerator

(** \brief Create a DWARF unspecified type.*)
; PROCEDURE DIBcreateUnspecifiedType
    ( Builder : DIBuilderRef ; READONLY Name : StringRef )
  : LLVMDIBasicType

(** \brief Create C++11 nullptr type.*)
; PROCEDURE DIBcreateNullPtrType ( ) : LLVMDIBasicType

(** createBasicType - Create debugging information entry for a basic*)
(** type.*)
(** @param Name        Type name.*)
(** @param SizeInBits  Size of the type.*)
(** @param AlignInBits Type alignment.*)
(** @param Encoding    DWARF encoding code, e.g. dwarf::DW_ATE_float.*)
; PROCEDURE DIBcreateBasicType
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Encoding : unsigned
    )
  : LLVMDIBasicType

(** createQualifiedType - Create debugging information entry for a qualified*)
(** type, e.g. 'const int'.*)
(** @param Tag         Tag identifing type, e.g. dwarf::TAG_volatile_type*)
(** @param FromTy      Base Type.*)
; PROCEDURE DIBcreateQualifiedType
    ( Builder : DIBuilderRef ; Tag : unsigned ; FromTy : LLVMDIType )
  : LLVMDIDerivedType

(** createPointerType - Create debugging information entry for a pointer.*)
(** @param PointeeTy   Type pointed by this pointer.*)
(** @param SizeInBits  Size.*)
(** @param AlignInBits Alignment. (optional)*)
(** @param Name        Pointer type name. (optional)*)
; PROCEDURE DIBcreatePointerType
    ( Builder : DIBuilderRef
    ; PointeeTy : LLVMDIType
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t := 0
    ; READONLY Name : StringRef := StringRefNull
    )
  : LLVMDIDerivedType

(** \brief Create debugging information entry for a pointer to member.*)
(** @param PointeeTy Type pointed to by this pointer.*)
(** @param Class Type for which this pointer points to members of.*)
; PROCEDURE DIBcreateMemberPointerType
    ( Builder : DIBuilderRef ; PointeeTy : LLVMDIType ; Class : LLVMDIType )
  : LLVMDIDerivedType

(** createReferenceType - Create debugging information entry for a c++*)
(** style reference or rvalue reference type.*)
; PROCEDURE DIBcreateReferenceType
    ( Builder : DIBuilderRef ; Tag : unsigned ; RTy : LLVMDIType )
  : LLVMDIDerivedType

(** createTypedef - Create debugging information entry for a typedef.*)
(** @param Ty          Original type.*)
(** @param Name        Typedef name.*)
(** @param File        File where this type is defined.*)
(** @param LineNo      Line number.*)
(** @param Context     The surrounding context for the typedef.*)
; PROCEDURE DIBcreateTypedef
    ( Builder : DIBuilderRef
    ; Ty : LLVMDIType
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Context : LLVMDIDescriptor
    )
  : LLVMDIDerivedType

(** createFriend - Create debugging information entry for a 'friend'.*)
; PROCEDURE DIBcreateFriend
    ( Builder : DIBuilderRef ; Ty : LLVMDIType ; FriendTy : LLVMDIType )
  : LLVMDIDerivedType

(** createInheritance - Create debugging information entry to establish*)
(** inheritance relationship between two types.*)
(** @param Ty           Original type.*)
(** @param BaseTy       Base type. Ty is inherits from base.*)
(** @param BaseOffset   Base offset.*)
(** @param Flags        Flags to describe inheritance attribute,*)
(**                     e.g. private*)
; PROCEDURE DIBcreateInheritance
    ( Builder : DIBuilderRef
    ; Ty : LLVMDIType
    ; BaseTy : LLVMDIType
    ; BaseOffset : uint64_t
    ; Flags : unsigned
    )
  : LLVMDIDerivedType

(** createMemberType - Create debugging information entry for a member.*)
(** @param Scope        Member scope.*)
(** @param Name         Member name.*)
(** @param File         File where this member is defined.*)
(** @param LineNo       Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param OffsetInBits Member offset.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Ty           Parent type.*)
; PROCEDURE DIBcreateMemberType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; OffsetInBits : uint64_t
    ; Flags : unsigned
    ; Ty : LLVMDIType
    )
  : LLVMDIDerivedType

(** createStaticMemberType - Create debugging information entry for a*)
(** C++ static data member.*)
(** @param Scope      Member scope.*)
(** @param Name       Member name.*)
(** @param File       File where this member is declared.*)
(** @param LineNo     Line number.*)
(** @param Ty         Type of the static member.*)
(** @param Flags      Flags to encode member attribute, e.g. private.*)
(** @param Val        Const initializer of the member.*)
; PROCEDURE DIBcreateStaticMemberType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDIType
    ; Flags : unsigned
    ; Val : LLVM . ValueRef
    )
  : LLVMDIDerivedType

(** createObjCIVar - Create debugging information entry for Objective-C*)
(** instance variable.*)
(** @param Name         Member name.*)
(** @param File         File where this member is defined.*)
(** @param LineNo       Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param OffsetInBits Member offset.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Ty           Parent type.*)
(** @param PropertyName Name of the Objective C property associated with*)
(**                     this ivar.*)
(** @param PropertyGetterName Name of the Objective C property getter*)
(**                           selector.*)
(** @param PropertySetterName Name of the Objective C property setter*)
(**                           selector.*)
(** @param PropertyAttributes Objective C property attributes.*)
; PROCEDURE DIBcreateObjCIVarFromPropertyNode (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; OffsetInBits : uint64_t
    ; Flags : unsigned
    ; Ty : LLVMDIType
    ; READONLY PropertyName : StringRef := StringRefNull
    ; READONLY PropertyGetterName : StringRef := StringRefNull
    ; READONLY PropertySetterName : StringRef := StringRefNull
    ; PropertyAttributes : unsigned := 0
    )
  : LLVMDIDerivedType

(** createObjCIVar - Create debugging information entry for Objective-C*)
(** instance variable.*)
(** @param Name         Member name.*)
(** @param File         File where this member is defined.*)
(** @param LineNo       Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param OffsetInBits Member offset.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Ty           Parent type.*)
(** @param PropertyNode Property associated with this ivar.*)
; PROCEDURE DIBcreateObjCIVarFromNames (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; OffsetInBits : uint64_t
    ; Flags : unsigned
    ; Ty : LLVMDIType
    ; PropertyNode : MDNodeRef
    )
  : LLVMDIDerivedType

(** createObjCProperty - Create debugging information entry for Objective-C*)
(** property.*)
(** @param Name         Property name.*)
(** @param File         File where this property is defined.*)
(** @param LineNumber   Line number.*)
(** @param GetterName   Name of the Objective C property getter selector.*)
(** @param SetterName   Name of the Objective C property setter selector.*)
(** @param PropertyAttributes Objective C property attributes.*)
(** @param Ty           Type.*)
; PROCEDURE DIBcreateObjCProperty
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; READONLY GetterName : StringRef
    ; READONLY SetterName : StringRef
    ; PropertyAttributes : unsigned
    ; Ty : LLVMDIType
    )
  : LLVMDIObjCProperty

(** createClassType - Create debugging information entry for a class.*)
(** @param Scope        Scope in which this class is defined.*)
(** @param Name         class name.*)
(** @param File         File where this member is defined.*)
(** @param LineNumber   Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param OffsetInBits Member offset.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Elements     class members.*)
(** @param VTableHolder Debug info of the base class that contains vtable*)
(**                     for this type. This is used in*)
(**                     DW_AT_containing_type. See DWARF documentation*)
(**                     for more info.*)
(** @param TemplateParms Template type parameters.*)
(** @param UniqueIdentifier A unique identifier for the class.*)
; PROCEDURE DIBcreateClassType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; OffsetInBits : uint64_t
    ; Flags : unsigned
    ; DerivedFrom : LLVMDIType
    ; Elements : DIArray
    ; VTableHolder : LLVMDIType := LLVMDITypeEmpty
    ; TemplateParms : MDNodeRef := NIL 
    ; READONLY UniqueIdentifier : StringRef := StringRefNull
    )
  : LLVMDICompositeType

(** createStructType - Create debugging information entry for a struct.*)
(** @param Scope        Scope in which this struct is defined.*)
(** @param Name         Struct name.*)
(** @param File         File where this member is defined.*)
(** @param LineNumber   Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Elements     Struct elements.*)
(** @param RunTimeLang  Optional parameter, Objective-C runtime version.*)
(** @param UniqueIdentifier A unique identifier for the struct.*)
; PROCEDURE DIBcreateStructType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Flags : unsigned
    ; DerivedFrom : LLVMDIType
    ; Elements : DIArray
    ; RunTimeLang : unsigned := 0
    ; VTableHolder : LLVMDIType := LLVMDITypeEmpty
    ; READONLY UniqueIdentifier : StringRef := StringRefNull
    )
  : LLVMDICompositeType

(** createUnionType - Create debugging information entry for an union.*)
(** @param Scope        Scope in which this union is defined.*)
(** @param Name         Union name.*)
(** @param File         File where this member is defined.*)
(** @param LineNumber   Line number.*)
(** @param SizeInBits   Member size.*)
(** @param AlignInBits  Member alignment.*)
(** @param Flags        Flags to encode member attribute, e.g. private*)
(** @param Elements     Union elements.*)
(** @param RunTimeLang  Optional parameter, Objective-C runtime version.*)
(** @param UniqueIdentifier A unique identifier for the union.*)
; PROCEDURE DIBcreateUnionType
    ( Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Flags : unsigned
    ; Elements : DIArray
    ; RunTimeLang : unsigned := 0
    ; READONLY UniqueIdentifier : StringRef := StringRefNull
    )
  : LLVMDICompositeType

(** createTemplateTypeParameter - Create debugging information for template*)
(** type parameter.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Type parameter name.*)
(** @param Ty           Parameter type.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateTypeParameter
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateTypeParameter

(** createTemplateValueParameter - Create debugging information for template*)
(** value parameter.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Value parameter name.*)
(** @param Ty           Parameter type.*)
(** @param Val          Constant parameter value.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateValueParameterNoTag
            (* ^Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; Val : LLVM . ValueRef
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateValueParameter

(** createTemplateValueParameter - Create debugging information for template*)
(** value parameter.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Value parameter name.*)
(** @param Ty           Parameter type.*)
(** @param Val          Constant parameter value.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateValueParameterWTag
            (* ^Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Tag : unsigned 
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; Val : LLVM . ValueRef
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateValueParameter

(** \brief Create debugging information for a template template parameter.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Value parameter name.*)
(** @param Ty           Parameter type.*)
(** @param Val          The fully qualified name of the template.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateTemplateParameter
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; READONLY Val : StringRef
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateValueParameter

(** \brief Create debugging information for a template parameter pack.*)
(** @param Scope        Scope in which this type is defined.*)
(** @param Name         Value parameter name.*)
(** @param Ty           Parameter type.*)
(** @param Val          An array of types in the pack.*)
(** @param File         File where this type parameter is defined.*)
(** @param LineNo       Line number.*)
(** @param ColumnNo     Column Number.*)
; PROCEDURE DIBcreateTemplateParameterPack
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; Ty : LLVMDIType
    ; Val : DIArray
    ; File : MDNodeRef := NIL
    ; LineNo : unsigned := 0
    ; ColumnNo : unsigned := 0
    )
  : LLVMDITemplateValueParameter

(** createArrayType - Create debugging information entry for an array.*)
(** @param Size         Array size.*)
(** @param AlignInBits  Alignment.*)
(** @param Ty           Element type.*)
(** @param Subscripts   Subscripts.*)
; PROCEDURE DIBcreateArrayType
    ( Builder : DIBuilderRef
    ; Size : uint64_t
    ; AlignInBits : uint64_t
    ; Ty : LLVMDIType
    ; Subscripts : DIArray
    )
  : LLVMDICompositeType

(** createVectorType - Create debugging information entry for a vector type.*)
(** @param Size         Array size.*)
(** @param AlignInBits  Alignment.*)
(** @param Ty           Element type.*)
(** @param Subscripts   Subscripts.*)
; PROCEDURE DIBcreateVectorType
    ( Builder : DIBuilderRef
    ; Size : uint64_t
    ; AlignInBits : uint64_t
    ; Ty : LLVMDIType
    ; Subscripts : DIArray
    )
  : LLVMDICompositeType

(** createEnumerationType - Create debugging information entry for an*)
(** enumeration.*)
(** @param Scope          Scope in which this enumeration is defined.*)
(** @param Name           Union name.*)
(** @param File           File where this member is defined.*)
(** @param LineNumber     Line number.*)
(** @param SizeInBits     Member size.*)
(** @param AlignInBits    Member alignment.*)
(** @param Elements       Enumeration elements.*)
(** @param UnderlyingType Underlying type of a C++11/ObjC fixed enum.*)
(** @param UniqueIdentifier A unique identifier for the enum.*)
; PROCEDURE DIBcreateEnumerationType
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNumber : unsigned
    ; SizeInBits : uint64_t
    ; AlignInBits : uint64_t
    ; Elements : DIArray
    ; UnderlyingType : LLVMDIType
    ; READONLY UniqueIdentifier : StringRef := StringRefNull
    )
  : LLVMDICompositeType

(** createSubroutineType - Create subroutine type.*)
(** @param File           File in which this subroutine is defined.*)
(** @param ParameterTypes An array of subroutine parameter types. This*)
(**                       includes return type at 0th index.*)
; PROCEDURE DIBcreateSubroutineType
    ( Builder : DIBuilderRef ; File : LLVMDIFile ; ParameterTypes : DIArray )
  : LLVMDICompositeType

(** createArtificialType - Create a new procedure with "artificial" flag set.*)
; PROCEDURE DIBcreateArtificialType ( Builder : DIBuilderRef ; Ty : LLVMDIType )
  : LLVMDIType

(** createObjectPointerType - Create a new procedure with the "object pointer"
    flag set.*)
; PROCEDURE DIBcreateObjectPointerType 
    ( Builder : DIBuilderRef ; Ty : LLVMDIType ) 
  : LLVMDIType

(** createForwardDecl - Create a temporary forward-declared type.*)
; PROCEDURE DIBcreateForwardDecl
    ( Builder : DIBuilderRef
    ; Tag : unsigned
    ; READONLY Name : StringRef
    ; Scope : LLVMDIDescriptor
    ; F : LLVMDIFile
    ; Line : unsigned
    ; RuntimeLang : unsigned := 0
    ; SizeInBits : uint64_t := 0
    ; AlignInBits : uint64_t := 0
    ; READONLY UniqueIdentifier : StringRef := StringRefNull
    )
  : LLVMDICompositeType

(** retainType - Retain DIType in a module even if it is not referenced*)
(** through debug info anchors.*)
; PROCEDURE DIBretainType ( Builder : DIBuilderRef ; T : LLVMDIType )

(** createUnspecifiedParameter - Create unspecified type descriptor*)
(** for a subroutine type.*)
; PROCEDURE DIBcreateUnspecifiedParameter ( ) : LLVMDIDescriptor

(** getOrCreateArray - Get a DIArray, create one if required.*)
; PROCEDURE DIBgetOrCreateArray
    ( Builder : DIBuilderRef ; Elements : ArrayRefOfValueRef )
  : DIArray

(** getOrCreateSubrange - Create a descriptor for a value range.  This*)
(** implicitly uniques the values returned.*)
; PROCEDURE DIBgetOrCreateSubrange
    ( Builder : DIBuilderRef ; Lo : int64_t ; Count : int64_t )
  : LLVMDISubrange

(** createGlobalVariable - Create a new descriptor for the specified global.*)
(** @param Name        Name of the variable.*)
(** @param File        File where this variable is defined.*)
(** @param LineNo      Line number.*)
(** @param Ty          Variable Type.*)
(** @param isLocalToUnit Boolean flag indicate whether this variable is*)
(**                      externally visible or not.*)
(** @param Val         LLVM . ValueRef of the variable.*)
; PROCEDURE DIBcreateGlobalVariableNoLinkageName 
            (* ^Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDIType
    ; isLocalToUnit : bool
    ; Val : LLVM . ValueRef
    )
  : LLVMDIGlobalVariable

(** \brief Create a new descriptor for the specified global.*)
(** @param Name        Name of the variable.*)
(** @param LinkageName Mangled variable name.*)
(** @param File        File where this variable is defined.*)
(** @param LineNo      Line number.*)
(** @param Ty          Variable Type.*)
(** @param isLocalToUnit Boolean flag indicate whether this variable is*)
(**                      externally visible or not.*)
(** @param Val         LLVM . ValueRef of the variable.*)
; PROCEDURE DIBcreateGlobalVariableWithLinkageName 
            (* ^Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDIType
    ; isLocalToUnit : bool
    ; Val : LLVM . ValueRef
    )
  : LLVMDIGlobalVariable

(** createStaticVariable - Create a new descriptor for the specified*)
(** variable.*)
(** @param Context     Variable scope.*)
(** @param Name        Name of the variable.*)
(** @param LinkageName Mangled  name of the variable.*)
(** @param File        File where this variable is defined.*)
(** @param LineNo      Line number.*)
(** @param Ty          Variable Type.*)
(** @param isLocalToUnit Boolean flag indicate whether this variable is*)
(**                      externally visible or not.*)
(** @param Val         LLVM . ValueRef of the variable.*)
(** @param Decl        Reference to the corresponding declaration.*)
; PROCEDURE DIBcreateStaticVariable
    ( Builder : DIBuilderRef
    ; Context : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDIType
    ; isLocalToUnit : bool
    ; Val : LLVM . ValueRef
    ; Decl : MDNodeRef := NIL
    )
  : LLVMDIGlobalVariable


(** createLocalVariable - Create a new descriptor for the specified*)
(** local variable.*)
(** @param Tag         Dwarf TAG. Usually DW_TAG_auto_variable or*)
(**                    DW_TAG_arg_variable.*)
(** @param Scope       Variable scope.*)
(** @param Name        Variable name.*)
(** @param File        File where this variable is defined.*)
(** @param LineNo      Line number.*)
(** @param Ty          Variable Type*)
(** @param AlwaysPreserve Boolean. Set to true if debug info for this*)
(**                       variable should be preserved in optimized build.*)
(** @param Flags          Flags, e.g. artificial variable.*)
(** @param ArgNo       If this variable is an argument then this argument's*)
(**                    number. 1 indicates 1st argument.*)
; PROCEDURE DIBcreateLocalVariable
    ( Builder : DIBuilderRef
    ; Tag : unsigned
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDIType
    ; AlwaysPreserve : bool := false
    ; Flags : unsigned := 0
    ; ArgNo : unsigned := 0
    )
  : LLVMDIVariable


(** createComplexVariable - Create a new descriptor for the specified*)
(** variable which has a complex address expression for its address.*)
(** @param Tag         Dwarf TAG. Usually DW_TAG_auto_variable or*)
(**                    DW_TAG_arg_variable.*)
(** @param Scope       Variable scope.*)
(** @param Name        Variable name.*)
(** @param F           File where this variable is defined.*)
(** @param LineNo      Line number.*)
(** @param Ty          Variable Type*)
(** @param Addr        An array of complex address operations.*)
(** @param ArgNo       If this variable is an argument then this argument's*)
(**                    number. 1 indicates 1st argument.*)
; PROCEDURE DIBcreateComplexVariable
    ( Builder : DIBuilderRef
    ; Tag : unsigned
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; F : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDIType
    ; Addr : ArrayRefOfValueRef
    ; ArgNo : unsigned := 0
    )
  : LLVMDIVariable

(** createFunction - Create a new descriptor for the specified subprogram.*)
(** See comments in DISubprogram for descriptions of these fields.*)
(** @param Scope         Function scope.*)
(** @param Name          Function name.*)
(** @param LinkageName   Mangled function name.*)
(** @param File          File where this variable is defined.*)
(** @param LineNo        Line number.*)
(** @param Ty            Function type.*)
(** @param isLocalToUnit True if this function is not externally visible..*)
(** @param isDefinition  True if this is a function definition.*)
(** @param ScopeLine     Set to the beginning of the scope this starts*)
(** @param Flags         e.g. is this function prototyped or not.*)
(**                      This flags are used to emit dwarf attributes.*)
(** @param isOptimized   True if optimization is ON.*)
(** @param Fn            llvm::Function pointer.*)
(** @param TParam        Function template parameters.*)
; PROCEDURE DIBcreateFunctionFromDescriptor (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDICompositeType
    ; isLocalToUnit : bool
    ; isDefinition : bool
    ; ScopeLine : unsigned
    ; Flags : unsigned := 0
    ; isOptimized : bool := false
    ; Fn : FunctionRef := NIL
    ; TParam : MDNodeRef := NIL
    ; Decl : MDNodeRef := NIL
    )
  : LLVMDISubprogram

(** FIXME: this is added for dragonegg. Once we update dragonegg*)
(** to call resolve function, this will be removed.*)
; PROCEDURE DIBcreateFunctionFromScope (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIScopeRef
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDICompositeType
    ; isLocalToUnit : bool
    ; isDefinition : bool
    ; ScopeLine : unsigned
    ; Flags : unsigned := 0
    ; isOptimized : bool := false
    ; Fn : FunctionRef := NIL
    ; TParam : MDNodeRef := NIL
    ; Decl : MDNodeRef := NIL
    )
  : LLVMDISubprogram

(** createMethod - Create a new descriptor for the specified C++ method.*)
(** See comments in DISubprogram for descriptions of these fields.*)
(** @param Scope         Function scope.*)
(** @param Name          Function name.*)
(** @param LinkageName   Mangled function name.*)
(** @param File          File where this variable is defined.*)
(** @param LineNo        Line number.*)
(** @param Ty            Function type.*)
(** @param isLocalToUnit True if this function is not externally visible..*)
(** @param isDefinition  True if this is a function definition.*)
(** @param Virtuality    Attributes describing virtualness. e.g. pure*)
(**                      virtual function.*)
(** @param VTableIndex   Index no of this method in virtual table.*)
(** @param VTableHolder  Type that holds vtable.*)
(** @param Flags         e.g. is this function prototyped or not.*)
(**                      This flags are used to emit dwarf attributes.*)
(** @param isOptimized   True if optimization is ON.*)
(** @param Fn            llvm::Function pointer.*)
(** @param TParam        Function template parameters.*)
; PROCEDURE DIBcreateMethod
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; READONLY LinkageName : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    ; Ty : LLVMDICompositeType
    ; isLocalToUnit : bool
    ; isDefinition : bool
    ; Virtuality : unsigned := 0
    ; VTableIndex : unsigned := 0
    ; VTableHolder : LLVMDIType := LLVMDITypeEmpty
    ; Flags : unsigned := 0
    ; isOptimized : bool := false
    ; Fn : FunctionRef := NIL
    ; TParam : MDNodeRef := NIL
    )
  : LLVMDISubprogram

(** createNameSpace - This creates new descriptor for a namespace*)
(** with the specified parent scope.*)
(** @param Scope       Namespace scope*)
(** @param Name        Name of this namespace*)
(** @param File        Source file*)
(** @param LineNo      Line number*)
; PROCEDURE DIBcreateNameSpace
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; READONLY Name : StringRef
    ; File : LLVMDIFile
    ; LineNo : unsigned
    )
  : LLVMDINameSpace


(** createLexicalBlockFile - This creates a descriptor for a lexical*)
(** block with a new file attached. This merely extends the existing*)
(** lexical block as it crosses a file.*)
(** @param Scope       Lexical block.*)
(** @param File        Source file.*)
; PROCEDURE DIBcreateLexicalBlockFile
    ( Builder : DIBuilderRef ; Scope : LLVMDIDescriptor ; File : LLVMDIFile )
  : LLVMDILexicalBlockFile

(** createLexicalBlock - This creates a descriptor for a lexical block*)
(** with the specified parent context.*)
(** @param Scope       Parent lexical scope.*)
(** @param File        Source file*)
(** @param Line        Line number*)
(** @param Col         Column number*)
; PROCEDURE DIBcreateLexicalBlock
    ( Builder : DIBuilderRef
    ; Scope : LLVMDIDescriptor
    ; File : LLVMDIFile
    ; Line : unsigned
    ; Col : unsigned
    )
  : LLVMDILexicalBlock

(** \brief Create a descriptor for an imported module.*)
(** @param Context The scope this module is imported into*)
(** @param NS The namespace being imported here*)
(** @param Line Line number*)
; PROCEDURE DIBcreateImportedModuleFromNamespace 
            (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Context : LLVMDIScope
    ; NS : LLVMDINameSpace
    ; Line : unsigned
    ; READONLY Name : StringRef := StringRefNull
    )
  : LLVMDIImportedEntity

(** \brief Create a descriptor for an imported module.*)
(** @param Context The scope this module is imported into*)
(** @param NS An aliased namespace*)
(** @param Line Line number*)
; PROCEDURE DIBcreateImportedModuleFromImportedEntity 
            (* ^Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Context : LLVMDIScope
    ; NS : LLVMDIImportedEntity
    ; Line : unsigned
    ; READONLY Name : StringRef
    )
  : LLVMDIImportedEntity

(** \brief Create a descriptor for an imported function.*)
(** @param Context The scope this module is imported into*)
(** @param Decl The declaration (or definition) of a function, type, or*)
(**             variable*)
(** @param Line Line number*)
; PROCEDURE DIBcreateImportedDeclaration
    ( Builder : DIBuilderRef
    ; Context : LLVMDIScope
    ; Decl : LLVMDIDescriptor
    ; Line : unsigned
    )
  : LLVMDIImportedEntity

(** insertDeclare - Insert a new llvm.dbg.declare intrinsic call.*)
(** @param Storage     LLVM . ValueRef of the variable*)
(** @param VarInfo     Variable's debug info descriptor.*)
(** @param InsertAtEnd Location for the new intrinsic.*)
; PROCEDURE DIBinsertDeclareAtEnd (* Renamed to resolve overload. *) 
    ( Builder : DIBuilderRef
    ; Storage : LLVM . ValueRef 
    ; VarInfo : LLVMDIVariable
    ; InsertAtEnd : LLVM . BasicBlockRef
    )
  : InstructionRef

(** insertDeclare - Insert a new llvm.dbg.declare intrinsic call.*)
(** @param Storage      LLVM . ValueRef of the variable*)
(** @param VarInfo      Variable's debug info descriptor.*)
(** @param InsertBefore Location for the new intrinsic.*)
; PROCEDURE DIBinsertDeclareBefore (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Storage : LLVM . ValueRef 
    ; VarInfo : LLVMDIVariable
    ; InsertBefore : InstructionRef
    )
  : InstructionRef

(** insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.*)
(** @param Val          LLVM . ValueRef of the variable*)
(** @param Offset       Offset*)
(** @param VarInfo      Variable's debug info descriptor.*)
(** @param InsertAtEnd Location for the new intrinsic.*)
; PROCEDURE DIBinsertDbgValueIntrinsicAtEnd (* Renamed to resolve overload. *) 
    ( Builder : DIBuilderRef
    ; Val : LLVM . ValueRef
    ; Offset : uint64_t
    ; VarInfo : LLVMDIVariable
    ; InsertAtEnd : LLVM . BasicBlockRef
    )
  : InstructionRef

(** insertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.*)
(** @param Val          LLVM . ValueRef of the variable*)
(** @param Offset       Offset*)
(** @param VarInfo      Variable's debug info descriptor.*)
(** @param InsertBefore Location for the new intrinsic.*)
; PROCEDURE DIBinsertDbgValueIntrinsicBefore (* Renamed to resolve overload. *)
    ( Builder : DIBuilderRef
    ; Val : LLVM . ValueRef 
    ; Offset : uint64_t
    ; VarInfo : LLVMDIVariable
    ; InsertBefore : InstructionRef
    )
  : InstructionRef

; END M3DIB
.


