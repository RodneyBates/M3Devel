/* Modula-3: modified */

/* Target definitions for Darwin (Mac OS X) systems.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef CONFIG_DARWIN_H
#define CONFIG_DARWIN_H

/* The definitions in this file are common to all processor types
   running Darwin, which is the kernel for Mac OS X.  Darwin is
   basically a BSD user layer laid over a Mach kernel, then evolved
   for many years (at NeXT) in parallel with other Unix systems.  So
   while the runtime is a somewhat idiosyncratic Mach-based thing,
   other definitions look like they would for a BSD variant.  */

/* Although NeXT ran on many different architectures, as of Jan 2001
   the only supported Darwin targets are PowerPC and x86.  */

/* One of Darwin's NeXT legacies is the Mach-O format, which is partly
   like a.out and partly like COFF, with additional features like
   multi-architecture binary support.  */

#define DARWIN_X86 0
#define DARWIN_PPC 0

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

/* Suppress g++ attempt to link in the math library automatically. */
#define MATH_LIBRARY ""

/* We have atexit.  */

#define HAVE_ATEXIT

/* Define an empty body for the function do_global_dtors() in libgcc2.c.  */

#define DO_GLOBAL_DTORS_BODY

/* Default to using the NeXT-style runtime, since that's what is
   pre-installed on Darwin systems.  */

#define NEXT_OBJC_RUNTIME 1

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */

#undef	DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* True if pragma ms_struct is in effect.  */
extern GTY(()) int darwin_ms_struct;

#define SUBSUBTARGET_OVERRIDE_OPTIONS					\
  do {									\
    darwin_override_options ();						\
  } while (0)

#define SUBTARGET_C_COMMON_OVERRIDE_OPTIONS do {                        \
    if (!global_options_set.x_flag_objc_sjlj_exceptions)		\
      global_options.x_flag_objc_sjlj_exceptions = 			\
				flag_next_runtime && !TARGET_64BIT;	\
    if (flag_mkernel || flag_apple_kext)				\
      {									\
	if (flag_use_cxa_atexit == 2)					\
	  flag_use_cxa_atexit = 0;					\
	/* kexts should always be built without the coalesced sections	\
	   because the kernel loader doesn't grok such sections.  */	\
	flag_weak = 0;							\
	/* No RTTI in kexts.  */					\
	flag_rtti = 0;							\
      }									\
  } while (0)


/* We still allow output of STABS.  */

#define DBX_DEBUGGING_INFO 1

#define DWARF2_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define DEBUG_FRAME_SECTION	"__DWARF,__debug_frame,regular,debug"
#define DEBUG_INFO_SECTION	"__DWARF,__debug_info,regular,debug"
#define DEBUG_ABBREV_SECTION	"__DWARF,__debug_abbrev,regular,debug"
#define DEBUG_ARANGES_SECTION	"__DWARF,__debug_aranges,regular,debug"
#define DEBUG_MACINFO_SECTION	"__DWARF,__debug_macinfo,regular,debug"
#define DEBUG_LINE_SECTION	"__DWARF,__debug_line,regular,debug"
#define DEBUG_LOC_SECTION	"__DWARF,__debug_loc,regular,debug"
#define DEBUG_PUBNAMES_SECTION	"__DWARF,__debug_pubnames,regular,debug"
#define DEBUG_PUBTYPES_SECTION	"__DWARF,__debug_pubtypes,regular,debug"
#define DEBUG_STR_SECTION	"__DWARF,__debug_str,regular,debug"
#define DEBUG_RANGES_SECTION	"__DWARF,__debug_ranges,regular,debug"
#define DEBUG_MACRO_SECTION    "__DWARF,__debug_macro,regular,debug"

#define TARGET_WANT_DEBUG_PUB_SECTIONS true

/* When generating stabs debugging, use N_BINCL entries.  */

#define DBX_USE_BINCL

/* There is no limit to the length of stabs strings.  */

#define DBX_CONTIN_LENGTH 0

/* gdb needs a null N_SO at the end of each file for scattered loading.  */

#define DBX_OUTPUT_NULL_N_SO_AT_MAIN_SOURCE_FILE_END

/* GCC's definition of 'one_only' is the same as its definition of 'weak'.  */
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

/* Mach-O supports 'weak imports', and 'weak definitions' in coalesced
   sections.  machopic_select_section ensures that weak variables go in
   coalesced sections.  Weak aliases (or any other kind of aliases) are
   not supported.  Weak symbols that aren't visible outside the .s file
   are not supported.  */
#define ASM_WEAKEN_DECL(FILE, DECL, NAME, ALIAS)			\
  do {									\
    if (ALIAS)								\
      {									\
	warning (0, "alias definitions not supported in Mach-O; ignored");	\
	break;								\
      }									\
 									\
    if (! DECL_EXTERNAL (DECL) && TREE_PUBLIC (DECL))			\
      targetm.asm_out.globalize_label (FILE, NAME);			\
    if (DECL_EXTERNAL (DECL))						\
      fputs ("\t.weak_reference ", FILE);				\
    else if (lookup_attribute ("weak_import", DECL_ATTRIBUTES (DECL)))	\
      break;								\
    else if (TREE_PUBLIC (DECL))					\
      fputs ("\t.weak_definition ", FILE);				\
    else								\
      break;								\
    assemble_name (FILE, NAME);						\
    fputc ('\n', FILE);							\
  } while (0)

/* Darwin has the pthread routines in libSystem, which every program
   links to, so there's no need for weak-ness for that.  */
#define GTHREAD_USE_WEAK 0

/* The Darwin linker imposes two limitations on common symbols: they
   can't have hidden visibility, and they can't appear in dylibs.  As
   a consequence, we should never use common symbols to represent
   vague linkage. */
#undef USE_COMMON_FOR_ONE_ONLY
#define USE_COMMON_FOR_ONE_ONLY 0

/* The Darwin linker doesn't want coalesced symbols to appear in
   a static archive's table of contents. */
#undef TARGET_WEAK_NOT_IN_ARCHIVE_TOC
#define TARGET_WEAK_NOT_IN_ARCHIVE_TOC 1

/* On Darwin, we don't (at the time of writing) have linkonce sections
   with names, so it's safe to make the class data not comdat.  */
#define TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT hook_bool_void_false

/* For efficiency, on Darwin the RTTI information that is always
   emitted in the standard C++ library should not be COMDAT.  */
#define TARGET_CXX_LIBRARY_RTTI_COMDAT hook_bool_void_false

/* We make exception information linkonce. */
#undef TARGET_USES_WEAK_UNWIND_INFO
#define TARGET_USES_WEAK_UNWIND_INFO 1

/* We need to use a nonlocal label for the start of an EH frame: the
   Darwin linker requires that a coalesced section start with a label.
   Unfortunately, it also requires that 'debug' sections don't contain
   labels.  */
#undef FRAME_BEGIN_LABEL
#define FRAME_BEGIN_LABEL (for_eh ? "EH_frame" : "Lframe")

/* Emit a label for the FDE corresponding to DECL.  EMPTY means
   emit a label for an empty FDE. */
#define TARGET_ASM_EMIT_UNWIND_LABEL darwin_emit_unwind_label

/* Emit a label to separate the exception table.  */
#define TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL darwin_emit_except_table_label

/* Our profiling scheme doesn't LP labels and counter words.  */

#define NO_PROFILE_COUNTERS	1

#undef	INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP

#undef	INVOKE__main

#define TARGET_ASM_CONSTRUCTOR  machopic_asm_out_constructor
#define TARGET_ASM_DESTRUCTOR   machopic_asm_out_destructor

/* Always prefix with an underscore.  */

#define USER_LABEL_PREFIX "_"

/* A dummy symbol that will be replaced with the function base name.  */
#define MACHOPIC_FUNCTION_BASE_NAME "<pic base>"

/* Don't output a .file directive.  That is only used by the assembler for
   error reporting.  */
#undef	TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE false

#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END darwin_file_end

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space "HOST_WIDE_INT_PRINT_UNSIGNED"\n", SIZE)

/* Give ObjC methods pretty symbol names.  */

#undef	OBJC_GEN_METHOD_LABEL
#define OBJC_GEN_METHOD_LABEL(BUF,IS_INST,CLASS_NAME,CAT_NAME,SEL_NAME,NUM) \
  do { if (CAT_NAME)							\
	 sprintf (BUF, "%c[%s(%s) %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (CAT_NAME), (SEL_NAME));		\
       else								\
	 sprintf (BUF, "%c[%s %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (SEL_NAME));				\
     } while (0)

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL) \
	darwin_asm_declare_object_name ((FILE), (NAME), (DECL))

/* The RTTI data (e.g., __ti4name) is common and public (and static),
   but it does need to be referenced via indirect PIC data pointers.
   The machopic_define_symbol calls are telling the machopic subsystem
   that the name *is* defined in this module, so it doesn't need to
   make them indirect.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    const char *xname = NAME;						\
    if (GET_CODE (XEXP (DECL_RTL (DECL), 0)) != SYMBOL_REF)		\
      xname = IDENTIFIER_POINTER (DECL_NAME (DECL));			\
    if (! DECL_WEAK (DECL)						\
        && ((TREE_STATIC (DECL)						\
	     && (!DECL_COMMON (DECL) || !TREE_PUBLIC (DECL)))		\
            || DECL_INITIAL (DECL)))					\
        machopic_define_symbol (DECL_RTL (DECL));			\
    if ((TREE_STATIC (DECL)						\
	 && (!DECL_COMMON (DECL) || !TREE_PUBLIC (DECL)))		\
        || DECL_INITIAL (DECL))						\
      (* targetm.encode_section_info) (DECL, DECL_RTL (DECL), false);	\
    ASM_OUTPUT_FUNCTION_LABEL (FILE, xname, DECL);			\
  } while (0)

#undef TARGET_ASM_DECLARE_CONSTANT_NAME
#define TARGET_ASM_DECLARE_CONSTANT_NAME darwin_asm_declare_constant_name

/* Wrap new method names in quotes so the assembler doesn't gag.
   Make Objective-C internal symbols local and in doing this, we need 
   to accommodate the name mangling done by c++ on file scope locals.  */

#undef	ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)					     \
  do {									     \
       const char *xname = (NAME);					     \
       if (! strcmp (xname, MACHOPIC_FUNCTION_BASE_NAME))		     \
         machopic_output_function_base_name(FILE);                           \
       else if (xname[0] == '&' || xname[0] == '*')			     \
         {								     \
           int len = strlen (xname);					     \
	   if (len > 6 && !strcmp ("$stub", xname + len - 5))		     \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   else if (len > 7 && !strcmp ("$stub\"", xname + len - 6))	     \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   else if (len > 14 && !strcmp ("$non_lazy_ptr", xname + len - 13)) \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   else if (len > 15 && !strcmp ("$non_lazy_ptr\"", xname + len - 14)) \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   if (xname[1] != '"' && name_needs_quotes (&xname[1]))	     \
	     fprintf (FILE, "\"%s\"", &xname[1]);			     \
	   else								     \
	     fputs (&xname[1], FILE); 					     \
	 }								     \
       else if (xname[0] == '+' || xname[0] == '-')			     \
         fprintf (FILE, "\"%s\"", xname);				     \
       else if (xname[0] != '"' && name_needs_quotes (xname))		     \
	 asm_fprintf (FILE, "\"%U%s\"", xname);				     \
       else								     \
         asm_fprintf (FILE, "%U%s", xname);				     \
  } while (0)

/* Output before executable code.  */
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "\t.data"

#undef	ALIGN_ASM_OP
#define ALIGN_ASM_OP		".align"

#undef	ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t%s\t%d\n", ALIGN_ASM_OP, (LOG))

/* The maximum alignment which the object file format can support in
   bits.  For Mach-O, this is 2^15 bytes.  */

#undef	MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (0x8000 * 8)

#define L2_MAX_OFILE_ALIGNMENT 15

/*  These are the three variants that emit referenced blank space.  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
	darwin_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

#undef	ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
	darwin_asm_output_aligned_decl_local				\
				  ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

#undef  ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN)	\
	darwin_asm_output_aligned_decl_common				\
				   ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* The generic version, archs should over-ride where required.  */
#define MACHOPIC_NL_SYMBOL_PTR_SECTION ".non_lazy_symbol_pointer"

/* Declare the section variables.  */
#ifndef USED_FOR_TARGET
enum darwin_section_enum {
#define DEF_SECTION(NAME, FLAGS, DIRECTIVE, OBJC) NAME,
#include "darwin-sections.def"
#undef DEF_SECTION
  NUM_DARWIN_SECTIONS
};
extern GTY(()) section * darwin_sections[NUM_DARWIN_SECTIONS];
#endif

#undef	TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION machopic_select_section

#undef	TARGET_ASM_FUNCTION_SECTION
#define TARGET_ASM_FUNCTION_SECTION darwin_function_section

#undef	TARGET_ASM_FUNCTION_SWITCHED_TEXT_SECTIONS
#define TARGET_ASM_FUNCTION_SWITCHED_TEXT_SECTIONS \
	darwin_function_switched_text_sections

#undef	TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION machopic_select_rtx_section
#undef  TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION darwin_unique_section
#undef  TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_no_function_rodata_section

#undef  TARGET_ASM_TM_CLONE_TABLE_SECTION
#define TARGET_ASM_TM_CLONE_TABLE_SECTION darwin_tm_clone_table_section

#undef  TARGET_ASM_RELOC_RW_MASK
#define TARGET_ASM_RELOC_RW_MASK machopic_reloc_rw_mask


#define ASM_DECLARE_UNRESOLVED_REFERENCE(FILE,NAME)			\
    do {								\
	 if (FILE) {							\
	   if (MACHOPIC_INDIRECT)					\
	     fprintf (FILE, "\t.lazy_reference ");			\
	   else								\
	     fprintf (FILE, "\t.reference ");				\
	   assemble_name (FILE, NAME);					\
	   fprintf (FILE, "\n");					\
	 }                                                              \
       } while (0)

#define ASM_DECLARE_CLASS_REFERENCE(FILE,NAME)				\
    do {								\
	 if (FILE) {							\
	   fprintf (FILE, "\t");					\
	   assemble_name (FILE, NAME);					\
	   fprintf (FILE, "=0\n");					\
	   (*targetm.asm_out.globalize_label) (FILE, NAME);		\
	 }								\
       } while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "
#define TARGET_ASM_GLOBALIZE_LABEL darwin_globalize_label

/* Emit an assembler directive to set visibility for a symbol.  Used
   to support visibility attribute and Darwin's private extern
   feature.  */
#undef TARGET_ASM_ASSEMBLE_VISIBILITY
#define TARGET_ASM_ASSEMBLE_VISIBILITY darwin_assemble_visibility

/* Extra attributes for Darwin.  */
#define SUBTARGET_ATTRIBUTE_TABLE					     \
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,     \
       affects_type_identity } */						     \
  { "apple_kext_compatibility", 0, 0, false, true, false,		     \
    darwin_handle_kext_attribute, false },				     \
  { "weak_import", 0, 0, true, false, false,				     \
    darwin_handle_weak_import_attribute, false }

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%ld", PREFIX, (long)(NUM))

#undef TARGET_ASM_MARK_DECL_PRESERVED
#define TARGET_ASM_MARK_DECL_PRESERVED darwin_mark_decl_preserved

/* Set on a symbol with SYMBOL_FLAG_FUNCTION or
   MACHO_SYMBOL_FLAG_VARIABLE to indicate that the function or
   variable has been defined in this translation unit.
   When porting Mach-O to new architectures you need to make
   sure these aren't clobbered by the backend.  */

#define MACHO_SYMBOL_FLAG_VARIABLE (SYMBOL_FLAG_MACH_DEP)
#define MACHO_SYMBOL_FLAG_DEFINED ((SYMBOL_FLAG_MACH_DEP) << 1)

/* Set on a symbol to indicate when fix-and-continue style code
   generation is being used and the symbol refers to a static symbol
   that should be rebound from new instances of a translation unit to
   the original instance of the data.  */

#define MACHO_SYMBOL_STATIC ((SYMBOL_FLAG_MACH_DEP) << 2)

/* Symbolic names for various things we might know about a symbol.  */

enum machopic_addr_class {
  MACHOPIC_UNDEFINED,
  MACHOPIC_DEFINED_DATA,
  MACHOPIC_UNDEFINED_DATA,
  MACHOPIC_DEFINED_FUNCTION,
  MACHOPIC_UNDEFINED_FUNCTION
};

/* Macros defining the various PIC cases.  */

#undef  MACHO_DYNAMIC_NO_PIC_P
#define MACHO_DYNAMIC_NO_PIC_P	(TARGET_MACHO_DYNAMIC_NO_PIC)
#undef  MACHOPIC_INDIRECT
#define MACHOPIC_INDIRECT	(flag_pic || MACHO_DYNAMIC_NO_PIC_P)
#define MACHOPIC_JUST_INDIRECT	(MACHO_DYNAMIC_NO_PIC_P)
#undef  MACHOPIC_PURE
#define MACHOPIC_PURE		(flag_pic && ! MACHO_DYNAMIC_NO_PIC_P)

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO  darwin_encode_section_info
#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING  default_strip_name_encoding

#define GEN_BINDER_NAME_FOR_STUB(BUF,STUB,STUB_LENGTH)		\
  do {								\
    const char *const stub_ = (STUB);				\
    char *buffer_ = (BUF);					\
    strcpy (buffer_, stub_);					\
    if (stub_[0] == '"')					\
      {								\
	strcpy (buffer_ + (STUB_LENGTH) - 1, "_binder\"");	\
      }								\
    else							\
      {								\
	strcpy (buffer_ + (STUB_LENGTH), "_binder");		\
      }								\
  } while (0)

#define GEN_SYMBOL_NAME_FOR_SYMBOL(BUF,SYMBOL,SYMBOL_LENGTH)	\
  do {								\
    const char *const symbol_ = (SYMBOL);			\
    char *buffer_ = (BUF);					\
    if (name_needs_quotes (symbol_) && symbol_[0] != '"')	\
      {								\
	  sprintf (buffer_, "\"%s\"", symbol_);			\
      }								\
    else							\
      {								\
	strcpy (buffer_, symbol_);				\
      }								\
  } while (0)

/* Given a symbol name string, create the lazy pointer version
   of the symbol name.  */

#define GEN_LAZY_PTR_NAME_FOR_SYMBOL(BUF,SYMBOL,SYMBOL_LENGTH)	\
  do {								\
    const char *symbol_ = (SYMBOL);                             \
    char *buffer_ = (BUF);					\
    if (symbol_[0] == '"')					\
      {								\
        strcpy (buffer_, "\"L");				\
        strcpy (buffer_ + 2, symbol_ + 1);			\
	strcpy (buffer_ + (SYMBOL_LENGTH), "$lazy_ptr\"");	\
      }								\
    else if (name_needs_quotes (symbol_))			\
      {								\
        strcpy (buffer_, "\"L");				\
        strcpy (buffer_ + 2, symbol_);				\
	strcpy (buffer_ + (SYMBOL_LENGTH) + 2, "$lazy_ptr\"");	\
      }								\
    else							\
      {								\
        strcpy (buffer_, "L");					\
        strcpy (buffer_ + 1, symbol_);				\
	strcpy (buffer_ + (SYMBOL_LENGTH) + 1, "$lazy_ptr");	\
      }								\
  } while (0)

#define EH_FRAME_SECTION_NAME   "__TEXT"
#define EH_FRAME_SECTION_ATTR ",coalesced,no_toc+strip_static_syms+live_support"

/* Java runtime class list.  */
#define JCR_SECTION_NAME "__DATA,jcr,regular,no_dead_strip"

#undef ASM_PREFERRED_EH_DATA_FORMAT
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)  \
  (((CODE) == 2 && (GLOBAL) == 1) \
   ? (DW_EH_PE_pcrel | DW_EH_PE_indirect | DW_EH_PE_sdata4) : \
     ((CODE) == 1 || (GLOBAL) == 0) ? DW_EH_PE_pcrel : DW_EH_PE_absptr)

#define ASM_OUTPUT_DWARF_DELTA(FILE,SIZE,LABEL1,LABEL2)  \
  darwin_asm_output_dwarf_delta (FILE, SIZE, LABEL1, LABEL2)

#define ASM_OUTPUT_DWARF_OFFSET(FILE,SIZE,LABEL,BASE)  \
  darwin_asm_output_dwarf_offset (FILE, SIZE, LABEL, BASE)

#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(ASM_OUT_FILE, ENCODING, SIZE, ADDR, DONE)	\
      if (ENCODING == ASM_PREFERRED_EH_DATA_FORMAT (2, 1)) {				\
	darwin_non_lazy_pcrel (ASM_OUT_FILE, ADDR);					\
	goto DONE;									\
      }

/* Experimentally, putting jump tables in text is faster on SPEC.
   Also this is needed for correctness for coalesced functions.  */

#ifndef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 1
#endif

#define TARGET_TERMINATE_DW2_EH_FRAME_INFO false

#define TARGET_ASM_INIT_SECTIONS darwin_init_sections
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION darwin_asm_named_section

#define DARWIN_REGISTER_TARGET_PRAGMAS()			\
  do {								\
    if (!flag_preprocess_only)					\
      cpp_register_pragma (parse_in, NULL, "mark",		\
			   darwin_pragma_ignore, false);	\
    c_register_pragma (0, "options", darwin_pragma_options);	\
    c_register_pragma (0, "segment", darwin_pragma_ignore);	\
    c_register_pragma (0, "unused", darwin_pragma_unused);	\
    c_register_pragma (0, "ms_struct", darwin_pragma_ms_struct); \
  } while (0)

#undef ASM_APP_ON
#define ASM_APP_ON ""
#undef ASM_APP_OFF
#define ASM_APP_OFF ""

#define TARGET_POSIX_IO

/* All new versions of Darwin have C99 functions.  */

#define TARGET_C99_FUNCTIONS 1

/* Darwin disables section anchors by default.  
   They should be enabled per arch where support exists in that arch.  */
#define TARGET_ASM_OUTPUT_ANCHOR NULL
#define DARWIN_SECTION_ANCHORS 0

#define HAVE_ENABLE_EXECUTE_STACK

/* For Apple KEXTs, we make the constructors return this to match gcc
   2.95.  */
#define TARGET_CXX_CDTOR_RETURNS_THIS (darwin_kextabi_p)
#define TARGET_KEXTABI flag_apple_kext

/* We have target-specific builtins.  */
#define TARGET_FOLD_BUILTIN darwin_fold_builtin

#define TARGET_N_FORMAT_TYPES 1
#define TARGET_FORMAT_TYPES darwin_additional_format_types

#ifndef USED_FOR_TARGET
extern void darwin_driver_init (unsigned int *,struct cl_decoded_option **);
#define GCC_DRIVER_HOST_INITIALIZATION \
  darwin_driver_init (&decoded_options_count, &decoded_options)
#endif

/* The Apple assembler and linker do not support constructor priorities.  */
#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 0

/* When building cross-compilers (and native crosses) we shall default to 
   providing an osx-version-min of this unless overridden by the User.  */
#define DEF_MIN_OSX_VERSION "10.4"

#endif /* CONFIG_DARWIN_H */
