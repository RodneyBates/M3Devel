/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 * 
 * This file is not intended to be easily readable and contains a number of 
 * coding conventions designed to improve portability and efficiency. Do not make
 * changes to this file unless you know what you are doing--modify the SWIG 
 * interface file instead. 
 * ----------------------------------------------------------------------------- */

#define SWIGMODULA3


#ifdef __cplusplus
/* SwigValueWrapper is described in swig.swg */
template<typename T> class SwigValueWrapper {
  struct SwigMovePointer {
    T *ptr;
    SwigMovePointer(T *p) : ptr(p) { }
    ~SwigMovePointer() { delete ptr; }
    SwigMovePointer& operator=(SwigMovePointer& rhs) { T* oldptr = ptr; ptr = 0; delete oldptr; ptr = rhs.ptr; rhs.ptr = 0; return *this; }
  } pointer;
  SwigValueWrapper& operator=(const SwigValueWrapper<T>& rhs);
  SwigValueWrapper(const SwigValueWrapper<T>& rhs);
public:
  SwigValueWrapper() : pointer(0) { }
  SwigValueWrapper& operator=(const T& t) { SwigMovePointer tmp(new T(t)); pointer = tmp; return *this; }
  operator T&() const { return *pointer.ptr; }
  T *operator&() { return pointer.ptr; }
};

template <typename T> T SwigValueInit() {
  return T();
}
#endif

/* -----------------------------------------------------------------------------
 *  This section contains generic SWIG labels for method/variable
 *  declarations/attributes, and other compiler dependent labels.
 * ----------------------------------------------------------------------------- */

/* template workaround for compilers that cannot correctly implement the C++ standard */
#ifndef SWIGTEMPLATEDISAMBIGUATOR
# if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x560)
#  define SWIGTEMPLATEDISAMBIGUATOR template
# elif defined(__HP_aCC)
/* Needed even with `aCC -AA' when `aCC -V' reports HP ANSI C++ B3910B A.03.55 */
/* If we find a maximum version that requires this, the test would be __HP_aCC <= 35500 for A.03.55 */
#  define SWIGTEMPLATEDISAMBIGUATOR template
# else
#  define SWIGTEMPLATEDISAMBIGUATOR
# endif
#endif

/* inline attribute */
#ifndef SWIGINLINE
# if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#   define SWIGINLINE inline
# else
#   define SWIGINLINE
# endif
#endif

/* attribute recognised by some compilers to avoid 'unused' warnings */
#ifndef SWIGUNUSED
# if defined(__GNUC__)
#   if !(defined(__cplusplus)) || (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4))
#     define SWIGUNUSED __attribute__ ((__unused__)) 
#   else
#     define SWIGUNUSED
#   endif
# elif defined(__ICC)
#   define SWIGUNUSED __attribute__ ((__unused__)) 
# else
#   define SWIGUNUSED 
# endif
#endif

#ifndef SWIG_MSC_UNSUPPRESS_4505
# if defined(_MSC_VER)
#   pragma warning(disable : 4505) /* unreferenced local function has been removed */
# endif 
#endif

#ifndef SWIGUNUSEDPARM
# ifdef __cplusplus
#   define SWIGUNUSEDPARM(p)
# else
#   define SWIGUNUSEDPARM(p) p SWIGUNUSED 
# endif
#endif

/* internal SWIG method */
#ifndef SWIGINTERN
# define SWIGINTERN static SWIGUNUSED
#endif

/* internal inline SWIG method */
#ifndef SWIGINTERNINLINE
# define SWIGINTERNINLINE SWIGINTERN SWIGINLINE
#endif

/* exporting methods */
#if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#  ifndef GCC_HASCLASSVISIBILITY
#    define GCC_HASCLASSVISIBILITY
#  endif
#endif

#ifndef SWIGEXPORT
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   if defined(STATIC_LINKED)
#     define SWIGEXPORT
#   else
#     define SWIGEXPORT __declspec(dllexport)
#   endif
# else
#   if defined(__GNUC__) && defined(GCC_HASCLASSVISIBILITY)
#     define SWIGEXPORT __attribute__ ((visibility("default")))
#   else
#     define SWIGEXPORT
#   endif
# endif
#endif

/* calling conventions for Windows */
#ifndef SWIGSTDCALL
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   define SWIGSTDCALL __stdcall
# else
#   define SWIGSTDCALL
# endif 
#endif

/* Deal with Microsoft's attempt at deprecating C standard runtime functions */
#if !defined(SWIG_NO_CRT_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_CRT_SECURE_NO_DEPRECATE)
# define _CRT_SECURE_NO_DEPRECATE
#endif

/* Deal with Microsoft's attempt at deprecating methods in the standard C++ library */
#if !defined(SWIG_NO_SCL_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_SCL_SECURE_NO_DEPRECATE)
# define _SCL_SECURE_NO_DEPRECATE
#endif




#include <stdlib.h>
#include <string.h>
#include <stdio.h>


#include <QtGui/qdialog.h>


#ifdef __cplusplus
extern "C" {
#endif

SWIGEXPORT QDialog * New_QDialog0(QWidget * parent, Qt::WindowFlags f) {
  QWidget *arg1 = (QWidget *) 0 ;
  Qt::WindowFlags arg2 ;
  QDialog *result = 0 ;
  QDialog * cresult ;
  
  arg1 = *(QWidget **)&parent; 
  arg2 = (Qt::WindowFlags)f; 
  result = (QDialog *)new QDialog(arg1,arg2);
  *(QDialog **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QDialog * New_QDialog1(QWidget * parent) {
  QWidget *arg1 = (QWidget *) 0 ;
  QDialog *result = 0 ;
  QDialog * cresult ;
  
  arg1 = *(QWidget **)&parent; 
  result = (QDialog *)new QDialog(arg1);
  *(QDialog **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QDialog * New_QDialog2() {
  QDialog *result = 0 ;
  QDialog * cresult ;
  
  result = (QDialog *)new QDialog();
  *(QDialog **)&cresult = result; 
  return cresult;
}


SWIGEXPORT void Delete_QDialog(QDialog * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  
  arg1 = *(QDialog **)&self; 
  delete arg1;
}


SWIGEXPORT int QDialog_result(QDialog const * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  int result;
  int cresult ;
  
  arg1 = *(QDialog **)&self; 
  result = (int)((QDialog const *)arg1)->result();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QDialog_setVisible(QDialog * self, bool visible) {
  QDialog *arg1 = (QDialog *) 0 ;
  bool arg2 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = visible ? true : false; 
  (arg1)->setVisible(arg2);
}


SWIGEXPORT void QDialog_setOrientation(QDialog * self, Qt::Orientation orientation) {
  QDialog *arg1 = (QDialog *) 0 ;
  Qt::Orientation arg2 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = (Qt::Orientation)orientation; 
  (arg1)->setOrientation(arg2);
}


SWIGEXPORT Qt::Orientation QDialog_orientation(QDialog const * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  Qt::Orientation result;
  Qt::Orientation cresult ;
  
  arg1 = *(QDialog **)&self; 
  result = (Qt::Orientation)((QDialog const *)arg1)->orientation();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QDialog_setExtension(QDialog * self, QWidget * extension) {
  QDialog *arg1 = (QDialog *) 0 ;
  QWidget *arg2 = (QWidget *) 0 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = *(QWidget **)&extension; 
  (arg1)->setExtension(arg2);
}


SWIGEXPORT QWidget * QDialog_extension(QDialog const * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  QWidget *result = 0 ;
  QWidget * cresult ;
  
  arg1 = *(QDialog **)&self; 
  result = (QWidget *)((QDialog const *)arg1)->extension();
  *(QWidget **)&cresult = result; 
  return cresult;
}


SWIGEXPORT QSize * QDialog_sizeHint(QDialog const * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  QSize * cresult ;
  
  arg1 = *(QDialog **)&self; 
  *(QSize **)&cresult = new QSize((const QSize &)((QDialog const *)arg1)->sizeHint());
  return cresult;
}


SWIGEXPORT QSize * QDialog_minimumSizeHint(QDialog const * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  QSize * cresult ;
  
  arg1 = *(QDialog **)&self; 
  *(QSize **)&cresult = new QSize((const QSize &)((QDialog const *)arg1)->minimumSizeHint());
  return cresult;
}


SWIGEXPORT void QDialog_setSizeGripEnabled(QDialog * self, bool m3arg2) {
  QDialog *arg1 = (QDialog *) 0 ;
  bool arg2 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = m3arg2 ? true : false; 
  (arg1)->setSizeGripEnabled(arg2);
}


SWIGEXPORT bool QDialog_isSizeGripEnabled(QDialog const * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QDialog **)&self; 
  result = (bool)((QDialog const *)arg1)->isSizeGripEnabled();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QDialog_setModal(QDialog * self, bool modal) {
  QDialog *arg1 = (QDialog *) 0 ;
  bool arg2 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = modal ? true : false; 
  (arg1)->setModal(arg2);
}


SWIGEXPORT void QDialog_setResult(QDialog * self, int r) {
  QDialog *arg1 = (QDialog *) 0 ;
  int arg2 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = (int)r; 
  (arg1)->setResult(arg2);
}


SWIGEXPORT void QDialog_open(QDialog * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  
  arg1 = *(QDialog **)&self; 
  (arg1)->open();
}


SWIGEXPORT int QDialog_exec(QDialog * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  int result;
  int cresult ;
  
  arg1 = *(QDialog **)&self; 
  result = (int)(arg1)->exec();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QDialog_done(QDialog * self, int m3arg2) {
  QDialog *arg1 = (QDialog *) 0 ;
  int arg2 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = (int)m3arg2; 
  (arg1)->done(arg2);
}


SWIGEXPORT void QDialog_accept(QDialog * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  
  arg1 = *(QDialog **)&self; 
  (arg1)->accept();
}


SWIGEXPORT void QDialog_reject(QDialog * self) {
  QDialog *arg1 = (QDialog *) 0 ;
  
  arg1 = *(QDialog **)&self; 
  (arg1)->reject();
}


SWIGEXPORT void QDialog_showExtension(QDialog * self, bool m3arg2) {
  QDialog *arg1 = (QDialog *) 0 ;
  bool arg2 ;
  
  arg1 = *(QDialog **)&self; 
  arg2 = m3arg2 ? true : false; 
  (arg1)->showExtension(arg2);
}


SWIGEXPORT long Modula3_QDialogToQWidget(long objectRef) {
    long baseptr = 0;
    *(QWidget **)&baseptr = *(QDialog **)&objectRef;
    return baseptr;
}

#ifdef __cplusplus
}
#endif

