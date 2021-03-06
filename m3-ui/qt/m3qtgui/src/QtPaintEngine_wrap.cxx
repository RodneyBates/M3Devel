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


#include <QtGui/qpaintengine.h>
#define RenderFlags QTextItem::RenderFlags
#define PaintEngineFeatures QPaintEngine::PaintEngineFeatures
#define DirtyFlags QPaintEngine::DirtyFlags


#ifdef __cplusplus
extern "C" {
#endif

SWIGEXPORT double QTextItem_descent(QTextItem const * self) {
  QTextItem *arg1 = (QTextItem *) 0 ;
  double result;
  double cresult ;
  
  arg1 = *(QTextItem **)&self; 
  result = (double)((QTextItem const *)arg1)->descent();
  cresult = result; 
  return cresult;
}


SWIGEXPORT double QTextItem_ascent(QTextItem const * self) {
  QTextItem *arg1 = (QTextItem *) 0 ;
  double result;
  double cresult ;
  
  arg1 = *(QTextItem **)&self; 
  result = (double)((QTextItem const *)arg1)->ascent();
  cresult = result; 
  return cresult;
}


SWIGEXPORT double QTextItem_width(QTextItem const * self) {
  QTextItem *arg1 = (QTextItem *) 0 ;
  double result;
  double cresult ;
  
  arg1 = *(QTextItem **)&self; 
  result = (double)((QTextItem const *)arg1)->width();
  cresult = result; 
  return cresult;
}


SWIGEXPORT RenderFlags QTextItem_renderFlags(QTextItem const * self) {
  QTextItem *arg1 = (QTextItem *) 0 ;
  RenderFlags cresult ;
  
  arg1 = *(QTextItem **)&self; 
  *(RenderFlags **)&cresult = new RenderFlags((const RenderFlags &)((QTextItem const *)arg1)->renderFlags());
  return cresult;
}


SWIGEXPORT QString * QTextItem_text(QTextItem const * self) {
  QTextItem *arg1 = (QTextItem *) 0 ;
  QString * cresult ;
  
  arg1 = *(QTextItem **)&self; 
  *(QString **)&cresult = new QString((const QString &)((QTextItem const *)arg1)->text());
  return cresult;
}


SWIGEXPORT QFont * QTextItem_font(QTextItem const * self) {
  QTextItem *arg1 = (QTextItem *) 0 ;
  QFont * cresult ;
  
  arg1 = *(QTextItem **)&self; 
  *(QFont **)&cresult = new QFont((const QFont &)((QTextItem const *)arg1)->font());
  return cresult;
}


SWIGEXPORT void Delete_QTextItem(QTextItem * self) {
  QTextItem *arg1 = (QTextItem *) 0 ;
  
  arg1 = *(QTextItem **)&self; 
  delete arg1;
}


SWIGEXPORT void Delete_QPaintEngine(QPaintEngine * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  delete arg1;
}


SWIGEXPORT bool QPaintEngine_isActive(QPaintEngine const * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  result = (bool)((QPaintEngine const *)arg1)->isActive();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QPaintEngine_setActive(QPaintEngine * self, bool newState) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  bool arg2 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = newState ? true : false; 
  (arg1)->setActive(arg2);
}


SWIGEXPORT void QPaintEngine_drawRects(QPaintEngine * self, QRect * rects, int rectCount) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRect *arg2 = (QRect *) 0 ;
  int arg3 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRect **)&rects; 
  arg3 = (int)rectCount; 
  (arg1)->drawRects((QRect const *)arg2,arg3);
}


SWIGEXPORT void QPaintEngine_drawRects1(QPaintEngine * self, QRectF * rects, int rectCount) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRectF *arg2 = (QRectF *) 0 ;
  int arg3 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRectF **)&rects; 
  arg3 = (int)rectCount; 
  (arg1)->drawRects((QRectF const *)arg2,arg3);
}


SWIGEXPORT void QPaintEngine_drawLines(QPaintEngine * self, QLine * lines, int lineCount) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QLine *arg2 = (QLine *) 0 ;
  int arg3 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QLine **)&lines; 
  arg3 = (int)lineCount; 
  (arg1)->drawLines((QLine const *)arg2,arg3);
}


SWIGEXPORT void QPaintEngine_drawLines1(QPaintEngine * self, QLineF * lines, int lineCount) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QLineF *arg2 = (QLineF *) 0 ;
  int arg3 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QLineF **)&lines; 
  arg3 = (int)lineCount; 
  (arg1)->drawLines((QLineF const *)arg2,arg3);
}


SWIGEXPORT void QPaintEngine_drawEllipse(QPaintEngine * self, QRectF * r) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRectF *arg2 = 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRectF **)&r;
  (arg1)->drawEllipse((QRectF const &)*arg2);
}


SWIGEXPORT void QPaintEngine_drawEllipse1(QPaintEngine * self, QRect * r) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRect *arg2 = 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRect **)&r;
  (arg1)->drawEllipse((QRect const &)*arg2);
}


SWIGEXPORT void QPaintEngine_drawPoints(QPaintEngine * self, QPointF * points, int pointCount) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QPointF *arg2 = (QPointF *) 0 ;
  int arg3 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QPointF **)&points; 
  arg3 = (int)pointCount; 
  (arg1)->drawPoints((QPointF const *)arg2,arg3);
}


SWIGEXPORT void QPaintEngine_drawPoints1(QPaintEngine * self, QPoint * points, int pointCount) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QPoint *arg2 = (QPoint *) 0 ;
  int arg3 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QPoint **)&points; 
  arg3 = (int)pointCount; 
  (arg1)->drawPoints((QPoint const *)arg2,arg3);
}


SWIGEXPORT void QPaintEngine_drawPolygon(QPaintEngine * self, QPointF * points, int pointCount, QPaintEngine::PolygonDrawMode mode) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QPointF *arg2 = (QPointF *) 0 ;
  int arg3 ;
  QPaintEngine::PolygonDrawMode arg4 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QPointF **)&points; 
  arg3 = (int)pointCount; 
  arg4 = (QPaintEngine::PolygonDrawMode)mode; 
  (arg1)->drawPolygon((QPointF const *)arg2,arg3,arg4);
}


SWIGEXPORT void QPaintEngine_drawPolygon1(QPaintEngine * self, QPoint * points, int pointCount, QPaintEngine::PolygonDrawMode mode) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QPoint *arg2 = (QPoint *) 0 ;
  int arg3 ;
  QPaintEngine::PolygonDrawMode arg4 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QPoint **)&points; 
  arg3 = (int)pointCount; 
  arg4 = (QPaintEngine::PolygonDrawMode)mode; 
  (arg1)->drawPolygon((QPoint const *)arg2,arg3,arg4);
}


SWIGEXPORT void QPaintEngine_drawTiledPixmap(QPaintEngine * self, QRectF * r, QPixmap * pixmap, QPointF * s) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRectF *arg2 = 0 ;
  QPixmap *arg3 = 0 ;
  QPointF *arg4 = 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRectF **)&r;
  arg3 = *(QPixmap **)&pixmap;
  arg4 = *(QPointF **)&s;
  (arg1)->drawTiledPixmap((QRectF const &)*arg2,(QPixmap const &)*arg3,(QPointF const &)*arg4);
}


SWIGEXPORT void QPaintEngine_drawImage(QPaintEngine * self, QRectF * r, QImage * pm, QRectF * sr, Qt::ImageConversionFlags flags) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRectF *arg2 = 0 ;
  QImage *arg3 = 0 ;
  QRectF *arg4 = 0 ;
  Qt::ImageConversionFlags arg5 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRectF **)&r;
  arg3 = *(QImage **)&pm;
  arg4 = *(QRectF **)&sr;
  arg5 = (Qt::ImageConversionFlags)flags; 
  (arg1)->drawImage((QRectF const &)*arg2,(QImage const &)*arg3,(QRectF const &)*arg4,arg5);
}


SWIGEXPORT void QPaintEngine_drawImage1(QPaintEngine * self, QRectF * r, QImage * pm, QRectF * sr) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRectF *arg2 = 0 ;
  QImage *arg3 = 0 ;
  QRectF *arg4 = 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRectF **)&r;
  arg3 = *(QImage **)&pm;
  arg4 = *(QRectF **)&sr;
  (arg1)->drawImage((QRectF const &)*arg2,(QImage const &)*arg3,(QRectF const &)*arg4);
}


SWIGEXPORT void QPaintEngine_setPaintDevice(QPaintEngine * self, QPaintDevice * device) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QPaintDevice *arg2 = (QPaintDevice *) 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QPaintDevice **)&device; 
  (arg1)->setPaintDevice(arg2);
}


SWIGEXPORT QPaintDevice * QPaintEngine_paintDevice(QPaintEngine const * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QPaintDevice *result = 0 ;
  QPaintDevice * cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  result = (QPaintDevice *)((QPaintEngine const *)arg1)->paintDevice();
  *(QPaintDevice **)&cresult = result; 
  return cresult;
}


SWIGEXPORT void QPaintEngine_setSystemClip(QPaintEngine * self, QRegion * baseClip) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRegion *arg2 = 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRegion **)&baseClip;
  (arg1)->setSystemClip((QRegion const &)*arg2);
}


SWIGEXPORT QRegion * QPaintEngine_systemClip(QPaintEngine const * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRegion * cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  *(QRegion **)&cresult = new QRegion((const QRegion &)((QPaintEngine const *)arg1)->systemClip());
  return cresult;
}


SWIGEXPORT void QPaintEngine_setSystemRect(QPaintEngine * self, QRect * rect) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRect *arg2 = 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(QRect **)&rect;
  (arg1)->setSystemRect((QRect const &)*arg2);
}


SWIGEXPORT QRect * QPaintEngine_systemRect(QPaintEngine const * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QRect * cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  *(QRect **)&cresult = new QRect((const QRect &)((QPaintEngine const *)arg1)->systemRect());
  return cresult;
}


SWIGEXPORT QPoint * QPaintEngine_coordinateOffset(QPaintEngine const * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  QPoint * cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  *(QPoint **)&cresult = new QPoint((const QPoint &)((QPaintEngine const *)arg1)->coordinateOffset());
  return cresult;
}


SWIGEXPORT void QPaintEngine_fix_neg_rect(QPaintEngine * self, int * x, int * y, int * w, int * h) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  int *arg2 = (int *) 0 ;
  int *arg3 = (int *) 0 ;
  int *arg4 = (int *) 0 ;
  int *arg5 = (int *) 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = *(int **)&x; 
  arg3 = *(int **)&y; 
  arg4 = *(int **)&w; 
  arg5 = *(int **)&h; 
  (arg1)->fix_neg_rect(arg2,arg3,arg4,arg5);
}


SWIGEXPORT bool QPaintEngine_testDirty(QPaintEngine * self, DirtyFlags df) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  DirtyFlags arg2 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = (DirtyFlags)df; 
  result = (bool)(arg1)->testDirty(arg2);
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QPaintEngine_setDirty(QPaintEngine * self, DirtyFlags df) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  DirtyFlags arg2 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = (DirtyFlags)df; 
  (arg1)->setDirty(arg2);
}


SWIGEXPORT void QPaintEngine_clearDirty(QPaintEngine * self, DirtyFlags df) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  DirtyFlags arg2 ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = (DirtyFlags)df; 
  (arg1)->clearDirty(arg2);
}


SWIGEXPORT bool QPaintEngine_hasFeature(QPaintEngine const * self, PaintEngineFeatures feature) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  PaintEngineFeatures arg2 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  arg2 = (PaintEngineFeatures)feature; 
  result = (bool)((QPaintEngine const *)arg1)->hasFeature(arg2);
  cresult = result; 
  return cresult;
}


SWIGEXPORT void QPaintEngine_syncState(QPaintEngine * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  
  arg1 = *(QPaintEngine **)&self; 
  (arg1)->syncState();
}


SWIGEXPORT bool QPaintEngine_isExtended(QPaintEngine const * self) {
  QPaintEngine *arg1 = (QPaintEngine *) 0 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QPaintEngine **)&self; 
  result = (bool)((QPaintEngine const *)arg1)->isExtended();
  cresult = result; 
  return cresult;
}


SWIGEXPORT QPaintEngine::DirtyFlags QPaintEngineState_state(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QPaintEngine::DirtyFlags cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QPaintEngine::DirtyFlags **)&cresult = new QPaintEngine::DirtyFlags((const QPaintEngine::DirtyFlags &)((QPaintEngineState const *)arg1)->state());
  return cresult;
}


SWIGEXPORT QPen * QPaintEngineState_pen(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QPen * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QPen **)&cresult = new QPen((const QPen &)((QPaintEngineState const *)arg1)->pen());
  return cresult;
}


SWIGEXPORT QBrush * QPaintEngineState_brush(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QBrush * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QBrush **)&cresult = new QBrush((const QBrush &)((QPaintEngineState const *)arg1)->brush());
  return cresult;
}


SWIGEXPORT QPointF * QPaintEngineState_brushOrigin(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QPointF * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QPointF **)&cresult = new QPointF((const QPointF &)((QPaintEngineState const *)arg1)->brushOrigin());
  return cresult;
}


SWIGEXPORT QBrush * QPaintEngineState_backgroundBrush(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QBrush * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QBrush **)&cresult = new QBrush((const QBrush &)((QPaintEngineState const *)arg1)->backgroundBrush());
  return cresult;
}


SWIGEXPORT Qt::BGMode QPaintEngineState_backgroundMode(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  Qt::BGMode result;
  Qt::BGMode cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  result = (Qt::BGMode)((QPaintEngineState const *)arg1)->backgroundMode();
  cresult = result; 
  return cresult;
}


SWIGEXPORT QFont * QPaintEngineState_font(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QFont * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QFont **)&cresult = new QFont((const QFont &)((QPaintEngineState const *)arg1)->font());
  return cresult;
}


SWIGEXPORT QMatrix * QPaintEngineState_matrix(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QMatrix * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QMatrix **)&cresult = new QMatrix((const QMatrix &)((QPaintEngineState const *)arg1)->matrix());
  return cresult;
}


SWIGEXPORT QTransform * QPaintEngineState_transform(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QTransform * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QTransform **)&cresult = new QTransform((const QTransform &)((QPaintEngineState const *)arg1)->transform());
  return cresult;
}


SWIGEXPORT Qt::ClipOperation QPaintEngineState_clipOperation(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  Qt::ClipOperation result;
  Qt::ClipOperation cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  result = (Qt::ClipOperation)((QPaintEngineState const *)arg1)->clipOperation();
  cresult = result; 
  return cresult;
}


SWIGEXPORT QRegion * QPaintEngineState_clipRegion(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  QRegion * cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  *(QRegion **)&cresult = new QRegion((const QRegion &)((QPaintEngineState const *)arg1)->clipRegion());
  return cresult;
}


SWIGEXPORT bool QPaintEngineState_isClipEnabled(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  result = (bool)((QPaintEngineState const *)arg1)->isClipEnabled();
  cresult = result; 
  return cresult;
}


SWIGEXPORT double QPaintEngineState_opacity(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  double result;
  double cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  result = (double)((QPaintEngineState const *)arg1)->opacity();
  cresult = result; 
  return cresult;
}


SWIGEXPORT bool QPaintEngineState_brushNeedsResolving(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  result = (bool)((QPaintEngineState const *)arg1)->brushNeedsResolving();
  cresult = result; 
  return cresult;
}


SWIGEXPORT bool QPaintEngineState_penNeedsResolving(QPaintEngineState const * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  bool result;
  bool cresult ;
  
  arg1 = *(QPaintEngineState **)&self; 
  result = (bool)((QPaintEngineState const *)arg1)->penNeedsResolving();
  cresult = result; 
  return cresult;
}


SWIGEXPORT void Delete_QPaintEngineState(QPaintEngineState * self) {
  QPaintEngineState *arg1 = (QPaintEngineState *) 0 ;
  
  arg1 = *(QPaintEngineState **)&self; 
  delete arg1;
}


#ifdef __cplusplus
}
#endif

