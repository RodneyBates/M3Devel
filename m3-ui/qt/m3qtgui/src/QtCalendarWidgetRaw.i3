(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

INTERFACE QtCalendarWidgetRaw;


IMPORT Ctypes AS C;




<* EXTERNAL New_QCalendarWidget0 *>
PROCEDURE New_QCalendarWidget0 (parent: ADDRESS; ): QCalendarWidget;

<* EXTERNAL New_QCalendarWidget1 *>
PROCEDURE New_QCalendarWidget1 (): QCalendarWidget;

<* EXTERNAL Delete_QCalendarWidget *>
PROCEDURE Delete_QCalendarWidget (self: QCalendarWidget; );

<* EXTERNAL QCalendarWidget_sizeHint *>
PROCEDURE QCalendarWidget_sizeHint (self: QCalendarWidget; ): ADDRESS;

<* EXTERNAL QCalendarWidget_minimumSizeHint *>
PROCEDURE QCalendarWidget_minimumSizeHint (self: QCalendarWidget; ):
  ADDRESS;

<* EXTERNAL QCalendarWidget_selectedDate *>
PROCEDURE QCalendarWidget_selectedDate (self: QCalendarWidget; ): ADDRESS;

<* EXTERNAL QCalendarWidget_yearShown *>
PROCEDURE QCalendarWidget_yearShown (self: QCalendarWidget; ): C.int;

<* EXTERNAL QCalendarWidget_monthShown *>
PROCEDURE QCalendarWidget_monthShown (self: QCalendarWidget; ): C.int;

<* EXTERNAL QCalendarWidget_minimumDate *>
PROCEDURE QCalendarWidget_minimumDate (self: QCalendarWidget; ): ADDRESS;

<* EXTERNAL QCalendarWidget_setMinimumDate *>
PROCEDURE QCalendarWidget_setMinimumDate
  (self: QCalendarWidget; date: ADDRESS; );

<* EXTERNAL QCalendarWidget_maximumDate *>
PROCEDURE QCalendarWidget_maximumDate (self: QCalendarWidget; ): ADDRESS;

<* EXTERNAL QCalendarWidget_setMaximumDate *>
PROCEDURE QCalendarWidget_setMaximumDate
  (self: QCalendarWidget; date: ADDRESS; );

<* EXTERNAL QCalendarWidget_firstDayOfWeek *>
PROCEDURE QCalendarWidget_firstDayOfWeek (self: QCalendarWidget; ): C.int;

<* EXTERNAL QCalendarWidget_setFirstDayOfWeek *>
PROCEDURE QCalendarWidget_setFirstDayOfWeek
  (self: QCalendarWidget; dayOfWeek: C.int; );

<* EXTERNAL QCalendarWidget_isHeaderVisible *>
PROCEDURE QCalendarWidget_isHeaderVisible (self: QCalendarWidget; ):
  BOOLEAN;

<* EXTERNAL QCalendarWidget_setHeaderVisible *>
PROCEDURE QCalendarWidget_setHeaderVisible
  (self: QCalendarWidget; show: BOOLEAN; );

<* EXTERNAL QCalendarWidget_isNavigationBarVisible *>
PROCEDURE QCalendarWidget_isNavigationBarVisible (self: QCalendarWidget; ):
  BOOLEAN;

<* EXTERNAL QCalendarWidget_isGridVisible *>
PROCEDURE QCalendarWidget_isGridVisible (self: QCalendarWidget; ): BOOLEAN;

<* EXTERNAL QCalendarWidget_selectionMode *>
PROCEDURE QCalendarWidget_selectionMode (self: QCalendarWidget; ): C.int;

<* EXTERNAL QCalendarWidget_setSelectionMode *>
PROCEDURE QCalendarWidget_setSelectionMode
  (self: QCalendarWidget; mode: C.int; );

<* EXTERNAL QCalendarWidget_horizontalHeaderFormat *>
PROCEDURE QCalendarWidget_horizontalHeaderFormat (self: QCalendarWidget; ):
  C.int;

<* EXTERNAL QCalendarWidget_setHorizontalHeaderFormat *>
PROCEDURE QCalendarWidget_setHorizontalHeaderFormat
  (self: QCalendarWidget; format: C.int; );

<* EXTERNAL QCalendarWidget_verticalHeaderFormat *>
PROCEDURE QCalendarWidget_verticalHeaderFormat (self: QCalendarWidget; ):
  C.int;

<* EXTERNAL QCalendarWidget_setVerticalHeaderFormat *>
PROCEDURE QCalendarWidget_setVerticalHeaderFormat
  (self: QCalendarWidget; format: C.int; );

<* EXTERNAL QCalendarWidget_setHeaderTextFormat *>
PROCEDURE QCalendarWidget_setHeaderTextFormat
  (self: QCalendarWidget; format: ADDRESS; );

<* EXTERNAL QCalendarWidget_setWeekdayTextFormat *>
PROCEDURE QCalendarWidget_setWeekdayTextFormat
  (self: QCalendarWidget; dayOfWeek: C.int; format: ADDRESS; );

<* EXTERNAL QCalendarWidget_setDateTextFormat *>
PROCEDURE QCalendarWidget_setDateTextFormat
  (self: QCalendarWidget; date, format: ADDRESS; );

<* EXTERNAL QCalendarWidget_isDateEditEnabled *>
PROCEDURE QCalendarWidget_isDateEditEnabled (self: QCalendarWidget; ):
  BOOLEAN;

<* EXTERNAL QCalendarWidget_setDateEditEnabled *>
PROCEDURE QCalendarWidget_setDateEditEnabled
  (self: QCalendarWidget; enable: BOOLEAN; );

<* EXTERNAL QCalendarWidget_dateEditAcceptDelay *>
PROCEDURE QCalendarWidget_dateEditAcceptDelay (self: QCalendarWidget; ):
  C.int;

<* EXTERNAL QCalendarWidget_setDateEditAcceptDelay *>
PROCEDURE QCalendarWidget_setDateEditAcceptDelay
  (self: QCalendarWidget; delay: C.int; );

<* EXTERNAL QCalendarWidget_setSelectedDate *>
PROCEDURE QCalendarWidget_setSelectedDate
  (self: QCalendarWidget; date: ADDRESS; );

<* EXTERNAL QCalendarWidget_setDateRange *>
PROCEDURE QCalendarWidget_setDateRange
  (self: QCalendarWidget; min, max: ADDRESS; );

<* EXTERNAL QCalendarWidget_setCurrentPage *>
PROCEDURE QCalendarWidget_setCurrentPage
  (self: QCalendarWidget; year, month: C.int; );

<* EXTERNAL QCalendarWidget_setGridVisible *>
PROCEDURE QCalendarWidget_setGridVisible
  (self: QCalendarWidget; show: BOOLEAN; );

<* EXTERNAL QCalendarWidget_setNavigationBarVisible *>
PROCEDURE QCalendarWidget_setNavigationBarVisible
  (self: QCalendarWidget; visible: BOOLEAN; );

<* EXTERNAL QCalendarWidget_showNextMonth *>
PROCEDURE QCalendarWidget_showNextMonth (self: QCalendarWidget; );

<* EXTERNAL QCalendarWidget_showPreviousMonth *>
PROCEDURE QCalendarWidget_showPreviousMonth (self: QCalendarWidget; );

<* EXTERNAL QCalendarWidget_showNextYear *>
PROCEDURE QCalendarWidget_showNextYear (self: QCalendarWidget; );

<* EXTERNAL QCalendarWidget_showPreviousYear *>
PROCEDURE QCalendarWidget_showPreviousYear (self: QCalendarWidget; );

<* EXTERNAL QCalendarWidget_showSelectedDate *>
PROCEDURE QCalendarWidget_showSelectedDate (self: QCalendarWidget; );

<* EXTERNAL QCalendarWidget_showToday *>
PROCEDURE QCalendarWidget_showToday (self: QCalendarWidget; );

TYPE QCalendarWidget = ADDRESS;

END QtCalendarWidgetRaw.
