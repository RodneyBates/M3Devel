(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

UNSAFE MODULE QtCalendarWidget;


FROM QtSize IMPORT QSize;
FROM QtDateTime IMPORT QDate;
IMPORT QtCalendarWidgetRaw;
FROM QtWidget IMPORT QWidget;
FROM QtTextFormat IMPORT QTextCharFormat;
FROM QtNamespace IMPORT DayOfWeek;


IMPORT WeakRef;

PROCEDURE New_QCalendarWidget0 (self: QCalendarWidget; parent: QWidget; ):
  QCalendarWidget =
  VAR
    result : ADDRESS;
    arg1tmp          := LOOPHOLE(parent.cxxObj, ADDRESS);
  BEGIN
    result := QtCalendarWidgetRaw.New_QCalendarWidget0(arg1tmp);

    self.cxxObj := result;
    EVAL WeakRef.FromRef(self, Cleanup_QCalendarWidget);

    RETURN self;
  END New_QCalendarWidget0;

PROCEDURE New_QCalendarWidget1 (self: QCalendarWidget; ): QCalendarWidget =
  VAR result: ADDRESS;
  BEGIN
    result := QtCalendarWidgetRaw.New_QCalendarWidget1();

    self.cxxObj := result;
    EVAL WeakRef.FromRef(self, Cleanup_QCalendarWidget);

    RETURN self;
  END New_QCalendarWidget1;

PROCEDURE Delete_QCalendarWidget (self: QCalendarWidget; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.Delete_QCalendarWidget(selfAdr);
  END Delete_QCalendarWidget;

PROCEDURE QCalendarWidget_sizeHint (self: QCalendarWidget; ): QSize =
  VAR
    ret    : ADDRESS;
    result : QSize;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtCalendarWidgetRaw.QCalendarWidget_sizeHint(selfAdr);

    result := NEW(QSize);
    result.cxxObj := ret;
    result.destroyCxx();

    RETURN result;
  END QCalendarWidget_sizeHint;

PROCEDURE QCalendarWidget_minimumSizeHint (self: QCalendarWidget; ):
  QSize =
  VAR
    ret    : ADDRESS;
    result : QSize;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtCalendarWidgetRaw.QCalendarWidget_minimumSizeHint(selfAdr);

    result := NEW(QSize);
    result.cxxObj := ret;
    result.destroyCxx();

    RETURN result;
  END QCalendarWidget_minimumSizeHint;

PROCEDURE QCalendarWidget_selectedDate (self: QCalendarWidget; ): QDate =
  VAR
    ret    : ADDRESS;
    result : QDate;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtCalendarWidgetRaw.QCalendarWidget_selectedDate(selfAdr);

    result := NEW(QDate);
    result.cxxObj := ret;
    result.destroyCxx();

    RETURN result;
  END QCalendarWidget_selectedDate;

PROCEDURE QCalendarWidget_yearShown (self: QCalendarWidget; ): INTEGER =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtCalendarWidgetRaw.QCalendarWidget_yearShown(selfAdr);
  END QCalendarWidget_yearShown;

PROCEDURE QCalendarWidget_monthShown (self: QCalendarWidget; ): INTEGER =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtCalendarWidgetRaw.QCalendarWidget_monthShown(selfAdr);
  END QCalendarWidget_monthShown;

PROCEDURE QCalendarWidget_minimumDate (self: QCalendarWidget; ): QDate =
  VAR
    ret    : ADDRESS;
    result : QDate;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtCalendarWidgetRaw.QCalendarWidget_minimumDate(selfAdr);

    result := NEW(QDate);
    result.cxxObj := ret;
    result.destroyCxx();

    RETURN result;
  END QCalendarWidget_minimumDate;

PROCEDURE QCalendarWidget_setMinimumDate
  (self: QCalendarWidget; date: QDate; ) =
  VAR
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
    arg2tmp          := LOOPHOLE(date.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setMinimumDate(selfAdr, arg2tmp);
  END QCalendarWidget_setMinimumDate;

PROCEDURE QCalendarWidget_maximumDate (self: QCalendarWidget; ): QDate =
  VAR
    ret    : ADDRESS;
    result : QDate;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtCalendarWidgetRaw.QCalendarWidget_maximumDate(selfAdr);

    result := NEW(QDate);
    result.cxxObj := ret;
    result.destroyCxx();

    RETURN result;
  END QCalendarWidget_maximumDate;

PROCEDURE QCalendarWidget_setMaximumDate
  (self: QCalendarWidget; date: QDate; ) =
  VAR
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
    arg2tmp          := LOOPHOLE(date.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setMaximumDate(selfAdr, arg2tmp);
  END QCalendarWidget_setMaximumDate;

PROCEDURE QCalendarWidget_firstDayOfWeek (self: QCalendarWidget; ):
  DayOfWeek =
  VAR
    ret    : INTEGER;
    result : DayOfWeek;
    selfAdr: ADDRESS   := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtCalendarWidgetRaw.QCalendarWidget_firstDayOfWeek(selfAdr);
    result := VAL(ret, DayOfWeek);
    RETURN result;
  END QCalendarWidget_firstDayOfWeek;

PROCEDURE QCalendarWidget_setFirstDayOfWeek
  (self: QCalendarWidget; dayOfWeek: DayOfWeek; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setFirstDayOfWeek(
      selfAdr, ORD(dayOfWeek));
  END QCalendarWidget_setFirstDayOfWeek;

PROCEDURE QCalendarWidget_isHeaderVisible (self: QCalendarWidget; ):
  BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtCalendarWidgetRaw.QCalendarWidget_isHeaderVisible(selfAdr);
  END QCalendarWidget_isHeaderVisible;

PROCEDURE QCalendarWidget_setHeaderVisible
  (self: QCalendarWidget; show: BOOLEAN; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setHeaderVisible(selfAdr, show);
  END QCalendarWidget_setHeaderVisible;

PROCEDURE QCalendarWidget_isNavigationBarVisible (self: QCalendarWidget; ):
  BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN
      QtCalendarWidgetRaw.QCalendarWidget_isNavigationBarVisible(selfAdr);
  END QCalendarWidget_isNavigationBarVisible;

PROCEDURE QCalendarWidget_isGridVisible (self: QCalendarWidget; ):
  BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtCalendarWidgetRaw.QCalendarWidget_isGridVisible(selfAdr);
  END QCalendarWidget_isGridVisible;

PROCEDURE QCalendarWidget_selectionMode (self: QCalendarWidget; ):
  SelectionMode =
  VAR
    ret    : INTEGER;
    result : SelectionMode;
    selfAdr: ADDRESS       := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtCalendarWidgetRaw.QCalendarWidget_selectionMode(selfAdr);
    result := VAL(ret, SelectionMode);
    RETURN result;
  END QCalendarWidget_selectionMode;

PROCEDURE QCalendarWidget_setSelectionMode
  (self: QCalendarWidget; mode: SelectionMode; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setSelectionMode(
      selfAdr, ORD(mode));
  END QCalendarWidget_setSelectionMode;

PROCEDURE QCalendarWidget_horizontalHeaderFormat (self: QCalendarWidget; ):
  HorizontalHeaderFormat =
  VAR
    ret    : INTEGER;
    result : HorizontalHeaderFormat;
    selfAdr: ADDRESS                := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret :=
      QtCalendarWidgetRaw.QCalendarWidget_horizontalHeaderFormat(selfAdr);
    result := VAL(ret, HorizontalHeaderFormat);
    RETURN result;
  END QCalendarWidget_horizontalHeaderFormat;

PROCEDURE QCalendarWidget_setHorizontalHeaderFormat
  (self: QCalendarWidget; format: HorizontalHeaderFormat; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setHorizontalHeaderFormat(
      selfAdr, ORD(format));
  END QCalendarWidget_setHorizontalHeaderFormat;

PROCEDURE QCalendarWidget_verticalHeaderFormat (self: QCalendarWidget; ):
  VerticalHeaderFormat =
  VAR
    ret    : INTEGER;
    result : VerticalHeaderFormat;
    selfAdr: ADDRESS              := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret :=
      QtCalendarWidgetRaw.QCalendarWidget_verticalHeaderFormat(selfAdr);
    result := VAL(ret, VerticalHeaderFormat);
    RETURN result;
  END QCalendarWidget_verticalHeaderFormat;

PROCEDURE QCalendarWidget_setVerticalHeaderFormat
  (self: QCalendarWidget; format: VerticalHeaderFormat; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setVerticalHeaderFormat(
      selfAdr, ORD(format));
  END QCalendarWidget_setVerticalHeaderFormat;

PROCEDURE QCalendarWidget_setHeaderTextFormat
  (self: QCalendarWidget; format: QTextCharFormat; ) =
  VAR
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
    arg2tmp          := LOOPHOLE(format.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setHeaderTextFormat(
      selfAdr, arg2tmp);
  END QCalendarWidget_setHeaderTextFormat;

PROCEDURE QCalendarWidget_setWeekdayTextFormat
  (self: QCalendarWidget; dayOfWeek: DayOfWeek; format: QTextCharFormat; ) =
  VAR
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
    arg3tmp          := LOOPHOLE(format.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setWeekdayTextFormat(
      selfAdr, ORD(dayOfWeek), arg3tmp);
  END QCalendarWidget_setWeekdayTextFormat;

PROCEDURE QCalendarWidget_setDateTextFormat
  (self: QCalendarWidget; date: QDate; format: QTextCharFormat; ) =
  VAR
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
    arg2tmp          := LOOPHOLE(date.cxxObj, ADDRESS);
    arg3tmp          := LOOPHOLE(format.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setDateTextFormat(
      selfAdr, arg2tmp, arg3tmp);
  END QCalendarWidget_setDateTextFormat;

PROCEDURE QCalendarWidget_isDateEditEnabled (self: QCalendarWidget; ):
  BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtCalendarWidgetRaw.QCalendarWidget_isDateEditEnabled(selfAdr);
  END QCalendarWidget_isDateEditEnabled;

PROCEDURE QCalendarWidget_setDateEditEnabled
  (self: QCalendarWidget; enable: BOOLEAN; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setDateEditEnabled(selfAdr, enable);
  END QCalendarWidget_setDateEditEnabled;

PROCEDURE QCalendarWidget_dateEditAcceptDelay (self: QCalendarWidget; ):
  INTEGER =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN
      QtCalendarWidgetRaw.QCalendarWidget_dateEditAcceptDelay(selfAdr);
  END QCalendarWidget_dateEditAcceptDelay;

PROCEDURE QCalendarWidget_setDateEditAcceptDelay
  (self: QCalendarWidget; delay: INTEGER; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setDateEditAcceptDelay(
      selfAdr, delay);
  END QCalendarWidget_setDateEditAcceptDelay;

PROCEDURE QCalendarWidget_setSelectedDate
  (self: QCalendarWidget; date: QDate; ) =
  VAR
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
    arg2tmp          := LOOPHOLE(date.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setSelectedDate(selfAdr, arg2tmp);
  END QCalendarWidget_setSelectedDate;

PROCEDURE QCalendarWidget_setDateRange
  (self: QCalendarWidget; min, max: QDate; ) =
  VAR
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
    arg2tmp          := LOOPHOLE(min.cxxObj, ADDRESS);
    arg3tmp          := LOOPHOLE(max.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setDateRange(
      selfAdr, arg2tmp, arg3tmp);
  END QCalendarWidget_setDateRange;

PROCEDURE QCalendarWidget_setCurrentPage
  (self: QCalendarWidget; year, month: INTEGER; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setCurrentPage(
      selfAdr, year, month);
  END QCalendarWidget_setCurrentPage;

PROCEDURE QCalendarWidget_setGridVisible
  (self: QCalendarWidget; show: BOOLEAN; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setGridVisible(selfAdr, show);
  END QCalendarWidget_setGridVisible;

PROCEDURE QCalendarWidget_setNavigationBarVisible
  (self: QCalendarWidget; visible: BOOLEAN; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_setNavigationBarVisible(
      selfAdr, visible);
  END QCalendarWidget_setNavigationBarVisible;

PROCEDURE QCalendarWidget_showNextMonth (self: QCalendarWidget; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_showNextMonth(selfAdr);
  END QCalendarWidget_showNextMonth;

PROCEDURE QCalendarWidget_showPreviousMonth (self: QCalendarWidget; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_showPreviousMonth(selfAdr);
  END QCalendarWidget_showPreviousMonth;

PROCEDURE QCalendarWidget_showNextYear (self: QCalendarWidget; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_showNextYear(selfAdr);
  END QCalendarWidget_showNextYear;

PROCEDURE QCalendarWidget_showPreviousYear (self: QCalendarWidget; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_showPreviousYear(selfAdr);
  END QCalendarWidget_showPreviousYear;

PROCEDURE QCalendarWidget_showSelectedDate (self: QCalendarWidget; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_showSelectedDate(selfAdr);
  END QCalendarWidget_showSelectedDate;

PROCEDURE QCalendarWidget_showToday (self: QCalendarWidget; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtCalendarWidgetRaw.QCalendarWidget_showToday(selfAdr);
  END QCalendarWidget_showToday;

PROCEDURE Cleanup_QCalendarWidget
  (<* UNUSED *> READONLY self: WeakRef.T; ref: REFANY) =
  VAR obj: QCalendarWidget := ref;
  BEGIN
    Delete_QCalendarWidget(obj);
  END Cleanup_QCalendarWidget;

PROCEDURE Destroy_QCalendarWidget (self: QCalendarWidget) =
  BEGIN
    EVAL WeakRef.FromRef(self, Cleanup_QCalendarWidget);
  END Destroy_QCalendarWidget;

REVEAL
  QCalendarWidget =
    QCalendarWidgetPublic BRANDED OBJECT
    OVERRIDES
      init_0                 := New_QCalendarWidget0;
      init_1                 := New_QCalendarWidget1;
      sizeHint               := QCalendarWidget_sizeHint;
      minimumSizeHint        := QCalendarWidget_minimumSizeHint;
      selectedDate           := QCalendarWidget_selectedDate;
      yearShown              := QCalendarWidget_yearShown;
      monthShown             := QCalendarWidget_monthShown;
      minimumDate            := QCalendarWidget_minimumDate;
      setMinimumDate         := QCalendarWidget_setMinimumDate;
      maximumDate            := QCalendarWidget_maximumDate;
      setMaximumDate         := QCalendarWidget_setMaximumDate;
      firstDayOfWeek         := QCalendarWidget_firstDayOfWeek;
      setFirstDayOfWeek      := QCalendarWidget_setFirstDayOfWeek;
      isHeaderVisible        := QCalendarWidget_isHeaderVisible;
      setHeaderVisible       := QCalendarWidget_setHeaderVisible;
      isNavigationBarVisible := QCalendarWidget_isNavigationBarVisible;
      isGridVisible          := QCalendarWidget_isGridVisible;
      selectionMode          := QCalendarWidget_selectionMode;
      setSelectionMode       := QCalendarWidget_setSelectionMode;
      horizontalHeaderFormat := QCalendarWidget_horizontalHeaderFormat;
      setHorizontalHeaderFormat := QCalendarWidget_setHorizontalHeaderFormat;
      verticalHeaderFormat    := QCalendarWidget_verticalHeaderFormat;
      setVerticalHeaderFormat := QCalendarWidget_setVerticalHeaderFormat;
      setHeaderTextFormat     := QCalendarWidget_setHeaderTextFormat;
      setWeekdayTextFormat    := QCalendarWidget_setWeekdayTextFormat;
      setDateTextFormat       := QCalendarWidget_setDateTextFormat;
      isDateEditEnabled       := QCalendarWidget_isDateEditEnabled;
      setDateEditEnabled      := QCalendarWidget_setDateEditEnabled;
      dateEditAcceptDelay     := QCalendarWidget_dateEditAcceptDelay;
      setDateEditAcceptDelay  := QCalendarWidget_setDateEditAcceptDelay;
      setSelectedDate         := QCalendarWidget_setSelectedDate;
      setDateRange            := QCalendarWidget_setDateRange;
      setCurrentPage          := QCalendarWidget_setCurrentPage;
      setGridVisible          := QCalendarWidget_setGridVisible;
      setNavigationBarVisible := QCalendarWidget_setNavigationBarVisible;
      showNextMonth           := QCalendarWidget_showNextMonth;
      showPreviousMonth       := QCalendarWidget_showPreviousMonth;
      showNextYear            := QCalendarWidget_showNextYear;
      showPreviousYear        := QCalendarWidget_showPreviousYear;
      showSelectedDate        := QCalendarWidget_showSelectedDate;
      showToday               := QCalendarWidget_showToday;
      destroyCxx              := Destroy_QCalendarWidget;
    END;


BEGIN
END QtCalendarWidget.
