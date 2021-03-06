(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

UNSAFE MODULE QtEvent;


IMPORT QtEventRaw;


IMPORT WeakRef;
FROM QtObject IMPORT QObject;

PROCEDURE New_QEvent0 (self: QEvent; type: Type; ): QEvent =
  VAR result: ADDRESS;
  BEGIN
    result := QtEventRaw.New_QEvent0(ORD(type));

    self.cxxObj := result;
    EVAL WeakRef.FromRef(self, Cleanup_QEvent);

    RETURN self;
  END New_QEvent0;

PROCEDURE Delete_QEvent (self: QEvent; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtEventRaw.Delete_QEvent(selfAdr);
  END Delete_QEvent;

PROCEDURE QEvent_type (self: QEvent; ): Type =
  VAR
    ret    : INTEGER;
    result : Type;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtEventRaw.QEvent_type(selfAdr);
    result := VAL(ret, Type);
    RETURN result;
  END QEvent_type;

PROCEDURE QEvent_spontaneous (self: QEvent; ): BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtEventRaw.QEvent_spontaneous(selfAdr);
  END QEvent_spontaneous;

PROCEDURE QEvent_setAccepted (self: QEvent; accepted: BOOLEAN; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtEventRaw.QEvent_setAccepted(selfAdr, accepted);
  END QEvent_setAccepted;

PROCEDURE QEvent_isAccepted (self: QEvent; ): BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtEventRaw.QEvent_isAccepted(selfAdr);
  END QEvent_isAccepted;

PROCEDURE QEvent_accept (self: QEvent; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtEventRaw.QEvent_accept(selfAdr);
  END QEvent_accept;

PROCEDURE QEvent_ignore (self: QEvent; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtEventRaw.QEvent_ignore(selfAdr);
  END QEvent_ignore;

PROCEDURE RegisterEventType (hint: INTEGER; ): INTEGER =
  BEGIN
    RETURN QtEventRaw.RegisterEventType(hint);
  END RegisterEventType;

PROCEDURE Cleanup_QEvent
  (<* UNUSED *> READONLY self: WeakRef.T; ref: REFANY) =
  VAR obj: QEvent := ref;
  BEGIN
    Delete_QEvent(obj);
  END Cleanup_QEvent;

PROCEDURE Destroy_QEvent (self: QEvent) =
  BEGIN
    EVAL WeakRef.FromRef(self, Cleanup_QEvent);
  END Destroy_QEvent;

REVEAL
  QEvent = QEventPublic BRANDED OBJECT
           OVERRIDES
             init_0      := New_QEvent0;
             type        := QEvent_type;
             spontaneous := QEvent_spontaneous;
             setAccepted := QEvent_setAccepted;
             isAccepted  := QEvent_isAccepted;
             accept      := QEvent_accept;
             ignore      := QEvent_ignore;
             destroyCxx  := Destroy_QEvent;
           END;

PROCEDURE New_QTimerEvent0 (self: QTimerEvent; timerId: INTEGER; ):
  QTimerEvent =
  VAR result: ADDRESS;
  BEGIN
    result := QtEventRaw.New_QTimerEvent0(timerId);

    self.cxxObj := result;
    EVAL WeakRef.FromRef(self, Cleanup_QTimerEvent);

    RETURN self;
  END New_QTimerEvent0;

PROCEDURE Delete_QTimerEvent (self: QTimerEvent; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtEventRaw.Delete_QTimerEvent(selfAdr);
  END Delete_QTimerEvent;

PROCEDURE QTimerEvent_timerId (self: QTimerEvent; ): INTEGER =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtEventRaw.QTimerEvent_timerId(selfAdr);
  END QTimerEvent_timerId;

PROCEDURE Cleanup_QTimerEvent
  (<* UNUSED *> READONLY self: WeakRef.T; ref: REFANY) =
  VAR obj: QTimerEvent := ref;
  BEGIN
    Delete_QTimerEvent(obj);
  END Cleanup_QTimerEvent;

PROCEDURE Destroy_QTimerEvent (self: QTimerEvent) =
  BEGIN
    EVAL WeakRef.FromRef(self, Cleanup_QTimerEvent);
  END Destroy_QTimerEvent;

REVEAL
  QTimerEvent = QTimerEventPublic BRANDED OBJECT
                OVERRIDES
                  init_0     := New_QTimerEvent0;
                  timerId    := QTimerEvent_timerId;
                  destroyCxx := Destroy_QTimerEvent;
                END;

PROCEDURE New_QChildEvent0
  (self: QChildEvent; type: Type; child: REFANY (*QObject*); ):
  QChildEvent =
  VAR
    result : ADDRESS;
    arg2tmp          := LOOPHOLE(NARROW(child, QObject).cxxObj, ADDRESS);
  BEGIN
    result := QtEventRaw.New_QChildEvent0(ORD(type), arg2tmp);

    self.cxxObj := result;
    EVAL WeakRef.FromRef(self, Cleanup_QChildEvent);

    RETURN self;
  END New_QChildEvent0;

PROCEDURE Delete_QChildEvent (self: QChildEvent; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtEventRaw.Delete_QChildEvent(selfAdr);
  END Delete_QChildEvent;

PROCEDURE QChildEvent_child (self: QChildEvent; ): REFANY =
  VAR
    ret    : ADDRESS;
    result : QObject;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtEventRaw.QChildEvent_child(selfAdr);

    result := NEW(QObject);
    result.cxxObj := ret;
    result.destroyCxx();

    RETURN result;
  END QChildEvent_child;

PROCEDURE QChildEvent_added (self: QChildEvent; ): BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtEventRaw.QChildEvent_added(selfAdr);
  END QChildEvent_added;

PROCEDURE QChildEvent_polished (self: QChildEvent; ): BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtEventRaw.QChildEvent_polished(selfAdr);
  END QChildEvent_polished;

PROCEDURE QChildEvent_removed (self: QChildEvent; ): BOOLEAN =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtEventRaw.QChildEvent_removed(selfAdr);
  END QChildEvent_removed;

PROCEDURE Cleanup_QChildEvent
  (<* UNUSED *> READONLY self: WeakRef.T; ref: REFANY) =
  VAR obj: QChildEvent := ref;
  BEGIN
    Delete_QChildEvent(obj);
  END Cleanup_QChildEvent;

PROCEDURE Destroy_QChildEvent (self: QChildEvent) =
  BEGIN
    EVAL WeakRef.FromRef(self, Cleanup_QChildEvent);
  END Destroy_QChildEvent;

REVEAL
  QChildEvent = QChildEventPublic BRANDED OBJECT
                OVERRIDES
                  init_0     := New_QChildEvent0;
                  child      := QChildEvent_child;
                  added      := QChildEvent_added;
                  polished   := QChildEvent_polished;
                  removed    := QChildEvent_removed;
                  destroyCxx := Destroy_QChildEvent;
                END;

PROCEDURE New_QDynamicPropertyChangeEvent0
  (self: QDynamicPropertyChangeEvent; READONLY name: QByteArray; ):
  QDynamicPropertyChangeEvent =
  VAR result: ADDRESS;
  BEGIN
    result := QtEventRaw.New_QDynamicPropertyChangeEvent0(name);

    self.cxxObj := result;
    EVAL WeakRef.FromRef(self, Cleanup_QDynamicPropertyChangeEvent);

    RETURN self;
  END New_QDynamicPropertyChangeEvent0;

PROCEDURE Delete_QDynamicPropertyChangeEvent
  (self: QDynamicPropertyChangeEvent; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtEventRaw.Delete_QDynamicPropertyChangeEvent(selfAdr);
  END Delete_QDynamicPropertyChangeEvent;

PROCEDURE QDynamicPropertyChangeEvent_propertyName
  (self: QDynamicPropertyChangeEvent; ): QByteArray =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    RETURN QtEventRaw.QDynamicPropertyChangeEvent_propertyName(selfAdr);
  END QDynamicPropertyChangeEvent_propertyName;

PROCEDURE Cleanup_QDynamicPropertyChangeEvent
  (<* UNUSED *> READONLY self: WeakRef.T; ref: REFANY) =
  VAR obj: QDynamicPropertyChangeEvent := ref;
  BEGIN
    Delete_QDynamicPropertyChangeEvent(obj);
  END Cleanup_QDynamicPropertyChangeEvent;

PROCEDURE Destroy_QDynamicPropertyChangeEvent
  (self: QDynamicPropertyChangeEvent) =
  BEGIN
    EVAL WeakRef.FromRef(self, Cleanup_QDynamicPropertyChangeEvent);
  END Destroy_QDynamicPropertyChangeEvent;

REVEAL
  QDynamicPropertyChangeEvent =
    QDynamicPropertyChangeEventPublic BRANDED OBJECT
    OVERRIDES
      init_0       := New_QDynamicPropertyChangeEvent0;
      propertyName := QDynamicPropertyChangeEvent_propertyName;
      destroyCxx   := Destroy_QDynamicPropertyChangeEvent;
    END;


BEGIN
END QtEvent.
