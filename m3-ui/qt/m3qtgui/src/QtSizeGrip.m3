(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

UNSAFE MODULE QtSizeGrip;


IMPORT QtSizeGripRaw;
FROM QtSize IMPORT QSize;
FROM QtWidget IMPORT QWidget;


IMPORT WeakRef;

PROCEDURE New_QSizeGrip0 (self: QSizeGrip; parent: QWidget; ): QSizeGrip =
  VAR
    result : ADDRESS;
    arg1tmp          := LOOPHOLE(parent.cxxObj, ADDRESS);
  BEGIN
    result := QtSizeGripRaw.New_QSizeGrip0(arg1tmp);

    self.cxxObj := result;
    EVAL WeakRef.FromRef(self, Cleanup_QSizeGrip);

    RETURN self;
  END New_QSizeGrip0;

PROCEDURE Delete_QSizeGrip (self: QSizeGrip; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtSizeGripRaw.Delete_QSizeGrip(selfAdr);
  END Delete_QSizeGrip;

PROCEDURE QSizeGrip_sizeHint (self: QSizeGrip; ): QSize =
  VAR
    ret    : ADDRESS;
    result : QSize;
    selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    ret := QtSizeGripRaw.QSizeGrip_sizeHint(selfAdr);

    result := NEW(QSize);
    result.cxxObj := ret;
    result.destroyCxx();

    RETURN result;
  END QSizeGrip_sizeHint;

PROCEDURE QSizeGrip_setVisible (self: QSizeGrip; arg2: BOOLEAN; ) =
  VAR selfAdr: ADDRESS := LOOPHOLE(self.cxxObj, ADDRESS);
  BEGIN
    QtSizeGripRaw.QSizeGrip_setVisible(selfAdr, arg2);
  END QSizeGrip_setVisible;

PROCEDURE Cleanup_QSizeGrip
  (<* UNUSED *> READONLY self: WeakRef.T; ref: REFANY) =
  VAR obj: QSizeGrip := ref;
  BEGIN
    Delete_QSizeGrip(obj);
  END Cleanup_QSizeGrip;

PROCEDURE Destroy_QSizeGrip (self: QSizeGrip) =
  BEGIN
    EVAL WeakRef.FromRef(self, Cleanup_QSizeGrip);
  END Destroy_QSizeGrip;

REVEAL
  QSizeGrip = QSizeGripPublic BRANDED OBJECT
              OVERRIDES
                init_0     := New_QSizeGrip0;
                sizeHint   := QSizeGrip_sizeHint;
                setVisible := QSizeGrip_setVisible;
                destroyCxx := Destroy_QSizeGrip;
              END;


BEGIN
END QtSizeGrip.
