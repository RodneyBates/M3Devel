(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

INTERFACE QtAbstractButton;

FROM QtIcon IMPORT QIcon;
FROM QtSize IMPORT QSize;
FROM QGuiStubs IMPORT QButtonGroup;
FROM QtKeySequence IMPORT QKeySequence;


FROM QtWidget IMPORT QWidget;
TYPE T = QAbstractButton;


TYPE
  QAbstractButton <: QAbstractButtonPublic;
  QAbstractButtonPublic = QWidget BRANDED OBJECT
                          METHODS
                            setText               (text: TEXT; );
                            text                  (): TEXT;
                            setIcon               (icon: QIcon; );
                            icon                  (): QIcon;
                            iconSize              (): QSize;
                            setShortcut           (key: QKeySequence; );
                            shortcut              (): QKeySequence;
                            setCheckable          (arg1: BOOLEAN; );
                            isCheckable           (): BOOLEAN;
                            isChecked             (): BOOLEAN;
                            setDown               (arg1: BOOLEAN; );
                            isDown                (): BOOLEAN;
                            setAutoRepeat         (arg1: BOOLEAN; );
                            autoRepeat            (): BOOLEAN;
                            setAutoRepeatDelay    (arg1: INTEGER; );
                            autoRepeatDelay       (): INTEGER;
                            setAutoRepeatInterval (arg1: INTEGER; );
                            autoRepeatInterval    (): INTEGER;
                            setAutoExclusive      (arg1: BOOLEAN; );
                            autoExclusive         (): BOOLEAN;
                            group                 (): QButtonGroup;
                            setIconSize           (size: QSize; );
                            animateClick          (msec: INTEGER; );
                            animateClick1         ();
                            click                 ();
                            toggle                ();
                            setChecked            (arg1: BOOLEAN; );
                            destroyCxx            ();
                          END;


END QtAbstractButton.
