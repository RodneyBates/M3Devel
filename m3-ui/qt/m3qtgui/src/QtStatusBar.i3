(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

INTERFACE QtStatusBar;

FROM QtWidget IMPORT QWidget;


TYPE T = QStatusBar;


TYPE
  QStatusBar <: QStatusBarPublic;
  QStatusBarPublic =
    QWidget BRANDED OBJECT
    METHODS
      init_0     (parent: QWidget; ): QStatusBar;
      init_1     (): QStatusBar;
      addWidget  (widget: QWidget; stretch: INTEGER; );
      addWidget1 (widget: QWidget; );
      insertWidget (index: INTEGER; widget: QWidget; stretch: INTEGER; ):
                    INTEGER;
      insertWidget1       (index: INTEGER; widget: QWidget; ): INTEGER;
      addPermanentWidget  (widget: QWidget; stretch: INTEGER; );
      addPermanentWidget1 (widget: QWidget; );
      insertPermanentWidget (index  : INTEGER;
                             widget : QWidget;
                             stretch: INTEGER; ): INTEGER;
      insertPermanentWidget1 (index: INTEGER; widget: QWidget; ): INTEGER;
      removeWidget           (widget: QWidget; );
      setSizeGripEnabled     (arg1: BOOLEAN; );
      isSizeGripEnabled      (): BOOLEAN;
      currentMessage         (): TEXT;
      showMessage            (text: TEXT; timeout: INTEGER; );
      showMessage1           (text: TEXT; );
      clearMessage           ();
      destroyCxx             ();
    END;


END QtStatusBar.
