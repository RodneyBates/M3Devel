(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 2.0.11
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

INTERFACE QtComboBox;

FROM QtIcon IMPORT QIcon;
FROM QtStringList IMPORT QStringList;
FROM QtSize IMPORT QSize;
FROM QtAbstractItemDelegate IMPORT QAbstractItemDelegate;
FROM QtAbstractItemModel IMPORT QAbstractItemModel, QModelIndex;
FROM QtEvent IMPORT QEvent;
FROM QGuiStubs IMPORT QValidator, QVariant, QCompleter;
FROM QtWidget IMPORT QWidget;
FROM QtLineEdit IMPORT QLineEdit;
FROM QtNamespace IMPORT CaseSensitivity, MatchFlags;
FROM QtAbstractItemView IMPORT QAbstractItemView;


TYPE T = QComboBox;


TYPE                             (* Enum InsertPolicy *)
  InsertPolicy =
    {NoInsert, InsertAtTop, InsertAtCurrent, InsertAtBottom,
     InsertAfterCurrent, InsertBeforeCurrent, InsertAlphabetically};

TYPE                             (* Enum SizeAdjustPolicy *)
  SizeAdjustPolicy =
    {AdjustToContents, AdjustToContentsOnFirstShow,
     AdjustToMinimumContentsLength, AdjustToMinimumContentsLengthWithIcon};

TYPE
  QComboBox <: QComboBoxPublic;
  QComboBoxPublic =
    QWidget BRANDED OBJECT
    METHODS
      init_0                           (parent: QWidget; ): QComboBox;
      init_1                           (): QComboBox;
      maxVisibleItems                  (): INTEGER;
      setMaxVisibleItems               (maxItems: INTEGER; );
      count                            (): INTEGER;
      setMaxCount                      (max: INTEGER; );
      maxCount                         (): INTEGER;
      autoCompletion                   (): BOOLEAN;
      setAutoCompletion                (enable: BOOLEAN; );
      autoCompletionCaseSensitivity    (): CaseSensitivity;
      setAutoCompletionCaseSensitivity (sensitivity: CaseSensitivity; );
      duplicatesEnabled                (): BOOLEAN;
      setDuplicatesEnabled             (enable: BOOLEAN; );
      setFrame                         (arg1: BOOLEAN; );
      hasFrame                         (): BOOLEAN;
      findText  (text: TEXT; flags: MatchFlags; ): INTEGER;
      findText1 (text: TEXT; ): INTEGER;
      findData (data: QVariant; role: INTEGER; flags: MatchFlags; ):
                INTEGER;
      findData1                (data: QVariant; role: INTEGER; ): INTEGER;
      findData2                (data: QVariant; ): INTEGER;
      insertPolicy             (): InsertPolicy;
      setInsertPolicy          (policy: InsertPolicy; );
      sizeAdjustPolicy         (): SizeAdjustPolicy;
      setSizeAdjustPolicy      (policy: SizeAdjustPolicy; );
      minimumContentsLength    (): INTEGER;
      setMinimumContentsLength (characters: INTEGER; );
      iconSize                 (): QSize;
      setIconSize              (size: QSize; );
      isEditable               (): BOOLEAN;
      setEditable              (editable: BOOLEAN; );
      setLineEdit              (edit: QLineEdit; );
      lineEdit                 (): QLineEdit;
      setValidator             (v: QValidator; );
      validator                (): QValidator;
      setCompleter             (c: QCompleter; );
      completer                (): QCompleter;
      itemDelegate             (): QAbstractItemDelegate;
      setItemDelegate          (delegate: QAbstractItemDelegate; );
      model                    (): QAbstractItemModel;
      setModel                 (model: QAbstractItemModel; );
      rootModelIndex           (): QModelIndex;
      setRootModelIndex        (index: QModelIndex; );
      modelColumn              (): INTEGER;
      setModelColumn           (visibleColumn: INTEGER; );
      currentIndex             (): INTEGER;
      currentText              (): TEXT;
      itemText                 (index: INTEGER; ): TEXT;
      itemIcon                 (index: INTEGER; ): QIcon;
      itemData                 (index, role: INTEGER; ): QVariant;
      itemData1                (index: INTEGER; ): QVariant;
      addItem                  (text: TEXT; userData: QVariant; );
      addItem1                 (text: TEXT; );
      addItem2    (icon: QIcon; text: TEXT; userData: QVariant; );
      addItem3    (icon: QIcon; text: TEXT; );
      addItems    (texts: QStringList; );
      insertItem  (index: INTEGER; text: TEXT; userData: QVariant; );
      insertItem1 (index: INTEGER; text: TEXT; );
      insertItem2 (index   : INTEGER;
                   icon    : QIcon;
                   text    : TEXT;
                   userData: QVariant; );
      insertItem3     (index: INTEGER; icon: QIcon; text: TEXT; );
      insertItems     (index: INTEGER; texts: QStringList; );
      insertSeparator (index: INTEGER; );
      removeItem      (index: INTEGER; );
      setItemText     (index: INTEGER; text: TEXT; );
      setItemIcon     (index: INTEGER; icon: QIcon; );
      setItemData     (index: INTEGER; value: QVariant; role: INTEGER; );
      setItemData1    (index: INTEGER; value: QVariant; );
      view            (): QAbstractItemView;
      setView         (itemView: QAbstractItemView; );
      sizeHint        (): QSize; (* virtual *)
      minimumSizeHint (): QSize; (* virtual *)
      showPopup       ();        (* virtual *)
      hidePopup       ();        (* virtual *)
      event           (event: QEvent; ): BOOLEAN; (* virtual *)
      clear           ();
      clearEditText   ();
      setEditText     (text: TEXT; );
      setCurrentIndex (index: INTEGER; );
      destroyCxx      ();
    END;


END QtComboBox.
