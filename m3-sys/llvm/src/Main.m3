MODULE Main;

IMPORT AtomList, FileRd, FileWr, OSError; 
IMPORT IO, Process, Rd, Stdio, Text, Wr;
IMPORT Params; 

IMPORT M3CG, M3CG_Rd, M3CG_Wr, M3CG_BinRd, M3CG_BinWr, MxConfig, Target;
IMPORT M3CG_LLVM;

(*
  usage is problematic depending on program writing to file or stderr
  could be
  m3cgcat -binary < path/file.mc > somefile.mll
  m3llvm < somefile.mll > somefile.ll  with the bin reader 
  or m3llvm < somefile.mll > somefile.ll with the ascii reader 
*)

CONST DefaultBinOutFileName = "m3test.ll";
CONST DefaultExecutableName = "m3llvm"; 

VAR GExecutableName : TEXT := DefaultExecutableName; 
VAR GInFileName : TEXT := NIL;
VAR GBinOutFileName : TEXT := NIL;
VAR GCharOutFileName : TEXT := NIL;
VAR GInIsBinary : BOOLEAN := TRUE;
VAR GStdOutIsBinary : BOOLEAN := TRUE;
VAR GDoDisplayHelp : BOOLEAN := FALSE;
VAR GDoDisplayVersion : BOOLEAN := FALSE;
VAR GDebug : BOOLEAN := FALSE; 

PROCEDURE GetParams ( ) = 
  VAR LParamNo : CARDINAL;
  VAR LParam : TEXT;
  VAR LParamLen : CARDINAL;
  VAR LCharNo : CARDINAL;
  VAR LChar : CHAR; 

  PROCEDURE ConsumeParam ( ) = 
    BEGIN 
      INC(LParamNo);   
      IF LParamNo < Params.Count THEN 
        LParam := Params.Get ( LParamNo ); 
        LParamLen := Text.Length (LParam); 
      END (*IF*);
    END ConsumeParam; 

  PROCEDURE ParamWFileName (IsBin : BOOLEAN; VAR Name : TEXT ) =  
  (* Finishes with the current (and maybe following) param. *) 
    VAR LName : TEXT; 
    BEGIN
      INC (LCharNo);
      IF LCharNo < LParamLen THEN
      (* Remainder of LParam is the file name. *)
        LName := Text.Sub (LParam, LCharNo, LParamLen-LCharNo);
        Name := LName; 
        ConsumeParam ( ); 
      ELSE (* Look for a separate file name param. *) 
        ConsumeParam ( );
        IF LParamNo >= Params.Count THEN (* No more params, no file name. *)
          GStdOutIsBinary := IsBin;
        ELSIF Text.GetChar (LParam, 0) = '-' 
        THEN (* Next param is another option, not a file name. *)
          GStdOutIsBinary := IsBin;
        ELSE (* File name follows. *) 
          Name := LParam; 
          ConsumeParam ( ); 
        END; 
      END(*IF*); 
    END ParamWFileName;

  BEGIN
    GExecutableName := DefaultExecutableName; 
    GInFileName := NIL;
    GBinOutFileName := NIL;
    GCharOutFileName := NIL;
    GInIsBinary := TRUE;
    GStdOutIsBinary := TRUE;
    GDoDisplayHelp := FALSE;
    GDoDisplayVersion := FALSE;
    GDebug := FALSE;

    IF Params.Count > 0 THEN
      GExecutableName := Params.Get ( 0 ); 
    END (*IF*);  

    IF Params.Count > 1 THEN 
      LParamNo := 1;
      LParam := Params.Get ( LParamNo ); 
      LParamLen := Text.Length (LParam); 
      LOOP (* Thru' params. *) 
        IF Text.GetChar (LParam , 0) = '-'
        THEN (* It's an option or string thereof. *) 
          LCharNo := 1; 
          LOOP (* Thru' chars of this param. *)
            IF LCharNo >= LParamLen THEN 
              ConsumeParam ( );
              EXIT; 
            END;  
            LChar := Text.GetChar ( LParam, LCharNo); 
            CASE LChar 
            OF
              'a' => GInIsBinary := FALSE;
            | 'b' => GInIsBinary := TRUE;
            | 'd' => GDebug := TRUE;
            | 'o' => ParamWFileName (TRUE, GBinOutFileName);
                     EXIT;
            | 'O' => ParamWFileName (FALSE, GCharOutFileName);
                     EXIT; 
            | 'h' => GDoDisplayVersion := TRUE; 
                     GDoDisplayHelp := TRUE; 
            | 'v' => GDoDisplayVersion := TRUE;
            ELSE 
              IO.Put (GExecutableName);
              IO.Put ( ", invalid option character: \'" 
                         & Text.FromChar (LChar)
                         & "\'" 
                         & Wr.EOL);
              GDoDisplayHelp := TRUE; 
            END (*CASE*); 
            INC(LCharNo);
          END (*LOOP*); 
        ELSE
          GInFileName := LParam;
          ConsumeParam ( ); 
        END (*IF*); 
        IF LParamNo >= Params.Count THEN EXIT END;  
      END (*LOOP*); 
    END (*IF*); 
  END GetParams; 

CONST VersionString = "0.1";

PROCEDURE DisplayVersion ( ) = 
  BEGIN 
    IO.Put (GExecutableName);
    IO.Put (": standalone converter from cm3 IR to llvm IR, version "); 
    IO.Put (VersionString); 
    IO.Put (" "); 
    IO.Put (Wr.EOL); 
  END DisplayVersion; 

PROCEDURE DisplayHelp ( ) = 
  BEGIN 
    IO.Put ("Usage:"); 
    IO.Put (GExecutableName);
    IO.Put (" {-{option}} <inFileName>"); 
    IO.Put (Wr.EOL); 
    IO.Put ("  If <inFileName> is absent, read from standard input."); 
    IO.Put (Wr.EOL); 
    IO.Put ("  Options are:"); 
    IO.Put (Wr.EOL); 

    IO.Put ("  -a Treat input file as ascii cm3 IR."); 
    IO.Put (Wr.EOL); 

    IO.Put ("  -b Treat input file as binary cm3 IR (the default)."); 
    IO.Put (Wr.EOL); 

    IO.Put ("  -d Enable debug output."); 
    IO.Put (Wr.EOL); 

    IO.Put ("  -h Display help text and terminate."); 
    IO.Put (Wr.EOL); 

    IO.Put ("  -v Display program version and terminate."); 
    IO.Put (Wr.EOL); 

    IO.Put ("  -o<bitcodeFileName> or -o <bitcodeFileName>"); 
    IO.Put (Wr.EOL); 
    IO.Put ("     Write llvm bitcode to <bitcodeFileName>"); 
    IO.Put (Wr.EOL); 

    IO.Put ("  -O<asmFileName> or -O <asmFileName>"); 
    IO.Put (Wr.EOL); 
    IO.Put ("     Write llvm assembly code to <asmFileName>"); 
    IO.Put (Wr.EOL); 

    IO.Put ("  Either or both bitcode and assembly output may be specified."); 
    IO.Put (Wr.EOL); 
    IO.Put ("  If neither is specified, write llvm assembly to 'm3test.ll'"); 
    IO.Put (Wr.EOL); 

  END DisplayHelp; 

PROCEDURE WriteAtomList (List : AtomList.T ) = 
  BEGIN 
(*TODO: Fill this in. *) 
  END WriteAtomList; 

PROCEDURE WriteOpenFailure ( FileDescr, FileName : TEXT; Code : AtomList.T  ) = 
  BEGIN
    IO.Put (GExecutableName);
    IO.Put (": unable to open ");
    IO.Put (FileDescr);
    IO.Put (" \"");
    IO.Put (FileName); 
    IO.Put ("\" ("); 
    WriteAtomList (Code); 
    IO.Put (")."); 
    IO.Put (Wr.EOL);
  END WriteOpenFailure;

PROCEDURE DoIt() =
VAR 
  rd_in : Rd.T := NIL;
  wr_out : Wr.T := NIL; (* Not currently being used. *) 
  cg : M3CG_LLVM.U;
  BEGIN
    Init ();

    IF GInFileName = NIL OR Text . Equal ( GInFileName , "" ) THEN
      rd_in := Stdio.stdin; 
    ELSE 
      TRY
        rd_in := FileRd.Open (GInFileName); 
      EXCEPT OSError.E (Code (*AtomList.T*)) 
      => WriteOpenFailure ( "input file", GInFileName, Code); 
         RETURN; 
      END (*EXCEPT*); 
    END (*IF*);

    IF GBinOutFileName # NIL AND NOT Text . Equal ( GBinOutFileName , "" ) 
    THEN (* We have an output file name. *) 
    ELSIF GCharOutFileName # NIL AND NOT Text . Equal ( GCharOutFileName , "" ) 
    THEN (* We have an output file name. *)
    ELSE (* No output file of either kind.  Use a default bitcode name. *) 
      GBinOutFileName := DefaultBinOutFileName;
    END (*IF*);

    cg := M3CG_LLVM.New(wr_out);

    IF GInIsBinary THEN
      M3CG_BinRd.Inhale(rd_in, cg);
    ELSE
      M3CG_Rd.Inhale(rd_in, cg);
    END (*IF*); 

    cg.dumpLLVMIR(GBinOutFileName, GCharOutFileName);
  END DoIt;

PROCEDURE Init () =
  VAR machine: TEXT;
  BEGIN
    machine := MxConfig.Get ("TARGET");
    IF machine = NIL THEN
      IO.Put (GExecutableName);
      IO.Put (": unable to find TARGET definition in configuration file" & Wr.EOL);
      Process.Exit (1);
    ELSIF NOT Target.Init (machine) THEN
      IO.Put (GExecutableName);
      IO.Put (": unable to initialize Target: " & machine & Wr.EOL);
      Process.Exit (1);
    END;
  END Init;

BEGIN
  GetParams(); 
  IF GDoDisplayHelp THEN 
     DisplayVersion ( ); 
     DisplayHelp ( ); 
  ELSIF GDoDisplayVersion THEN DisplayVersion ( );
  ELSE 
    DoIt();
  END; 
END Main.
