MODULE RegExpParse;
(* Generated by kyacc *)
IMPORT RegExpTok;
IMPORT IntIntTbl, IntTextTbl;
IMPORT RTType;
IMPORT Env, Thread, Wr, Fmt, Rd;
FROM Stdio IMPORT stdout;
FROM RegExpTok IMPORT NewPT;
<* FATAL Wr.Failure, Thread.Alerted *>

TYPE
  TypedSymbol = RECORD
    code: INTEGER;
    value: RegExpTok.ParseType;
  END;
CONST
  EOFSymbol = TypedSymbol{code := 0, value := NIL};
  NoToken = TypedSymbol{code := -1, value := NIL};
  NotASymbol = TypedSymbol{code := -1000, value := NIL};

TYPE
  StackElem = RECORD
    state: INTEGER;
    value: TypedSymbol;
  END;
  StackElemArray = REF ARRAY OF StackElem;

  Stack = RECORD
    a: StackElemArray;
    ptr: INTEGER;
  END;

REVEAL
  T = Public BRANDED "RegExpParse" OBJECT
    lex: RegExpTok.Lexer;
    tokenLookup: IntIntTbl.T := NIL; (* M3 type code -> SymCode *)
    symbols: IntTextTbl.T;           (* SymCode -> name *)
    allocate_expr: RegExpTok.Allocator;
  OVERRIDES
    setLex := SetLex;
    parse := Parse;
    purge := Purge;
    paren_expr := paren_expr;
    concat_expr := concat_expr;
    or_expr := or_expr;
    plus_expr := plus_expr;
    star_expr := star_expr;
    quest_expr := quest_expr;
    repeat_expr := repeat_expr;
    ident_expr := ident_expr;
    string_expr := string_expr;
    charRange_expr := charRange_expr;
  END;

TYPE
  SymCode = BITS 9 FOR [0..262];
  (* symbol code:  0 .. 261
     set default:  262 *)

  Action = BITS 6 FOR [0..32];
  (* error:        -1   (not stored in table)
     shift:        1 .. 10
     accept:       11
     reduce:       12 .. 21
     shift&accept: 22
     shift&reduce: 23 .. 32  *)

  StateRef = BITS 5 FOR [0..28];
  (* no more:      0
     next state:   1..28 *)

  S = RECORD
    key: SymCode;
    action: Action;
    next: StateRef;
  END;

  R = RECORD
    length: INTEGER;
    returnCode: INTEGER;
    name: TEXT;
  END;

  Y = RECORD
    code: INTEGER;
    name: TEXT;
  END;

CONST
  States = ARRAY [1..28] OF S {
    S{257,3,11}, S{257,8,11}, S{262,11,12}, S{257,9,11}, S{262,13,13},
    S{257,10,11}, S{262,13,14}, S{41,23,15}, S{262,14,16}, S{262,14,17},
    S{40,2,18}, S{124,6,17}, S{257,5,19}, S{257,7,19}, S{124,4,16},
    S{257,7,20}, S{257,5,20}, S{259,30,21}, S{42,27,22}, S{42,27,23},
    S{260,31,24}, S{43,26,25}, S{43,26,26}, S{261,32,0}, S{63,28,27},
    S{63,28,28}, S{258,29,0}, S{258,29,11}};

  Rules = ARRAY [12..21] OF R {
    R{3, 257, "paren_expr : '(' expr ')'"},
    R{2, 257, "concat_expr : expr expr"},
    R{3, 257, "or_expr : expr '|' expr"},
    R{2, 257, "plus_expr : expr '+'"},
    R{2, 257, "star_expr : expr '*'"},
    R{2, 257, "quest_expr : expr '?'"},
    R{2, 257, "repeat_expr : expr COUNT"},
    R{1, 257, "ident_expr : IDENTIFIER"},
    R{1, 257, "string_expr : STRING"},
    R{1, 257, "charRange_expr : CHAR_RANGE"}
};

  Symbols = ARRAY [1..18] OF Y {
    Y{0,"EOF"}, Y{10,"'\\n'"}, Y{40,"'('"}, Y{41,"')'"}, Y{42,"'*'"},
    Y{43,"'+'"}, Y{45,"'-'"}, Y{63,"'?'"}, Y{91,"'['"}, Y{93,"']'"},
    Y{94,"'^'"}, Y{124,"'|'"}, Y{256,"ERROR"}, Y{257,"expr"}, Y{258,"COUNT"},
    Y{259,"IDENTIFIER"}, Y{260,"STRING"}, Y{261,"CHAR_RANGE"}};

VAR
  Debug := Env.Get("RegExpParseDEBUG") # NIL;

PROCEDURE SetLex(self: T; lex: RegExpTok.Lexer): T =
  BEGIN self.lex := lex; RETURN self; END SetLex;

PROCEDURE Init(self: T) =
  BEGIN (* called on first parse *)
    self.tokenLookup := NEW(IntIntTbl.Default).init(18);
    IF Debug THEN
      self.symbols := NEW(IntTextTbl.Default).init(18);
      FOR i := 1 TO 18 DO
        EVAL self.symbols.put(Symbols[i].code, Symbols[i].name);
      END;
    END;
  END Init;

PROCEDURE NextToken(self: T): TypedSymbol =
  VAR
    symCode, m3code: INTEGER;
    token: RegExpTok.Token;
    found := FALSE;
  BEGIN
    TRY
      token := self.lex.get();
    EXCEPT
      Rd.EndOfFile => RETURN EOFSymbol;
    END;
    m3code := TYPECODE(token);
    IF NOT self.tokenLookup.get(m3code, symCode) THEN
      REPEAT
        m3code := RTType.Supertype(m3code);
        IF m3code = RTType.NoSuchType THEN
          TYPECASE token OF
          | ConstToken => symCode := -1;
          | COUNT => symCode := 258;
          | CHAR_RANGE => symCode := 261;
          | STRING => symCode := 260;
          | IDENTIFIER => symCode := 259;
          ELSE
            <* ASSERT FALSE *>
          END;
          found := TRUE;
        ELSE
          found := self.tokenLookup.get(m3code, symCode);
        END;
      UNTIL found;
      EVAL self.tokenLookup.put(TYPECODE(token), symCode);
    END;
    IF symCode = -1 THEN
      symCode := NARROW(token, ConstToken).val;
    END;
    RETURN TypedSymbol{code := symCode, value := token};
  END NextToken;

PROCEDURE AllocStack(): Stack =
  VAR
    a :=NEW(StackElemArray, 16);
  BEGIN
    a[0] := StackElem{state := 1, value := EOFSymbol};
    RETURN Stack{a := a, ptr := 0};
  END AllocStack;

PROCEDURE Push(VAR stack: Stack; elem: StackElem) =
  VAR
    new: StackElemArray;
  BEGIN
    INC(stack.ptr);
    IF stack.ptr > LAST(stack.a^) THEN
      new := NEW(StackElemArray, NUMBER(stack.a^) * 2);
      SUBARRAY(new^, 0, NUMBER(stack.a^)) := stack.a^;
      stack.a := new;
    END;
    stack.a[stack.ptr] := elem;
  END Push;

PROCEDURE ActionLookup(curState: INTEGER; symbol: TypedSymbol): INTEGER =
  VAR
    cur := curState;
    state: S;
    default := -1;
  BEGIN
    REPEAT
      state := States[cur];
      IF state.key = 262 THEN
        default := state.action;
      ELSIF state.key = symbol.code THEN
        RETURN state.action;
      END;
      cur := state.next;
    UNTIL cur = 0;
    RETURN default;
  END ActionLookup;

PROCEDURE Parse(self: T; exhaustInput: BOOLEAN := TRUE): StartType =
  VAR
    curState: INTEGER := 1;
    stack := AllocStack();
    action: INTEGER;
    symbol, preservedToken: TypedSymbol;
    skipTokenGets: INTEGER := 0;

  PROCEDURE DebugPrint(message: TEXT) = BEGIN
    IF Debug THEN Wr.PutText(stdout,"RegExpParseDEBUG: "&message&"\n");
    END;END DebugPrint;
  PROCEDURE DebugSymbol(message: TEXT) = VAR name: TEXT; BEGIN
   IF Debug THEN EVAL self.symbols.get(symbol.code, name);
    DebugPrint(message & " " & name & "(" &
      Fmt.Int(symbol.code) & ")"); END; END DebugSymbol;
  PROCEDURE DebugState(message: TEXT) = BEGIN IF Debug THEN
    DebugPrint(message & " " & Fmt.Int(curState));END;END DebugState;
  PROCEDURE DebugRule(message: TEXT) = BEGIN IF Debug THEN
    DebugPrint(message&" "&Rules[action].name);END;END DebugRule;

  BEGIN
    IF self.tokenLookup = NIL THEN Init(self); END;
    stack.a[0] := StackElem{state := curState, value := NotASymbol};
    DebugState("starting in state");
    LOOP
      IF skipTokenGets = 2 THEN
        skipTokenGets := 1;
        DebugSymbol("scanning reduced symbol");
      ELSIF skipTokenGets = 1 AND preservedToken # NoToken THEN
        skipTokenGets := 0;
        symbol := preservedToken;
        DebugSymbol("re-scanning input token");
      ELSE
        skipTokenGets := 0;
        symbol := NextToken(self);
        preservedToken := symbol;
        DebugSymbol("input token");
      END;
      action := ActionLookup(curState, symbol);
      IF action >= 22 THEN
        DebugPrint("shifting anonymously");
        Push(stack, StackElem{state := 0, value := symbol});
        DEC(action, 11);
        IF skipTokenGets = 0 THEN
          preservedToken := NoToken;
        END;
      END;
      IF action = -1 THEN
        DebugPrint("syntax error");
        self.lex.error("RegExpParse: syntax error");RETURN NIL;
      ELSIF action <= 10 THEN
        curState := action;
        DebugState("shifting to state");
        Push(stack, StackElem{state := curState, value := symbol});
      ELSIF action = 11 THEN
        DebugPrint("parsing stopped with singleton start symbol on stack");
        <* ASSERT stack.ptr = 1 *>
        IF exhaustInput AND preservedToken = NoToken THEN
          symbol := NextToken(self);
          DebugPrint("getting token to check that it's an EOF");
        END;
        IF symbol.code # 0 THEN
          IF exhaustInput THEN
            DebugPrint("Error: last token was not EOF");
            self.lex.error("RegExpParse: syntax error (parsing stopped before EOF)");
            RETURN NIL;
          END;
          IF preservedToken # NoToken THEN
            self.lex.unget();
            DebugPrint("ungetting last token");
          END;
        END;
        symbol := stack.a[1].value;
        DebugSymbol("returning symbol");
        RETURN symbol.value;
      ELSE
        DebugRule("reducing by rule");
        WITH p=stack.ptr, a=stack.a, v=symbol.value, l=Rules[action].length DO
          CASE action OF
          | 12 => VAR w: expr := NIL;
            p1:expr:=a[p-1].value.value;
            BEGIN self.paren_expr(w, p1); v:=w; END;
          | 13 => VAR w: expr := NIL;
            p1:expr:=a[p-1].value.value;p2:expr:=a[p].value.value;
            BEGIN self.concat_expr(w, p1, p2); v:=w; END;
          | 14 => VAR w: expr := NIL;
            p1:expr:=a[p-2].value.value;p2:expr:=a[p].value.value;
            BEGIN self.or_expr(w, p1, p2); v:=w; END;
          | 15 => VAR w: expr := NIL;
            p1:expr:=a[p-1].value.value;
            BEGIN self.plus_expr(w, p1); v:=w; END;
          | 16 => VAR w: expr := NIL;
            p1:expr:=a[p-1].value.value;
            BEGIN self.star_expr(w, p1); v:=w; END;
          | 17 => VAR w: expr := NIL;
            p1:expr:=a[p-1].value.value;
            BEGIN self.quest_expr(w, p1); v:=w; END;
          | 18 => VAR w: expr := NIL;
            p1:expr:=a[p-1].value.value;p2:COUNT:=a[p].value.value;
            BEGIN self.repeat_expr(w, p1, p2); v:=w; END;
          | 19 => VAR w: expr := NIL;
            p1:IDENTIFIER:=a[p].value.value;
            BEGIN self.ident_expr(w, p1); v:=w; END;
          | 20 => VAR w: expr := NIL;
            p1:STRING:=a[p].value.value;
            BEGIN self.string_expr(w, p1); v:=w; END;
          | 21 => VAR w: expr := NIL;
            p1:CHAR_RANGE:=a[p].value.value;
            BEGIN self.charRange_expr(w, p1); v:=w; END;
          ELSE
            <* ASSERT FALSE *>
          END;
          FOR i := p - l + 1 TO p DO a[i].value.value.discard(); END;
          DEC(p, l);
          curState := a[p].state;
        END;
        DebugState("popping to state");
        symbol.code := Rules[action].returnCode;
        skipTokenGets := 2;
      END;
    END;
  END Parse; 

PROCEDURE Purge(self: T): INTEGER =
  BEGIN
    RETURN 0
      + RegExpTok.Purge(self.allocate_expr);
  END Purge;

(* default methods *)
PROCEDURE paren_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: expr) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END paren_expr;

PROCEDURE concat_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: expr;<*UNUSED*>p2: expr) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END concat_expr;

PROCEDURE or_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: expr;<*UNUSED*>p2: expr) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END or_expr;

PROCEDURE plus_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: expr) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END plus_expr;

PROCEDURE star_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: expr) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END star_expr;

PROCEDURE quest_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: expr) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END quest_expr;

PROCEDURE repeat_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: expr;<*UNUSED*>p2: COUNT) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END repeat_expr;

PROCEDURE ident_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: IDENTIFIER) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END ident_expr;

PROCEDURE string_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: STRING) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END string_expr;

PROCEDURE charRange_expr(self: T;
 VAR result: expr;<*UNUSED*>p1: CHAR_RANGE) = BEGIN
 IF result=NIL THEN
   result:=NewPT(self.allocate_expr,TYPECODE(expr));
 END;END charRange_expr;

BEGIN
END RegExpParse.
