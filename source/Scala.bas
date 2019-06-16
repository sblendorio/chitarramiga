DECLARE SUB RSTRING (a$, b$, c$)
DECLARE SUB REP (a$, b!)
DECLARE SUB CENTER (s$, w!)
DECLARE SUB GETKEY (a$)
OPTION BASE 1
DIM SHARED x$(12)
n$ = "DO  DO# RE  RE# MI  FA  FA# SOL SOL#LA  LA# SI  "
r$ = CHR$(13)
FOR i = 1 TO 12
  x$(i) = MID$(n$, (i - 1) * 4 + 1, 4)
  RSTRING x$(i), " ", ""
NEXT
workfile$ = "RAM:MYBUFFER"
temp$ = "RAM:TEMPBUFF"
norm = 7
evidenz = 11
maxlines = 25
Start:
COLOR norm, 1: CLS
PRINT "Programma SCALA ACCORDI - Copyright (C) 1994, 1995 by Francesco Sblendorio.": PRINT   ' color (10 11 14 15),1 evidenzia
PRINT "1. Setta nomi files"
PRINT "2. Inserisce nuovi dati"
PRINT "3. Scala dati inseriti"
PRINT "4. Calcola un intervallo"
PRINT "5. Esce dal programma"
PRINT : PRINT "  Work File: "; : COLOR evidenz, 1: PRINT workfile$: COLOR norm, 1
PRINT "  Temp File: "; : COLOR evidenz, 1: PRINT temp$: COLOR norm, 1
PRINT : INPUT "> ", c$: c = VAL(c$)
ON c GOSUB SetName, InsData, ScaleData, CalcRange, ExitProgram
GOTO Start

SetName:
CLS
PRINT "SETTA NOMI FILES.": PRINT
LINE INPUT "Work File> ", fw$
LINE INPUT "Temp File> ", ft$
IF fw$ <> "" THEN workfile$ = UCASE$(fw$)
IF ft$ <> "" THEN temp$ = UCASE$(ft$)
RETURN

InsData:
CLS
PRINT "INSERISCE NUOVI DATI.": PRINT
PRINT "Scrivi X per terminare l'inserimento, EXIT per uscire"
OPEN temp$ FOR OUTPUT AS #1
xpos = 0: ypos = 0
DO
  IF CSRLIN > maxlines - 2 THEN ypos = 0: xpos = xpos + 17
  LOCATE 5 + ypos, 1 + xpos
  LINE INPUT "    : ", cm$
  RSTRING cm$, " ", ""
  cm$ = UCASE$(cm$)
  pv = INSTR(cm$, ",")
  IF pv = 0 THEN
    d$ = cm$
    e$ = ""
  ELSE
    d$ = LEFT$(cm$, pv - 1)
    e$ = MID$(cm$, pv + 1)
  END IF
  REP d$, i
  LOCATE 5 + ypos, 1 + xpos
  IF i >= 1 THEN
    PRINT i
    PRINT #1, i; r$; e$+" "
    ypos = ypos + 1
  ELSE
    PRINT SPACE$(76 - xpos);
    LOCATE 5 + ypos, 1 + xpos
  END IF
LOOP UNTIL d$ = "X" OR d$ = "EXIT"
CLOSE #1
IF d$ = "X" THEN
  OPEN temp$ FOR INPUT AS #1
  OPEN workfile$ FOR OUTPUT AS #2
  WHILE NOT EOF(1)
    LINE INPUT #1, l$
    PRINT #2, l$
  WEND
  CLOSE #1, #2
END IF
KILL temp$
RETURN

ScaleData:
IF NOT -1 THEN                     ' if not fexists(workfile$) then ...
  CLS
  LOCATE maxlines / 2, 1
  CENTER "Nessun dato inserito. Premi un tasto", 76
  GETKEY k$
ELSE
  DO
    CLS
    PRINT "SCALA DATI INSERITI.": PRINT
    PRINT "Accordi inseriti:"
    OPEN workfile$ FOR INPUT AS #1
    xpos = 0: ypos = 0
    z2 = 0: fst = -1
    WHILE NOT EOF(1)
      INPUT #1, i, e$
      IF fst THEN z1 = i: fst = 0
      IF CSRLIN > maxlines - 2 THEN ypos = 0: xpos = xpos + 9
      LOCATE 4 + ypos, 2 + xpos
      PRINT x$(i); e$: ypos = ypos + 1
    WEND
    CLOSE #1
    DO
      LOCATE 3, 52: PRINT SPACE$(21);
      LOCATE 3, 35
      LINE INPUT "Quanti semitoni? "; z$: z = VAL(z$)
      REP z$, z2
      IF z2 >= 1 THEN z = z2 - z1: IF z < 0 THEN z = 12 + z
      IF ABS(z) > 11 THEN
        LOCATE maxlines, 1
        PRINT "Bisogna inserire un numero compreso fra -11 e +11";
      END IF
    LOOP UNTIL ABS(z) < 12
    LOCATE maxlines, 1: PRINT SPACE$(75);
    LOCATE 4, 1
    OPEN workfile$ FOR INPUT AS #1
    xpos = 0: ypos = 0
    WHILE NOT EOF(1)
      INPUT #1, i, e$
      k = (i + z)
      IF k <= 0 THEN k = k + 12
      k = ((k - 1) MOD 12) + 1
      IF CSRLIN > maxlines - 2 THEN ypos = 0: xpos = xpos + 9
      LOCATE 4 + ypos, 36 + xpos
      PRINT x$(k); e$: ypos = ypos + 1
    WEND
    CLOSE #1
    LOCATE maxlines, 1
    PRINT "Vuoi scalare gli stessi accordi? (S/N)";
    GETKEY k$
    k$ = UCASE$(k$)
  LOOP UNTIL k$ <> "S" AND k$ <> "Y" AND k$ <> CHR$(13)
END IF
RETURN

CalcRange:
DO
  CLS
  PRINT "CALCOLA UN INTERVALLO.": PRINT
  DO
    LOCATE 3, 11: PRINT SPACE$(20);
    LOCATE 3, 1: LINE INPUT "Inserisci la 1^ nota> ", n1$
    REP n1$, n1
  LOOP UNTIL n1 >= 1
  DO
    LOCATE 4, 11: PRINT SPACE$(20);
    LOCATE 4, 1: LINE INPUT "Inserisci la 2^ nota> ", n2$
    REP n2$, n2
  LOOP UNTIL n2 >= 1
  r = n2 - n1
  IF r < 0 THEN r = 12 + r
  PRINT : COLOR evidenz, 1
  PRINT "Intervallo ="; r: COLOR norm, 1
  PRINT : PRINT "Un'altro calcolo? (S/N)"
  GETKEY k$
  k$ = UCASE$(k$)
LOOP UNTIL k$ <> "S" AND k$ <> "Y" AND k$ <> CHR$(13)
RETURN
  
ExitProgram:
SYSTEM

SUB CENTER (s$, w) STATIC
  cl = (w - LEN(s$)) / 2
  PRINT TAB(cl); s$
END SUB

SUB GETKEY (a$) STATIC
  DO
    a$ = INKEY$
  LOOP UNTIL a$ <> ""
END SUB

'------- SUBprograms ------------------
SUB REP (a$, b) STATIC
  SHARED n$
  p$ = UCASE$(a$)
  f = 0
  IF INSTR(p$, "B") THEN p$ = LEFT$(p$, LEN(p$) - 1): f = -1
  c$ = LEFT$(p$ + "    ", 4)
  IF NOT ((c$ = "DO  " OR c$ = "FA  ") AND f = -1) THEN
    b = INT((INSTR(n$, c$) + 3) / 4) + f
  ELSE
    b = 0
  END IF
END SUB

SUB RSTRING (a$, b$, c$) STATIC
  IF b$ <> c$ THEN
    lb = LEN(b$)
    cf = INSTR(a$, b$)
    DO
      IF cf <> 0 THEN a$ = LEFT$(a$, cf - 1) + c$ + MID$(a$, cf + lb)
      cf = INSTR(a$, b$)
    LOOP UNTIL cf = 0
  END IF
END SUB

