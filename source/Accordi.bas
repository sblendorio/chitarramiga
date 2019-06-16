LOCATE 1,1:DrawCursor 1
ON ERROR GOTO trouble
CONST x.=0                      ' Costanti per le procedure
CONST y.=1                      ' di scrolling (SMOOTH e SPLIT)
cd$=COMMAND$                    ' cd$      = Path per i file accordi nella CLI-Line
fh=0                            ' fh       = Flag di caricamento dati (0=SI)
file1$="FACCORDI.TXT"           ' file1$   = File contenente i vari accordi
file2$="FGIRI.TXT"              ' file2$   = File contenente i giri armonici
IF FEXISTS(cd$+file1$) THEN
   OPEN cd$+file1$ FOR INPUT AS #1
   INPUT #1,nton                ' nton     = Numero tonalita' per accordo
   CLOSE #1
ELSE
   fh=-1
END IF
FOR cds=32 TO 126
   car$=car$+CHR$(cds)          ' car$     = Caratteri Stampabili per l'input
NEXT cds
back$=CHR$(8)+CHR$(29)+CHR$(31) ' back$    = Caratteri di ritorno indietro
DIM SHARED c(6)                 ' c()      = Frequenze delle corde
DIM SHARED n(6)                 ' n()      = Note delle corde
DIM SHARED p(6)                 ' p()      = Array per l'uso di Acc()
DIM SHARED ak$((nton*12)+1,3)   ' ak$(x,y) = Accordi in tutte le tonalita'
DIM SHARED gr$(13,5)            ' gr$(x,y) = Giri armonici
DIM SHARED sc$(12,1)            ' sc$(x,y) = Scale
SetVars                         ' - Setta le variabili principali
Assegna                         ' - Legge i dati dal disco
SCREEN 1,640,256,2,2
WINDOW 2,"",,128+16,1
PALETTE 0,0,0,0
PALETTE 1,1,1,1
PALETTE 2,.33,.87,0
PALETTE 3,.98,.2,0
COLOR 1,0
inizio:
CLS
LOCATE 6,1:COLOR 2,0
Center "***************************************",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***                                 ***",0,80
Center "***************************************",0,80

LOCATE 7,1:COLOR 1,0
AuUp$=UCASE$(zx$)
Center "Gli Accordi a Prima Vista",0,80
Center "metodo pratico di apprendimento",0,80:COLOR 3,0
Center "-------------------------------",0,80:COLOR 1,0
Center "Tratto dall'omonimo libro di",0,80
Center AuUp$,0,80
Center "Trasportato su computer da",0,80
Center UCASE$(l$),0,80:COLOR 3,0
Center "-------------------------------",0,80:COLOR 1,0
Center "Sezione Chitarra",0,80

LOCATE 18,1:COLOR 1,0
Center "Premi un tasto per continuare...",0,80
DrawCursor 1
GetKey a$
Split x.,0,136,639,143,4,.006
Delay .15
Split x.,0,72,639,111,4,.006
Smooth y.,0,72,639,135,-1,.02,39
Delay .25
Smooth y.,0,0,639,87,-1,.0115,39
DO
   ScnClr
   Restart:
   LOCATE 10,1
   Text "%10   MAIN MENU...":PRINT:PRINT
   Text "%02 1 %30  Parte Prima:%10 Giri armonici in tutti i toni":PRINT
   Text "%02 2 %30  Parte Seconda:%10 Accordi singoli":PRINT
   Text "%02 3 %10  Procedura per accordatura chitarra":PRINT
   Text "%02 4 %10  Setta il path per i files-accordi":PRINT
   Text "%02 5 %10  Ricarica i dati dal disco":PRINT
   Text "%02 6 %10  Informazioni sul programma...":PRINT
   Text "%02 7 %10  Fine Programma%20":PRINT:PRINT
   Choose ch,7
   IF fh AND (ch=1 OR ch=2) THEN
      LOCATE 8,1:COLOR 2,0
      Center "\Dati non caricati. Prova a cambiare il path e a ricaricare i dati.",0,80
      DrawCursor 2
      LOCATE 28,3:PRINT " ";
      GetKey k$
      LOCATE 8,1:PRINT SPACE$(79);
      LOCATE 28,1:PRINT ">";
      DrawCursor 2
      GOTO Restart
   END IF
   Fade 0,72,639,199,8
   IF ch=-99 THEN GOSUB PSelfFunct
   ON ch GOSUB PGiri,PAccs,PSync,PChDir,PRestore,PInfos
   IF ch=7 THEN
      ScnClr
      LOCATE 15,1
      COLOR 1,0:Center "\Sei Sicuro? (S/N)",0,80
      DrawCursor 1
      GetKey k$:k$=UCASE$(k$)
      IF INSTR("SYJ",k$) THEN EXIT LOOP
   END IF
LOOP
d$=""
car$=""
CLEAR
ERASE c,n,p,ak$,gr$,sc$
WINDOW CLOSE 2
SCREEN CLOSE 1
SYSTEM

PGiri:
   DO
      f1=0
      gs=1
      ScnClr
      LOCATE 10,1:COLOR 1,0:PRINT "   Giri armonici in tutti i toni"
      LOCATE 13,1
      Text "%02 1 %30  Visualizzazione di%10 tutti i giri":PRINT
      Text "%02 2 %30  Visualizzazione di%10 un giro, a scelta":PRINT
      Text "%02 3 %10  Ritorno al Main Menu%20":PRINT:PRINT
      Choose g,3
      Fade 0,72,639,135,8
      ON g GOSUB PPTuttiGiri,PPUnGiro
   LOOP UNTIL g=3
RETURN

PPTuttiGiri:
   FOR v=gs TO 12
      CLS:PRINT TAB(77);v
      z=0
      IF f1 THEN 
         IF fd THEN z=5
      END IF
      IF gr$(v,z)="'" THEN z=0
      COLOR 1,0
      Center "Giro di <"+gr$(v,z)+">",0,80
      Stampa 000,029,gr$(v,1)
      Stampa 320,029,gr$(v,2)
      Stampa 000,104,gr$(v,3)
      Stampa 320,104,gr$(v,4)
      LOCATE 25,1:Center "\--- Premi un tasto per continuare, ESC per uscire ---",0,80
      DrawCursor 1
      DO
         qk%=0
         GetKey k$
         IF INSTR(back$,k$) AND v=1 THEN qk%=1
         IF INSTR(back$,k$) AND v>1 THEN v=v-2:qk%=0
         IF k$=CHR$(27) THEN EXIT LOOP
      LOOP UNTIL qk%=0
      IF k$=CHR$(27) THEN EXIT FOR
   NEXT v
   IF NOT f1 THEN CLS:Testata
RETURN

PPUnGiro:
   f1=-1
   DO
      Req1:
      ScnClr
      COLOR 1,0:PRINT "   Inserisci il giro (Scrivi EXIT per terminare):":PRINT:PRINT
      gi$=" > "
      Pinput 0,gi$,car$,1
      gi$=UCASE$(gi$)
      SubRep gi$," ",""
      SubRep gi$,"&","B"
      SubRep ac$,"0","O"
      IF gi$="EXIT" THEN EXIT LOOP
      k=0
      DO
         INCR k
      LOOP UNTIL k>12 OR (gi$=UCASE$(gr$(k,0)) OR gi$=UCASE$(gr$(k,5)))
      IF k>12 THEN
         LOCATE 17,1:COLOR 2,0
         Center "\Giro armonico inesistente -- Premi un tasto",0,80:DrawCursor 1
         GetKey k$
         GOTO Req1
      END IF
      IF gi$=UCASE$(gr$(k,0)) THEN gp$=gr$(k,0)
      IF gi$=UCASE$(gr$(k,5)) THEN gp$=gr$(k,5)
      gs=k:fd=INSTR(gi$,"#")
      GOSUB PPTuttiGiri
      CLS:Testata
   LOOP
RETURN

PAccs:
   DO
      f2=0
      ks=1
      ScnClr
      LOCATE 10,1:COLOR 1,0:PRINT "   Accordi in tutte le tonalita'"
      LOCATE 13,1
      Text "%02 1 %30  Visualizzazione di%10 tutti gli accordi in ogni tonalita''":PRINT
      Text "%02 2 %30  Visualizzazione di%10 un accordo in tutte le tonalita''":PRINT
      Text "%02 3 %30  Visualizzazione di%10 un accordo in una sola tonalita''":PRINT
      Text "%02 4 %10  Ritorno al Main Menu%20":PRINT:PRINT
      Choose g,4
      Fade 0,72,639,151,8
      ON g GOSUB PPTuttiAccs,PPUnaPage,PPUnAcc
   LOOP UNTIL g=4
RETURN

PPTuttiAccs:
   i=ks-1:s=0
   DO
      INCR i
      z=0
      IF f2 THEN
         IF fd THEN z=3
      END IF
      j=i-1:sr$=ak$(j*nton+1,z)
      IF sr$="'" THEN sr$=ak$(j*nton+1,0)
      ps=INSTR(sr$," ")
      akt$=LEFT$(sr$,ps-1)
      FOR p=s TO v1
         n=j*nton+p*6
         CLS:PRINT TAB(77);i*2+p-1;
         COLOR 1,0
         Center "<"+akt$+"> /"+STR$(p+1),0,80
         z2=z:IF ak$(n+1,z2)="'" THEN z2=3-z2
         IF NOT (p=v1 AND vf=0) THEN
            IF (p<v1) OR ((p=v1) AND (vf>=1)) THEN Acc 000,022,ak$(n+1,1),ak$(n+1,2),ak$(n+1,z2)
            IF (p<v1) OR ((p=v1) AND (vf>=2)) THEN Acc 320,022,ak$(n+2,1),ak$(n+2,2),ak$(n+2,z2)
            IF (p<v1) OR ((p=v1) AND (vf>=3)) THEN Acc 000,097,ak$(n+3,1),ak$(n+3,2),ak$(n+3,z2)
            IF (p<v1) OR ((p=v1) AND (vf>=4)) THEN Acc 320,097,ak$(n+4,1),ak$(n+4,2),ak$(n+4,z2)
            IF (p<v1) OR ((p=v1) AND (vf>=5)) THEN Acc 000,172,ak$(n+5,1),ak$(n+5,2),ak$(n+5,z2)
            IF (p<v1) OR ((p=v1) AND (vf>=6)) THEN Acc 320,172,ak$(n+6,1),ak$(n+6,2),ak$(n+6,z2)
            msg$="--- Premi un tasto per l'altra pagina, ESC per uscire ---"
            IF p=1 THEN msg$="--- Premi un tasto per un altro accordo, ESC per uscire ---"
            LOCATE 32,1:Center "\"+msg$,0,80
            DrawCursor 1
            DO
               GetKey k$
               IF NOT INSTR(back$,k$)<>0 THEN EXIT LOOP
            LOOP UNTIL NOT (p=0 AND i=1)
            IF k$=CHR$(27) THEN EXIT FOR
            IF INSTR(back$,k$) THEN
               IF p=0 AND i>1 THEN s=1:i=i-2:EXIT FOR
               IF p>0 THEN p=p-2
            END IF
         END IF
         s=0
      NEXT p
      IF k$=CHR$(27) THEN EXIT LOOP
   LOOP UNTIL i>=12
   IF NOT f2 THEN CLS:Testata
RETURN

PPUnaPage:
   f2=-1
   DO
      Req2:
      ScnClr
      COLOR 1,0:PRINT "   Inserisci la nota fondamentale (Scrivi EXIT per terminare):":PRINT:PRINT
      ar$=" > "
      Pinput 0,ar$,car$,1
      ar$=UCASE$(ar$)
      SubRep ar$," ",""
      SubRep ar$,"&","B"
      SubRep ac$,"0","O"
      IF ar$="EXIT" THEN EXIT LOOP
      i=0
      DO
         INCR i
         x$=ak$((i-1)*nton+1,0)
         y$=ak$((i-1)*nton+1,3)
         ps1=INSTR(x$," ")-1
         ps2=INSTR(y$," ")-1
         x$=LEFT$(x$,ABS(ps1))
         y$=LEFT$(y$,ABS(ps2))
      LOOP UNTIL (ar$=UCASE$(x$) OR ar$=UCASE$(y$)) OR i>12
      akt$=x$:IF ar$=UCASE$(y$) THEN akt$=y$
      IF i>12 THEN
         LOCATE 17,1:COLOR 2,0
         Center "\Nota inesistente -- Premi un tasto",0,80:DrawCursor 1
         GetKey k$
         GOTO Req2
      END IF
      ks=i:fd=INSTR(akt$,"#")
      GOSUB PPTuttiAccs
      CLS:Testata
   LOOP
RETURN

PPUnAcc:
   DO
      ScnClr
      COLOR 1,0:PRINT "   Inserisci l'accordo (Scrivi EXIT per terminare):":PRINT:PRINT
      ac$=" > "
      Pinput 0,ac$,car$,1
      ac$=UCASE$(ac$)
      IF ac$="EXIT" THEN EXIT LOOP
      Correct ac$
      Stampa 180,106,ac$
      mY=25:msg$="--- Premi un tasto per un altro accordo, ESC per uscire ---"
      IF ac$="ERR" THEN mY=17:msg$="Accordo non memorizzato oppure errato -- Premi un tasto o ESC per uscire"
      LOCATE mY,1:COLOR 2,0:Center "\"+msg$,0,80:DrawCursor 1
      GetKey k$
   LOOP UNTIL k$=CHR$(27)
RETURN

PInfos:
   InfAU$=LEFT$(l$,1)+"."+RIGHT$(l$,11)
   InfAT$=LEFT$(zx$,3)+" "+RIGHT$(zx$,5)
   LOCATE 3,1:COLOR 3,0:Center "Programmato  da",0,20
   LOCATE 4,1:COLOR 1,0:Center InfAU$,0,20
   LOCATE 3,1:COLOR 3,0:Center "Tratto dal libro di",60,20
   LOCATE 4,1:COLOR 1,0:Center zx$,60,20
   ScnClr
   COLOR 2,0
   LOCATE 17,1:Center "https://github.com/sblendorio/chitarramiga",0,80
   DO
      FOR alfa=0 TO 6.2831853 STEP .024933275
         x1=150*COS(alfa)*2
         x2=150*SIN(alfa)*2
         PSET (320+x1,126),1
         PSET (320+x2,136),1
         Delay .005
         PRESET (320+x1,126)
         PRESET (320+x2,136)
         k$=INKEY$
         IF k$<>"" THEN EXIT FOR
      NEXT
      IF k$<>"" THEN EXIT LOOP
   LOOP
   LOCATE 3,1:PRINT SPACE$(20)
   LOCATE 4,1:PRINT SPACE$(20)
   LOCATE 3,60:PRINT SPACE$(20)
   LOCATE 4,60:PRINT SPACE$(20)
RETURN

PSync:
   ScnClr
   Acc 305,108,"000000","000000",""
   LOCATE 10,1:COLOR 1,0
   PRINT "   Accordatura chitarra: premi ESC per uscire"
   LOCATE 13,1
   Text "%02 1 %30  Per la 1^ corda %10(MI cantino)":PRINT
   Text "%02 2 %30  Per la 2^ corda %10(SI)":PRINT
   Text "%02 3 %30  Per la 3^ corda %10(SOL)":PRINT
   Text "%02 4 %30  Per la 4^ corda %10(RE)":PRINT
   Text "%02 5 %30  Per la 5^ corda %10(LA)":PRINT
   Text "%02 6 %30  Per la 6^ corda %10(MI basso)":PRINT:PRINT
   COLOR 2,0:PRINT ">";
   DrawCursor 2
   DO
      GetKey k$:k=VAL(k$)
      IF k>=1 AND k<=6 THEN
         LOCATE ,2:COLOR 1,0
         PRINT MID$(STR$(k),2);
         DrawCursor 2
         SOUND c(k),12,255,0
      END IF
   LOOP UNTIL k$=CHR$(27)
   Fade 0,72,639,183,8
RETURN

PChDir:
   vis$=cd$:IF RIGHT$(cd$,1)="/" THEN vis$=LEFT$(cd$,LEN(cd$)-1)
   IF vis$="" THEN vis$="Current Dir"
   ScnClr
   COLOR 2,0:PRINT "   Path attuale ";:COLOR 1,0:PRINT "<"+vis$+">";:COLOR 2,0:PRINT " -- Inser. nuovo (EXIT per no-modifiche):":PRINT:PRINT
   COLOR 1,0
   ts$=" > "
   Pinput 1,ts$,car$,1
   vd%=FEXISTS(ts$)
   IF UCASE$(ts$)<>"EXIT" THEN
      IF RIGHT$(cd$,1)<>":" AND RIGHT$(cd$,1)<>"/" THEN ts$=ts$+"/"
      IF vd% THEN
         cd$=ts$
      ELSE
         LOCATE 17,1:COLOR 2,0
         Center "\Path inesistente -- Premi un tasto per continuare",0,80:DrawCursor 1
         GetKey k$
         GOTO PChDir
      END IF
   END IF
RETURN

PRestore:
   fh=0
   IF FEXISTS(cd$+file1$) THEN
      OPEN cd$+file1$ FOR INPUT AS #1
      INPUT #1,nton
      ERASE ak$
      DIM SHARED ak$((nton*12)+1,3)
      CLOSE #1
   ELSE
      fh=-1
   END IF
   WINDOW 1
   SetVars
   Assegna
   WINDOW 2
RETURN

PSelfFunct:
   DO
      ScnClr
      COLOR 1,0:PRINT "   Inserisci le dita e le posizioni separate dalla virgola (EXIT esce):":PRINT:PRINT
      sv$=" > "
      Pinput 0,sv$,car$,1
      IF sv$="EXIT" THEN EXIT LOOP
      v=INSTR(sv$,",")
      IF v=0 THEN v=LEN(sv$)
      di$=LEFT$(sv$,v-1)
      po$=MID$(sv$,v+1)
      ACC 180,106,di$,po$,"Prova"
      LOCATE mY,1:COLOR 2,0:Center "\--- Premi un tasto per continuare, ESC per uscire ---",0,80:DrawCursor 1
      GetKey k$
   LOOP UNTIL k$=CHR$(27)
RETURN

trouble:
   CLS
   COLOR 1,0
   PRINT "Internal error no.";ERR
   PRINT
   PRINT "Press any key";
   DrawCursor 1
   GetKey k$
   IF k$=">" THEN ERROR ERR
   SYSTEM

' --- SUBprograms ---

SUB Fade(x1,y1,x2,y2,v)
  '      x1,y1,x2,y2 = Estremi
  '      v           = Pixel per volta
  FOR p=0 TO v-1
    FOR i=p TO ABS(x2-x1) STEP v
      LINE (x1+i,y1)-(x1+i,y2),0
    NEXT i
  NEXT p
END SUB

SUB ScnClr STATIC
   LINE (0,64)-(639,255),0,bf
   LOCATE 10,1
END SUB

SUB GetKey(a$) STATIC
   '     a$ = Tasto premuto
   DO
      a$=INKEY$
   LOOP UNTIL a$<>""
END SUB

SUB Center(x$,s,w) STATIC
   '     x$ = Stringa da stampare (Se il 1^ Car. e' "\" non andra' a capo)
   '     s  = Pos. iniziale
   '     w  = Larghezza massima
   f=0
   IF LEFT$(x$,1)="\" THEN x$=RIGHT$(x$,LEN(x$)-1):f=1
   PRINT TAB(s+(w-LEN(x$))/2);x$;
   IF f=0 THEN PRINT
END SUB

SUB Delay(n) STATIC
   '     n = N.secs
   t.bef=TIMER
   WHILE TIMER < t.bef+n
   WEND
END SUB

SUB Text(x$) STATIC
   '         x$ = Stringa da stampare con caratteri di controllo
   FOR c=1 TO LEN(x$)
      j$=MID$(x$,c,1)
      IF j$="%" THEN j$="":INCR c:f=VAL(MID$(x$,c,1)):INCR c:b=VAL(MID$(x$,c,1)):COLOR f,b
      IF j$="'" THEN c=c+1:j$=MID$(x$,c,1)
      PRINT j$;
   NEXT
   PRINT
END SUB

SUB DrawCursor(b) STATIC
   COLOR 0,3
   PRINT " ";
   COLOR b,0
END SUB

SUB Pinput (f%,x$,y$,z%) STATIC
  PRINT x$;:x$="":COLOR 2,3:PRINT " ";:LOCATE CSRLIN,POS(0)-1:COLOR 1,0 
  IF z%<>1 THEN z%=0
Pinputl1:
  k$=INKEY$
  IF k$="" GOTO Pinputl1
  IF k$=CHR$(8) THEN
    IF x$="" THEN k$="" ELSE x$=LEFT$(x$,LEN(x$)-1):PRINT " "CHR$(8);
  ELSE
    IF k$=CHR$(13) THEN
       IF (f%<>0 AND x$="") OR x$<>"" THEN
          PRINT " "
          IF x$="'" AND f%=0 THEN x$="//"
          EXIT SUB
       END IF
    END IF
    IF k$<>CHR$(27) THEN 
      IF INSTR(y$,k$)<>0 THEN i%=1 ELSE i%=0
      IF i%<>z% THEN GOTO Pinputl1
    END IF
    IF k$=CHR$(27) THEN k$="EXIT"
    x$=x$+k$
  END IF
  PRINT k$;:COLOR 2,3:PRINT " ";:LOCATE CSRLIN,POS(0)-1:COLOR 1,0
  IF LEN(x$)<255 GOTO Pinputl1
  BEEP:PRINT
END SUB

SUB Smooth(t,x1,y1,x2,y2,stp,del,nlines) STATIC
   '     t           = Tipo di scrolling (0=Oriz. -- 1=Vert.)
   '     x1,y1,x2,y2 = Estremi
   '     stp         = Pixel per volta
   '     del         = Delay (in secondi)
   '     nlines      = N. di cicli di scrolling (0=Tutti)
   IF t= 0 AND nlines=0 THEN nlines=ABS(x2-x1)
   IF t<>0 AND nlines=0 THEN nlines=ABS(y2-y1)
   dx=stp:dy=0
   IF t<>0 THEN dx=0:dy=stp
   FOR i=0 TO ABS(nlines/stp)
      SCROLL (x1,y1)-(x2,y2),dx,dy
      Delay del
   NEXT i
END SUB

SUB Split(t,x1,y1,x2,y2,stp,del) STATIC
   '    t           = Tipo  di scrolling (0=Oriz. -- 1=Vert.)
   '    x1,y1,x2,y2 = Estremi
   '    stp         = Pixel per volta
   '    del         = Delay (in secondi)
   xM=(x1+x2)/2
   yM=(y1+y2)/2
   IF t=0 THEN
      nrows=ABS(x2-x1)/2
      dx=stp:dy=0
      c=xM:d=y2
      e=xM+1:f=y1
   ELSE
      nrows=ABS(y2-y1)/2
      dx=0:dy=stp
      c=x2:d=yM
      e=x1:f=yM+1
   END IF
   FOR i=0 TO nrows+3 STEP ABS(stp)
      SCROLL (x1,y1)-(c,d),-dx,-dy
      SCROLL (e,f)-(x2,y2),dx,dy
      Delay del
   NEXT i
END SUB

SUB Choose(ch,t) STATIC
   '            ch = Variabile destinazione
   '            t  = Margine superiore
   COLOR 2,0
   PRINT ">";
   DrawCursor 2
   m=MOUSE(0)
   ch=0
   DO
      m=MOUSE(0):Xm=MOUSE(1):Ym=MOUSE(2)
      IF m AND (Xm>=0 AND Xm<=24) THEN
         FOR i=1 TO t
            y1=80+(i*16):y2=y1+7
            IF Ym>=y1 AND Ym<=y2 THEN
               ch=i
            END IF
         NEXT i         
      END IF
      ch$=INKEY$
      IF ch$<>"" THEN ch=VAL(ch$)
      IF ch$=">" THEN ch=-99
      IF ch$=CHR$(27) OR ch$="0" THEN ch=t
   LOOP UNTIL (ch>=1 AND ch<=t) OR ch=-99
   IF ch<>-99 THEN
      ch$=MID$(STR$(ch),2)
      COLOR 1,0:PRINT CHR$(8)ch$;:DrawCursor 1
      Py=CSRLIN:Px=POS(0)
      LOCATE 11+(ch*2),1:COLOR 2,0:PRINT " ";ch$;" "
      Delay .3
      LOCATE 11+(ch*2),1:COLOR 0,2:PRINT " ";ch$;" "
      LOCATE Py,Px:COLOR 1,0
   END IF
END SUB

SUB Assegna STATIC
   ' -- Legge gli accordi e i giri armonici da disco --
   CLS
   fh=0
   SHARED fh,cd$,file1$,file2$,d$,vf,v1
   LOCATE 12,1:Center "\Attendere prego, sto caricando i dati dal disco...",0,80
   DrawCursor 1
   IF FEXISTS(cd$+file1$) THEN
      OPEN cd$+file1$ FOR INPUT AS #1
      INPUT #1,nton
      vf=nton MOD 6
      v1=INT(nton/6)
      FOR i=0 TO 11
         FOR j=1 TO nton
            n=i*nton+j
            INPUT #1,ak$(n,3),ak$(n,0),ak$(n,1),ak$(n,2)
         NEXT j
      NEXT i
      CLOSE #1
   ELSE
      fh=-1
   END IF
   IF FEXISTS(cd$+file2$) THEN
      OPEN cd$+file2$ FOR INPUT AS #1
      FOR i=1 TO 12
         INPUT #1,gr$(i,5),gr$(i,0),gr$(i,1),gr$(i,2),gr$(i,3),gr$(i,4)
      NEXT i
      CLOSE #1
   ELSE
      fh=-1
   END IF
   CLS
END SUB

SUB SetVars STATIC
   SHARED l$,d$,zx$
   d$=""
   RESTORE
   FOR i=1 TO 6
      READ c(i)
   NEXT i
   FOR i=1 TO 6
      READ n(i)
   NEXT i
   FOR i=0 TO 1
      FOR j=1 TO 12
         READ x$
         sc$(j,i)=LCASE$(x$)
      NEXT j
   NEXT i
   l$="Francesco Sblendorio"
   zx$="M. Vetta"
   DATA 1315.77,989.74,782.10,588.63,440.32,329.91
   DATA 5,12,8,3,10,5
   DATA DO,DO#,RE,RE#,MI,FA,FA#,SOL,SOL#,LA,LA#,SI
   DATA DO,REb,RE,MIb,MI,FA,SOLb,SOL,LAb,LA,SIb,SI
END SUB

SUB Testata STATIC
   LOCATE 1,1:COLOR 2,0
   Center "***************************************",0,80
   Center "***                                 ***",0,80
   Center "***                                 ***",0,80
   Center "***                                 ***",0,80
   Center "***                                 ***",0,80
   Center "***************************************",0,80
   LOCATE 1,1:COLOR 1,0
   PRINT
   Center "Gli Accordi a Prima Vista",0,80
   Center "metodo pratico di apprendimento",0,80:COLOR 3,0
   Center "-------------------------------",0,80:COLOR 1,0
   Center "Sezione Chitarra",0,80:COLOR 1,0
END SUB

SUB SubRep(a$,b$,c$) STATIC
   '            a$  = Stringa Principale  
   '            b$  = Stringa da sostituire con c$  
   '            c$  = Stringa da sostituire a b$  
   IF b$<>c$ THEN
      lb=LEN(b$)
      cf=INSTR(a$,b$)
      DO
         IF cf<>0 THEN a$=LEFT$(a$,cf-1)+c$+MID$(a$,cf+lb)
         cf=INSTR(a$,b$)
      LOOP UNTIL cf=0  
   END IF
END SUB

SUB ACC(x,y,p$,r$,com$) STATIC
   '           x    = Coord. X   
   '           y    = Coord. Y   
   '           p$   = Dita   
   '           r$   = Posizioni
   '           com$ = Nome Accordo 
   mk=INSTR(com$," "):IF mk=0 THEN mk=LEN(com$)+1
   mk$=LEFT$(com$,mk-1)
   k=9
   FOR i=1 TO 6
      p(i)=VAL(MID$(r$,i,1))
   NEXT i
   IF INSTR(com$,"#") THEN fs=0:ELSE fs=1
   COLOR 1,0
   FOR i=1 TO 6
      LINE (x+14,y+(i*k))-(x+270,y+(i*k))
      LINE (x+16+(40*i),y+k)-(x+16+(40*i),y+6*k)
   NEXT I
   LINE (x+10,y)-(x+16,y+k)
   LINE (x+16,y+k)-(x+16,y+6*k)
   LINE (x+16,y+6*k)-(x+10,y+6*k+k)
   LINE (x+13,y+k)-(x+13,y+6*k)
   LINE (x+12,y+k)-(x+12,y+6*k)
   COLOR 2,0
   FOR i=1 TO LEN(p$)
      x$=MID$(p$,i,1)
      IF INSTR("01234",x$)<>0 THEN
         PRINT PTAB(x,y+i*k+3);x$
         nx=((n(i)+p(i)-1) MOD 12)+1
         nc$=sc$(nx,fs)
         IF UCASE$(nc$)=UCASE$(mk$) THEN COLOR 1,0:ELSE COLOR 2,0
         PRINT PTAB(x+274,y+i*k+3);nc$
         COLOR 2,0
      END IF
   NEXT i
   COLOR 1,0
   FOR i=1 TO 6
      IF p(i)<>0 THEN LINE (x+(40*p(i))-10,y+(i*k)-2)-(x+(40*p(i)),y+(i*k)+2),1,BF
   NEXT i
   COLOR 2,0:PRINT PTAB(x+(276-LEN(com$)*8)/2,y+7*k+5);com$
   COLOR 1,0
END SUB

SUB Correct(s$) STATIC
   s$=UCASE$(s$)
   SubRep ac$," ",""
   SubRep ac$,"MAGG",""
   SubRep s$,"-","M"
   SubRep s$,"&","B"
   mp=INSTR(s$,"+")
   IF mp>1 THEN
      IF INSTR("0123456789",MID$(s$,mp-1,1))=0 THEN SubRep s$,"+",""
   END IF
   pp=INSTR(MID$(s$,2),"M")+1
   IF pp>2 THEN
      IF INSTR("0123456789",MID$(s$,pp-1,1))<>0 THEN s$=LEFT$(s$,pp-2)+"M"+MID$(s$,pp-1,LEN(s$)-pp+1)
   END IF
END SUB   

SUB Stampa(x0,y0,akk$)
   '         x0   = Coord. X
   '         y0   = Coord. Y
   '         akk$ = Nome Accordo
   SHARED nton
   ac$=UCASE$(akk$)
   SubRep ac$," ",""
   SubRep ac$,"MAGG",""
   com$=""
   i$=LEFT$(ac$,3)
   IF INSTR(ac$,"#") XOR INSTR(ac$,"B") THEN
      IF i$="SOL" THEN w=4:ELSE w=3
   ELSE
      IF i$="SOL" THEN w=3:ELSE w=2
   END IF
   c$=LEFT$(ac$,w)
   k=0
   DO
      INCR k
      n=(k-1)*nton+1
      x$=UCASE$(ak$(n,0))
      y$=UCASE$(ak$(n,3))
      ps1=INSTR(x$," ")-1
      ps2=INSTR(y$," ")-1
      x$=LEFT$(x$,ABS(ps1))
      y$=LEFT$(y$,ABS(ps2))
   LOOP UNTIL k>12 OR (c$=x$ OR c$=y$)
   IF k>12 THEN akk$="ERR":EXIT SUB
   FOR j=n TO n+nton-1
      o1$=UCASE$(ak$(j,0))
      o2$=UCASE$(ak$(j,3))
      SubRep o1$," ",""
      SubRep o2$," ",""
      SubRep o1$,"MAGG",""
      SubRep o2$,"MAGG",""
      IF o1$=ac$ THEN com$=ak$(j,0):EXIT FOR
      IF o2$=ac$ THEN com$=ak$(j,3):EXIT FOR
   NEXT j
   IF j=n+nton THEN akk$="ERR":EXIT SUB
   ACC x0,y0,ak$(j,1),ak$(j,2),com$
END SUB

