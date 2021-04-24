TITLE   LIBRARY FUNCTIONS FOR ERA
PAGE 60,132
.LIST
;
;       DEFINITION OF MACROS
;
SAVREG  MACRO
        PUSH    BP
        PUSH    SI
        PUSH    DI
        PUSH    SS
        PUSH    DS
        PUSH    ES
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        MOV     BP,SP
        ENDM
POPREG  MACRO
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        POP     ES
        POP     DS
        POP     SS
        POP     DI
        POP     SI
        POP     BP
        ENDM
SCROLL  MACRO   COLOR
        MOV     AX,600H
        MOV     BH,COLOR
        INT     10H
        ENDM
SET_CUR MACRO
        MOV     AH,2
        INT     10H
        ENDM
SHOW_CHAR MACRO CUR_POS
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX
        MOV     AH,15
        INT     10H
        MOV     AH,2
        MOV     DX,CUR_POS
        INT     10H
        POP     AX
        PUSH    AX
        MOV     AH,9
        MOV     CX,1
        MOV     BL,07
        INT     10H
        POP     AX
        POP     BX
        POP     CX
        POP     DX
        ENDM
INVR_CHAR MACRO CUR_POS
        PUSH    DX
        PUSH    CX
        PUSH    BX
        PUSH    AX
        MOV     AH,15
        INT     10H
        MOV     AH,2
        MOV     DX,CUR_POS
        INT     10H
        POP     AX
        MOV     AH,9
        MOV     CX,1
        MOV     BL,120
        INT     10H
        POP     BX
        POP     CX
        POP     DX
        ENDM
STATUS  MACRO
        MOV     AH,15
        INT     10H
        DEC     AH
        MOV     DL,AH
        MOV     DH,24
        ENDM
FIND_CUR MACRO
        MOV     AH,3
        INT     10H
        MOV     CX,DX
        ENDM
KEYBOARD MACRO INSTR
        MOV     AH,INSTR
        INT     16H
        ENDM
CAPSLET MACRO
        LOCAL   BACK
        CMP     AL,61H  ;'a'
        JL      BACK
        CMP     AL,7AH  ;'z'
        JG      BACK
        XOR     AL,20H
BACK:   NOP
        ENDM
MODES   MACRO   COL
        MOV     AX,COL
        INT     10H
        ENDM
;
;       EQUATES
;
PPI_OUT EQU     61H     ;ppi output register
TIM_MOD EQU     43H     ;timer mode register
TIM_CNT EQU     42H     ;timer count register
HERTZ   EQU     33FEH   ;100 hertz
DURATN  EQU     2801    ;10 ms duration
;
;       USE STRUC PSEUDO-OP TO DEFINE WORD LOCATIONS FOR OFFSETS
;
FRAME   STRUC
        DW      ?       ;save area for DX register
        DW      ?       ;save area for CX register
        DW      ?       ;save area for BX register
        DW      ?       ;save area for AX register
        DW      ?       ;save area for ES register
        DW      ?       ;save area for DS register
        DW      ?       ;save area for SS register
        DW      ?       ;save area for DI register
        DW      ?       ;save area for SI register
        DW      ?       ;save area for BP register
        DD      ?       ;return address
X1      DD      ?       ;address of last parameter
X2      DD      ?       ;address of previous parm
X3      DD      ?       ;address of previous parm
X4      DD      ?       ;address of previous parm
X5      DD      ?       ;address of previous parm
X6      DD      ?       ;address of previous parm
FRAME   ENDS
;
;       DEFINE THE SEGMENTS
;
DGROUP  GROUP   DATA
DATA    SEGMENT  PARA
CS_SAVE DW      ?
DS_SAVE DW      ?
DATA    ENDS
STACK   SEGMENT PARA 'STACK'
        DW      256 DUP(?)
STACK   ENDS
CSEG    SEGMENT PARA 'CODE'
        ASSUME  CS:CSEG,DS:DGROUP,ES:DGROUP,SS:DGROUP
;
;       LIST OF PUBLIC SUBROUTINES (NO FUNCTION STATEMENTS)
;
        PUBLIC  SCREEN
        PUBLIC  WINDOW
        PUBLIC  GOTOXY
        PUBLIC  INCHR
        PUBLIC  EXIT
        PUBLIC  SOUND
        PUBLIC  PICTRE
        PUBLIC  WAIT
        PUBLIC  DRIVE
        PUBLIC  MATCH
        PUBLIC  LIGHT
        PUBLIC  CURSOR
        PUBLIC  ERRHAN
        PUBLIC  GETXY
        PUBLIC  PRINT
        PUBLIC  GETTIT
        PUBLIC  SETATR1
;----------------------------------------------------------------------;
;       SCREEN FUNCTIONS                                               ;
;                                                                      ;
;       CALL SCREEN (I)                                                ;
;               I=0 : CLEAR ENTIRE SCREEN                              ;
;               I=1 : CLEAR WINDOW                                     ;
;               I=2 : 40 COLUMN DISPLAY                                ;
;               I=3 : 80 COLUMN DISPLAY                                ;
;               I=4 : CLEAR WINDOW WITH INVISIBLE ATTRIBUTE            ;
;               I=5 : REVERSE VIDEO CLEAR SCREEN                       ;
;               I=6 : CLEAR SCREEN WITH FLASHING CHARACTERS            ;
;               I=7 : FLASHING REVERSE VIDEO WITH CLEAR SCREEN         ;
;               I=8 : SCROLL UP A PAGE                                 ;
;               I=9 : SCROLL DOWN A PAGE                               ;
;               I=10: SAVE CURRENT PAGE NUMBER                         ;
;               1=11: RESET TO SAVED PAGE NUMBER                       ;
;----------------------------------------------------------------------;
SCREEN  PROC    FAR
        JMP     FUNC
;
;       SCREEN ATTRIBUTE SAVE AREA
;
ATTRIB  DW      00
SCR_ATR DB      07              ;white on black attribute
DOS_PGE DB      ?               ;save area for page number
VID_TYP DW      0B00H           ;starting address for display memory
STR_BSE DW      ?
A_PAGE  DB      4096 DUP(?)
;
;
;       FIND SCREEN FUNCTION
;
FUNC:   SAVREG
        MOV     ATTRIB,0
        LES     BX,[BP]+X1      ;address of code
        MOV     AL,ES:[BX]
        OR      AL,AL
        JNE     NOT_CLR
        JMP     CLRALL          ;I = 0
NOT_CLR:
        DEC     AL
        JNE     NOT_END
        JMP     CLREND          ;I = 1
NOT_END:
        DEC     AL
        JNE     NOT_40
        JMP     COL40           ;I = 2
NOT_40: DEC     AL
        JNE     NOT_80
        JMP     COL80           ;I = 3
NOT_80: DEC     AL
        JNE     NOBLNK
        JMP     BLANK           ;I = 4
NOBLNK: DEC     AL
        JNE     NOREV
        JMP     REVRSE          ;I = 5
NOREV:  DEC     AL
        JNE     NEXT
        JMP     FLASH           ;I = 6
NEXT:   DEC     AL
        JNE     TRY_UP
        JMP     BORDER          ;I = 7
TRY_UP: DEC     AL
        JNE     TRY_DN
        JMP     PAGE_UP         ;I = 8
TRY_DN: DEC     AL
        JNE     TRY_SAV
        JMP     PAGE_DN         ;I = 9
TRY_SAV:
        DEC     AL
        JNE     TRY_SET
        JMP     SAVE_THE_PAGE   ;I = 10
TRY_SET:
        DEC     AL
        JNE     JMPDON
        JMP     SET_THE_PAGE    ;I = 11
JMPDON: JMP     DONES
;
;       CLEAR ENTIRE SCREEN
;
CLRALL: STATUS                  ;video status
        XOR     CX,CX           ;set (0,0)
        SCROLL  SCR_ATR         ;blank entire screen
        JMP     DONES
;
;       CLEAR FROM CURSOR ROW TO BOTTOM OF SCREEN
;
CLREND: STATUS                  ;video status
        PUSH    DX
        FIND_CUR                ;code to get cursor pos
        POP     DX
        MOV     CL,0            ;left column
        CMP     ATTRIB,0
        JNE     BLACK
        SCROLL  SCR_ATR         ;visible window
        JMP     DONES
BLACK:  CMP     ATTRIB,1
        JNE     INVRS
        SCROLL  08              ;invisible window
        JMP     DONES
INVRS:  SCROLL  70H             ;reverse video
        JMP     DONES
;
;       SET UP 40 COLUMN DISPLAY
;
COL40:  MODES   0H              ;set video mode
        JMP     DONES
;
;       SET UP 80 COLUMN DISPLAY
;
COL80:  MODES   2H              ;set video mode
        JMP     DONES
;
;       MAKE WINDOW INVISIBLE
;
BLANK:  MOV     ATTRIB,01
        JMP     CLREND
;
;       MAKE WINDOW REVERSE VIDEO
;
REVRSE: MOV     ATTRIB,02
        JMP     CLREND
;
;       CLEAR SCREEN AND SET UP ATTRIBUTES FOR FLASHING CHARACTERS
;
FLASH:  STATUS
        XOR     CX,CX
        SCROLL  87H
        JMP     DONES
;
;       FLASHING BORDER
;
BORDER: STATUS
        XOR     CX,CX
        SCROLL  0F0H
        JMP     DONES
;
;       SCROLL UP ONE PAGE
;
PAGE_UP:
        CMP     VID_TYP,0B900H
        JE      RETURN_COLOR
        MOV     BX,OFFSET A_PAGE
        MOV     AX,VID_TYP
        MOV     ES,AX
        MOV     DX,STR_BSE
        MOV     DI,0
LOOP_RETURN_PAGE:
        MOV     AL,CS:[BX]
        PUSH    AX
S01:
        IN      AL,DX
        TEST    AL,1
        JNZ     S01
        CLI
S02:
        IN      AL,DX
        TEST    AL,1
        JZ      S02
        POP     AX
        STOSB
        STI
        INC     BX
        CMP     DI,4000
        JNE     LOOP_RETURN_PAGE
        JMP     DONES
RETURN_COLOR:
        STATUS
        CMP     BH,0
        JNE     DEC_PAGE
        JMP     DONES
DEC_PAGE:
        MOV     AL,BH
        DEC     AL
        MOV     AH,05
        INT     10H
        JMP     DONES
;
;       SCROLL DOWN ONE PAGE
;
PAGE_DN:
        INT     11H
        AND     AL,00110000B
        CMP     AL,00100000B
        MOV     AX,0B000H       ;video starting address for mono
        MOV     VID_TYP,AX
        JNE     MONO_SCREEN
        MOV     AX,0B900H       ;video starting address for color
        MOV     VID_TYP,AX
        JMP     COLOR_SCREEN
MONO_SCREEN:
        PUSH    DS
        MOV     AX,40h
        MOV     DS,AX
        MOV     BX,63h
        MOV     AX,DS:[BX]
        CLC
        ADD     AX,6
        MOV     STR_BSE,AX
        MOV     DX,AX
        POP     DS
        MOV     BX,OFFSET A_PAGE
        MOV     AX,VID_TYP
        MOV     ES,AX
        MOV     DI,0
LOOP_SAVE_PAGE:
        IN      AL,DX
        TEST    AL,1
        JNZ     LOOP_SAVE_PAGE
        CLI
S03:
        IN      AL,DX
        TEST    AL,1
        JZ      S03
        MOV     AL,ES:[DI]
        STI
        MOV     CS:[BX],AL
        INC     DI
        INC     BX
        CMP     DI,4000
        JNE     LOOP_SAVE_PAGE
        JMP     DONES
COLOR_SCREEN:
        STATUS
        CMP     BH,03
        JE      DONES
        MOV     AL,BH
        INC     AL
        MOV     AH,05
        INT     10H
        JMP     DONES
;
;       SAVE CURRENT PAGE NUMBER
;
SAVE_THE_PAGE:
        STATUS
        MOV     DOS_PGE,BH
        JMP     DONES
;
;       SET PAGE NUMBER TO DOS_PGE
;
SET_THE_PAGE:
        MOV     AL,DOS_PGE
        MOV     AH,05
        INT     10H
;
;       RETURN TO CALLER
;
DONES:
        POPREG
        RET     04              ;pop data
SCREEN  ENDP
;----------------------------------------------------------------------;
;       WINDOW FUNCTION                                                ;
;                                                                      ;
;       CALL WINDOW (IROWT,IROWB,ICOLL,ICOLR,CHR,IBORD)                ;
;               IROWT = TOP ROW NUMBER (0-24)                          ;
;               IROWB = BOTTOM  ROW NUMBER (0-24)                      ;
;               ICOLL = LEFT COLUMN NUMBER (0-79)                      ;
;               ICOLR = RIGHT COLUMN NUMBER (0-79)                     ;
;               CHR = CHARACTER TO BE DISPLAYED                        ;
;               IBORD = 0 IF NO BOARDER, 1 IF BOARDER                  ;
;                      -1 for scroll up, -2 for scroll down            ;
;                      +2 for outline of box (only)                    ;
;                      +3 SHADOWING                                    ;
;----------------------------------------------------------------------;
WINDOW  PROC    FAR
        SAVREG
        JMP     GET_WINDOW_PARMS
;
;       DATA SAVE AREA
;
WND_TYP DW      0B00H           ;starting address for display memory
WND_BSE DW      ?
ROWT    DB      ?
ROWB    DB      ?
ROWD    DB      ?
COLL    DB      ?
COLR    DB      ?
WND_CHR DB      ?
IBORD   DB      ?
WND_DI  DW      ?
WND_BH  DB      ?
WND_ATR DB      07H             ;white on black
WND_CX  DW      ?
ROW_MX  DB      ?
;
;       GET THE ROW AND COLUMNS
;
GET_WINDOW_PARMS:
        STATUS
        MOV     WND_BH,BH
        LES     BX,[BP]+X6
        MOV     BL,ES:[BX]
        MOV     ROWT,BL
        LES     BX,[BP]+X5
        MOV     BL,ES:[BX]
        MOV     ROWB,BL
        LES     BX,[BP]+X4
        MOV     BL,ES:[BX]
        MOV     COLL,BL
        LES     BX,[BP]+X3
        MOV     BL,ES:[BX]
        MOV     COLR,BL
        LES     BX,[BP]+X2
        MOV     BL,ES:[BX]
        MOV     WND_CHR,BL
        LES     BX,[BP]+X1
        MOV     BL,ES:[BX]
        MOV     IBORD,BL
 
        SUB     CX, CX
        MOV     CL, COLR
        SUB     CL, COLL
        INC     CX
        MOV     WND_CX, CX
 
        MOV     CL, ROWB
        SUB     CL, 1
        MOV     ROW_MX, CL
 
        MOV     BH,WND_BH
        MOV     BL,WND_ATR
        MOV     DH,ROWT
 
        cmp     ibord, +3
        je      loop_window
        cmp     ibord, +2
        je      window_border
        cmp     ibord, -1
        jg      loop_window
        jmp     scroll_up_dn
 
LOOP_WINDOW:
        CMP     DH,ROWB
        JG      WINDOW_BORDER
 
        MOV     DL,COLL
        MOV     AH,02
        INT     10H
 
        MOV     CX, WND_CX
        CMP     IBORD, 3
        JNE     @F
 
ATRIBUTE_ONLY:
        MOV     AH,8
        INT     10H
        MOV     BL,07
        PUSH    CX
        MOV     CX, 1
        MOV     AH,9
        INT     10H
        POP     CX
        inc     dl
        mov     ah,02
        int     10h
        LOOP    ATRIBUTE_ONLY
 
        INC     DH
        JMP     LOOP_WINDOW
 
   @@:
        MOV     AL, WND_CHR
        MOV     AH,9
        INT     10H
 
        INC     DH
        JMP     LOOP_WINDOW
 
WINDOW_BORDER:
        CMP     IBORD, 0
        JNE     @F
        JMP     DONWND
 
   @@:
        CMP     ibord, 3
        jne     @f
        jmp     donwnd
 
   @@:
        MOV     DH, ROWT
        MOV     DL, COLL
        SET_CUR
        MOV     CX, 1
        MOV     AL, 201
        cmp     ibord, +2
        jne     @F
        mov     al, 218
  @@:
        MOV     AH, 9
        INT     10H
 
        INC     DL
        SET_CUR
        MOV     CX, WND_CX
        SUB     CX, 2
        MOV     AL, 205
        cmp     ibord, +2
        jne     @F
        mov     al, 196
  @@:
        MOV     AH, 9
        INT     10H
 
        ADD     DX, WND_CX
        SUB     DX, 2
        SET_CUR
        MOV     CX, 1
        MOV     AL, 187
        cmp     ibord, +2
        jne     @F
        mov     al, 191
  @@:
        MOV     AH, 9
        INT     10H
 
LOOP_RIGHT:
        INC     DH
        CMP     DH, ROW_MX
        JG      jump1
        SET_CUR
        MOV     CX, 1
        MOV     AL, 186
        cmp     ibord, +2
        jne     @F
        mov     al, 179
  @@:
        MOV     AH, 9
        INT     10H
        JMP     LOOP_RIGHT
 
  jump1:
        MOV     DH, ROWT
        MOV     DL, COLL
LOOP_LEFT:
        INC     DH
        CMP     DH, ROW_MX
        JG      jump2
        SET_CUR
        MOV     CX, 1
        MOV     AL, 186
        cmp     ibord, +2
        jne     @F
        mov     al, 179
 @@:
        MOV     AH, 9
        INT     10H
        JMP     LOOP_LEFT
 
 jump2:
        MOV     DH, ROWB
        MOV     DL, COLL
        SET_CUR
        MOV     CX, 1
        MOV     AL, 200
        cmp     ibord, +2
        jne     @F
        mov     al, 192
   @@:
        MOV     AH, 9
        INT     10H
 
        INC     DL
        SET_CUR
        MOV     CX, WND_CX
        SUB     CX, 2
        MOV     AL, 205
        cmp     ibord, +2
        jne     @F
        mov     al, 196
   @@:
        MOV     AH, 9
        INT     10H
 
        ADD     DX, WND_CX
        SUB     DX, 2
        SET_CUR
        MOV     CX, 1
        MOV     AL, 188
        cmp     ibord, +2
        jne     @F
        mov     al, 217
  @@:
        MOV     AH, 9
        INT     10H
        jmp     donwnd
 
scroll_up_dn:
        mov     ch, rowt
        mov     cl, coll
        mov     dh, rowb
        mov     dl, colr
 
        mov     al, 1
        mov     bh, wnd_atr
 
        mov     ah, 06
        cmp     ibord, -2
        jne     @F
        mov     ah,7
 
  @@:   int     10h
 
DONWND:
        POPREG
        RET     24              ;pop data
WINDOW  ENDP
;----------------------------------------------------------------------;
;       GOTOXY                                                         ;
;       CALL GOTOXY (COLUMN,ROW)                                       ;
;----------------------------------------------------------------------;
GOTOXY  PROC    FAR
        SAVREG
        LES     BX,[BP]+X2
        MOV     DL,ES:[BX]      ;column
        LES     BX,[BP]+X1
        MOV     DH,ES:[BX]      ;row
        PUSH    DX
        STATUS
        POP     DX
        SET_CUR                 ;set cursor
        POPREG
        RET     08              ;pop data
GOTOXY  ENDP
;----------------------------------------------------------------------;
;       INCHR : KEYBOARD INPUT                                         ;
;                                                                      ;
;       CALL INCHR(I,J,A)                                              ;
;            I=0 : SEE IF CHARACTER READY (J=1 IF YES, J=0 IF NO)      ;
;            I=1 : GET CHARACTER ,MOVE BUFFER POINTER  AND MOVE SCAN   ;
;                  CODE TO 'J'                                         ;
;            I=2 : IF CHARACTER IS AN 'ESC', 'N', 'Q', PG-DN, PG-UP,   :
;                  HOME, END, UP_ARROW, DN_ARROW, F1 KEY OR F9 KEY     ;
;                  THEN SET J = 1 - 11                                 :
;                  OTHERWISE SET J=0.  CLEAR THE BUFFER IF 'ESC'       ;
;            I=3 : FLUSH BUFFER OF ANY CHARACTERS (ERROR HANDLING)     ;
;----------------------------------------------------------------------;
INCHR   PROC    FAR
        JMP     SAVE
;
;       SACN CODE DATA
;
SCAN_CODE DB    1H,31H,10H,51H,49H,47H,4FH,48H,50H,3BH,43H
;
SAVE:   SAVREG
        LES     BX,[BP]+X3
        MOV     AL,ES:[BX]
        OR      AL,AL
        JE      READY           ;I = 0
        DEC     AL
        JE      GETA            ;I = 1
        DEC     AL
        JE      GETKEY          ;I = 2
        DEC     AL
        JE      FLUSH           ;I = 3
        JMP     DONKEY
;
;       GET CHARACTER AND TEST FOR SPECIAL SCAN CODES
;
GETKEY: KEYBOARD 1
        JZ      GETKEY
        CAPSLET                 ;make capital letter
        MOV     CX,12
        MOV     SI,11
COMPAR: DEC     SI
        CMP     AH,SCAN_CODE[SI]
        LOOPNE  COMPAR
        LES     BX,[BP]+X2
        MOV     ES:[BX],CX
        CMP     CX,1
        JE      GETA            ;get character if <ESC> key
        JMP     DONKEY          ;otherwise leave character in buffer
;
;       GET A CHARACTER
;
GETA:   KEYBOARD 0
        CAPSLET
        LES     BX,[BP]+X1
        MOV     ES:[BX],AL      ;character
        LES     BX,[BP]+X2
        MOV     ES:[BX],AH      ;scan code
        JMP     DONKEY
;
;       CHECK BUFFER STATUS
;
READY:  KEYBOARD 1
        LES     BX,[BP]+X1
        MOV     BYTE PTR ES:[BX],0
        LES     BX,[BP]+X2
        MOV     WORD PTR ES:[BX],0
        JE      DONKEY              ;keyboard not ready
        MOV     WORD PTR ES:[BX],1
        JMP     GETA                ;character entered, get it
;
;       FLUSH THE KEYBOARD BUFFER
;
FLUSH:  KEYBOARD 1
        JE      DONKEY
        KEYBOARD 0
        JMP     FLUSH
;
;       RETURN FROM PROCEDURE
;
DONKEY:
        POPREG
        RET     12              ;pop data
INCHR   ENDP
;----------------------------------------------------------------------;
;       EXIT TO SYSTEM WITHOUT PRINTING 'STOP'                         ;
;                                                                      ;
;       CALL EXIT                                                      ;
;----------------------------------------------------------------------;
EXIT    PROC    FAR
        MOV     AL,0
        MOV     AH,4CH
        INT     21H
EXIT    ENDP
;----------------------------------------------------------------------;
;       SOUND : SOUND A WARNING BEEP                                   ;
;                                                                      ;
;       CALL SOUND (PITCH,DURATION)                                    ;
;            PITCH    = INTEGER*2 MULTIPLES OF 100 HERTZ               ;
;            DURATION = INTEGER*2 MULTIPLES OF 10 MILLISECONDS         ;
;----------------------------------------------------------------------;
SOUND   PROC    FAR
        SAVREG
        LES     BX,[BP]+X2      ;pitch
        MOV     DI,ES:[BX]
        LES     BX,[BP]+X1      ;duration
        MOV     BX,ES:[BX]
;
;       SET UP THE TIMER
;
        MOV     AL,0B6H
        OUT     TIM_MOD,AL      ;write to timer port
        MOV     AX,HERTZ
        CWD
        DIV     DI              ;get pitch
        OUT     TIM_CNT,AL      ;low byte
        MOV     AL,AH
        OUT     TIM_CNT,AL      ;high byte
        IN      AL,PPI_OUT      ;ppi port
        MOV     AH,AL           ;save it
        OR      AL,3
        OUT     PPI_OUT,AL
;
;       LOOP FOR 100 MILLISEC
;
MILLI:  MOV     CX,DURATN
SPKR_ON:
        LOOP    SPKR_ON
        DEC     BX
        JNZ     MILLI
;
;       RESET PPI AND RETURN
;
        MOV     AL,AH
        OUT     PPI_OUT,AL
        POPREG
        RET     08              ;pop data
SOUND   ENDP
;----------------------------------------------------------------------;
;       MAP CHARACTERS AND ATTRIBUTES TO DISPLAY                       ;
;                                                                      ;
;       CALL PICTRE (NUMSET,SET)                                       ;
;               NUMSET = NUMBER OF DATA SETS                           ;
;               SET = DATA SET ADDRESS (COLUMN,ROW,CHARACTER,          ;
;                            ATTRIBUTE, REPEAT FACTOR)                 ;
;----------------------------------------------------------------------;
PICTRE  PROC    FAR
        JMP     DRAW
;
;       DATA AREAS FOR ROUTINE
;
NUMBR   DW      ?               ;NUMSET
ACT_PGE DB      ?               ;normal display
;
;       SET UP THE SCREEN
;
DRAW:   SAVREG
        LES     BX,[BP]+X2
        MOV     AX,ES:[BX]      ;NUMSET
        MOV     NUMBR,AX        ;save it
        LES     BP,[BP]+X1      ;BP contains offset
;
;       INITIALIZE
;
        PUSH    BP
        STATUS
        POP     BP
        MOV     ACT_PGE,BH
        MOV     SI,0
;
;       GET CURSOR POSITION AND DO THE GOTOXY
;
WRITE:
        MOV     DL,ES:[BP][SI]  ;column
        DEC     DL              ;make 0-79
        INC     SI
        INC     SI
        MOV     DH,ES:[BP][SI]  ;row
        DEC     DH              ;make 0-24
        INC     SI
        INC     SI
        PUSH    BP
        PUSH    SI
        MOV     AH,2
        INT     10H             ;gotoxy
        POP     SI
        POP     BP
;
;       GET CHARACTER, ATTRIBUTE, AND REPEAT FACTOR AND DRAW IT
;
        MOV     AL,ES:[BP][SI]  ;character
        INC     SI
        INC     SI
        MOV     BL,ES:[BP][SI]  ;attribute
        INC     SI
        INC     SI
        MOV     CX,ES:[BP][SI]
        INC     SI
        INC     SI
        MOV     BH,ACT_PGE
        PUSH    BP
        PUSH    SI
        MOV     AH,9
        INT     10H             ;draw character
        POP     SI
        POP     BP
        DEC     NUMBR
        JNE     WRITE
;
;       RETURN TO CALLER
;
        POPREG
        RET     08              ;pop data
PICTRE  ENDP
;----------------------------------------------------------------------;
;       WAIT FUNCTION                                                  ;
;                                                                      ;
;       CALL WAIT (I)                                                  ;
;               I=NUMBER OF SECONDS TO WAIT                            ;
;----------------------------------------------------------------------;
WAIT    PROC    FAR
;
;               SAVE REGISTERS  AND DETERMINE COUNTS
;
        SAVREG
        JMP CLOCK
;
;               SAVE AREA FOR THE CLOCK CONTENTS
;
CXSAV   DW      ?               ;high 16 bits of clock
DXSAV   DW      ?               ;low  16 bits of clock
;
;               SAVE PRESENT CLOCK STATUS
;
CLOCK:
        MOV     AH,0
        INT     1AH             ;read timer
        MOV     DXSAV,DX        ;save low  bits
;
;               SET UP TIMER COUNTS
;
        LES     BX,[BP]+X1      ;address of number of seconds
        MOV     AL,ES:[BX]      ;get value
        MOV     DH,0
;       MOV     DL,18           ;conversion to counts
        MOV     DL,1            ;conversion to counts
        MUL     DL              ;result in AX
;
;               SET THE NEW TIME FOR THE END OF THE WAIT
;
        ADD     AX,DXSAV        ;add time waited
        ADC     CX,0H           ;pick up carry bit
        MOV     CXSAV,CX        ;save high bits
        PUSH    AX              ;save result
;
;               LOOP UNTIL COUNT VALUE IN AX HAS ELAPSED
;
CHK_WAIT:
        MOV     AH,0            ;code to read time
        INT     1AH             ;call timer
        POP     AX              ;restore result
        CMP     CX,CXSAV        ;high bits of count
        JL      MOR_WAIT
        CMP     DX,AX           ;counts in DX
        JGE     DONE_WAIT
MOR_WAIT:
        PUSH    AX              ;not done. Save AX
        JMP     CHK_WAIT
;
;               WAIT COMPLETED.  RESTORE REGISTERS AND RETURN
;
DONE_WAIT:
        POPREG
        RET     04              ;pop data
WAIT    ENDP
;----------------------------------------------------------------------;
;       SUBROUTINE DRIVE                                               ;
;                                                                      ;
;       CALL DRIVE(I,J,K)                                              ;
;         I=0 : READ DEFAULT DRIVE CODE                                ;
;           1 : SELECT DEFAULT DRIVE AND GET NUMBER OF DRIVES          ;
;         J=DRIVE CODE (0=A,1=B, ETC)                                  ;
;         K=RESULT (NO. OF DRIVES)                                     ;
;                                                                      ;
;----------------------------------------------------------------------;
DRIVE   PROC    FAR
;
;       SAVE THE RESGITERS JUMP ACCORDING TO 'I'
;
        SAVREG
        LES     BX,[BP]+X3
        MOV     AL,ES:[BX]
        OR      AL,AL
        JE      READ_COD
;
;               SELECT DRIVE AND GET TOTAL NUMBER
;
        LES     BX,[BP]+X2      ;desired drive code address
        MOV     DX,ES:[BX]      ;load drive number
        MOV     AH,0EH          ;drive select code
        INT     21H
        MOV     AH,0
;
;               ADJUST RETURNED NUMBER OF DRIVES FOR DOS 3.0 OR HIGHER
;
        PUSH    AX
        MOV     AH,30H
        INT     21H             ;get DOS version
        CMP     AL,03
        JL      UNDER_VERSION_3
        POP     AX
;       DEC     AX
        PUSH    AX
;
;               POP AX AND OUTPUT IT TO THE CALLING ROUTINE
;
UNDER_VERSION_3:
        POP     AX
        LES     BX,[BP]+X1
        MOV     ES:[BX],AX
        JMP     DON_DRV
;
;               GET DEFAULT DRIVE CODE
;
READ_COD:
        MOV     AH,19H          ;default drive select
        INT     21H
        LES     BX,[BP]+X2
        MOV     AH,0
        MOV     ES:[BX],AX
;
;               RETURN TO CALLER
;
DON_DRV:
        POPREG
        RET     12              ;pop data
DRIVE   ENDP
;----------------------------------------------------------------------;
;       SEARCH FOR FILE MATCH                                          ;
;                                                                      ;
;       CALL MATCH  (IRET,PATH,STRING,ICOD)                            ;
;               IRET = RETURN CODE (0 MEANS FILE PRESENT)              ;
;               PATH = ASCIIZ STRING CONTAINING FILE PATH              ;
;               STRING = FILE NAME (ASCIIZ STRING)                     ;
;               ICOD = TEST CODE (0 FOR FIRST SEARCH, 1 FOR            ;
;                      SUBSEQUENT SEARCHES, -1 TO RESET ASCIIZ NAME    ;
;                      WITH A BLANK (20H) TO ALLOW FORTRAN I/O         ;
;----------------------------------------------------------------------;
MATCH   PROC    FAR
        SAVREG
        JMP     START_MATCH
;
;               SAVE AREA FOR JUMP CONTROL
;
MCOD    DB      ?
BLNK    DB      ?
LNTH    DB      ?
;
;       LOOK FOR MAXIMUM LENGTH OF FIELD
;
START_MATCH:
        LES     BX,[BP]+X4
        MOV     AX,ES:[BX]
        MOV     BYTE PTR ES:[BX],00
        MOV     LNTH,AL
        CMP     AL, 0
        JNE     SET_DS
        MOV     LNTH, 13
;
;               SET DS:DX TO POINT TO THE PATH
;
SET_DS: MOV     BLNK,0          ;initialize
        MOV     MCOD,0
        LDS     DX,[BP]+X3
        MOV     CX,26H          ;normal + system + hidden files
;
;       LOOK FOR A BLANK.  IF FOUND REPLACE WITH ASCIIZ
;
        MOV     DI,0
        MOV     BX,DX
FIND_BLNK:
        INC     DI
        CMP     BYTE PTR DS:[BX+DI],00   ;ASCIIZ
        JE      DONE_BLNK
        CMP     BYTE PTR DS:[BX+DI],20H  ;blank
        JNE     FIND_BLNK
        MOV     BYTE PTR DS:[BX+DI],00   ;ASCIIZ
;
;               CALL THE INTERRUPT AND GET THE NAME AND EXTENSION
;
DONE_BLNK:
        LES     BX,[BP]+X1      ;ICOD
        MOV     AH,4FH
        CMP     WORD PTR ES:[BX],0
        JG      MOV_INT
        MOV     AH,4EH          ;first search
        JE      MOV_INT
        MOV     MCOD,1          ;set code for blank
;
;               CALL THE INTERRUPT
;
MOV_INT:
        INT     21H
;
;               LOAD THE RESULT
;
        LES     BX,[BP]+X4
        MOV     ES:[BX],AX
;
;               GET CURRENT DTA
;
        PUSH    ES              ;result is in ES:BX
        MOV     AH,2FH
        INT     21H
        MOV     AX,ES           ;transfer to DS:BP
        MOV     DS,AX
        POP     ES              ;restore register
;
;               TRANSFER FILE NAME (ASCIIZ STRING)
;
        MOV     CX, 0
        MOV     CL,LNTH
  ;     MOV     CX,32           ;for the path command!!
        MOV     SI,0
        MOV     AX,BX
        LES     BX,[BP]+X2
        PUSH    BP              ;save BP register
        MOV     BP,AX
        MOV     DI,1EH          ;offset in DTA
MOVGET: MOV     AL,0            ;null
        CMP     BLNK,0          ;test for null in name
        JNE     KEEP_BLNK
        MOV     AL,DS:[BP][DI]
KEEP_BLNK:
        CMP     AL,0            ;null
        JNE     MOV_AL
        MOV     BLNK,1          ;set up branch
        MOV     AL,00H          ;binary zero
        CMP     MCOD,0
        JE      MOV_AL
        MOV     AL,20H          ;blank
        mov     cx, 0           ;!!!
MOV_AL: MOV     ES:[BX][SI],AL
        INC     SI
        INC     DI
        LOOP    MOVGET
DONMOV: POP     BP              ;restore from earlier push
        POPREG
        RET     16              ;pop data
MATCH   ENDP
;----------------------------------------------------------------------;
;       CHANGE INTENSITY OF LINE OR BLANK IT OUT                       ;
;                                                                      ;
;       CALL LIGHT(COLMAX,ICODE)                                       ;
;                                                                      ;
;               COLMX = LAST COLUMN NUMBER FOR REVERSE VIDEO           ;
;               ICODE = 0 : NORMAL INTENSITY                           ;
;                       1 : HIGH INTENSITY                             ;
;                       2 : BLANK OUT LINE UP TO COLMAX                ;
;                       3 : REVERSE VIDEO UP TO COLMAX                 ;
;                       4 : BLINKING                                   ;
;                       5 : REVERSE VIDEO WITH BLINKING                ;
;                       6 : NORMAL INTENSITY UP TO COLMAX              ;
;                       7 : HIGH INTENSITY UP TO COLMAX                ;
;                       8 : BLINKING UP TO COLMAX                      ;
;----------------------------------------------------------------------;
LIGHT   PROC    FAR
        SAVREG
        JMP     DISP_DAT
;
;               DATA AREA
;
COLMAX  DB      ?               ;number of columns affected
ICOD    DB      ?               ;code in call
LGT_BSE DW      ?               ;6845 base address
LGT_TYP DW      0B00H           ;screen address
LGT_DI  DW      ?               ;storage for DI
MAX_DI  DW      ?               ;DI at attribute of COLMAX
extra   DB      ?               ;prevent MAX_DI from being disturbed
LGT_ATR DB      ?               ;desired attribute
LGT_PGE DB      ?               ;BH for active page
;ATR_COD DB      1EH,0FH,1EH,78H,9EH,0F8H,1EH,1EH,9EH
ATR_COD DB      07H,0FH,07H,78H,87H,0F8H,07H,0FH,87H
LGT_BH  DB      ?
;
;               INITIALIZE
;
DISP_DAT:
        STATUS
        MOV     LGT_BH,BH
        LES     BX,[BP]+X2      ;colmx
        MOV     Ax,ES:[BX]
        MOV     COLMAX,AL       ;save for reverse video
        LES     BX,[BP]+X1      ;ICODE
        MOV     Ax,ES:[BX]
        MOV     ICOD,AL         ;save jump code
        cmp     colmax,0
        jl      atrib
        STATUS
        MOV     LGT_PGE,BH
        CMP     ICOD,3          ;reverse video ?
        JE      ATRIB
        CMP     ICOD,2          ;blank line ?
        JE      ATRIB
        CMP     ICOD,5          ;reverse video and blinking ?
        JGE     ATRIB           ;ICOD = 5, 6, 7 or 8
        MOV     COLMAX,AH       ;full width of screen
;
;               SET UP CODES TO DO SCREEN READ AND WRITE
;
ATRIB:
        MOV     AX,0
        MOV     AL,ICOD
        MOV     SI,AX
        MOV     AL,ATR_COD[SI]
        MOV     LGT_ATR,AL
;
;               LOOK FOR NEGATIVE VALUE OF COLMAX AS A FLAG THAT
;               ICOD IS THE DESIRED ATTRIBUTE
;
        CMP     COLMAX,0
        JGE     POSITIVE_COLMAX
        NEG     COLMAX
        MOV     AL,ICOD
        MOV     LGT_ATR,AL
        MOV     ICOD,06
POSITIVE_COLMAX:
        FIND_CUR
        MOV     AL,80
        MUL     CH
        MOV     CH,0
        ADD     AX,CX
        SHL     AX,1
        MOV     LGT_DI,AX
        INT     11H
        AND     AL,00110000B
        CMP     AL,00100000B
        MOV     AX,0B000H       ;video starting address for mono
        MOV     LGT_TYP,AX
        JNE     NORMAL
        MOV     AX,0B800H       ;video starting address for color
        ADD     AH,LGT_BH
        MOV     LGT_TYP,AX
;
;               DETERMINE MAX VALUE OF DI
;
NORMAL:
        FIND_CUR
        MOV     AL,80
        MUL     CH
        MOV     CH,0
        MOV     CL,COLMAX
        ADD     AX,CX
        SHL     AX,1
        ADD     AX,1
        MOV     MAX_DI,AX
;
;               SET UP THE BASE DX VALUE
;
        PUSH    DS
        MOV     AX,40h
        MOV     DS,AX
        MOV     BX,63h
        MOV     AX,DS:[BX]
        CLC
        ADD     AX,6
        MOV     LGT_BSE,AX
        MOV     DX,AX
        POP     DS
        MOV     AX,LGT_TYP
        MOV     ES,AX
        MOV     DI,LGT_DI
LOOP_LIGHT:
        MOV     AX,DI
        ROR     AL,1
        MOV     AL,LGT_ATR
        JC      SHOW_LIGHT
        CMP     ICOD,2
        JNE     END_LIGHT
        MOV     AL,20H
SHOW_LIGHT:
        PUSH    AX
L01:
        IN      AL,DX
        TEST    AL,1
        JNZ     L01
        CLI
L02:
        IN      AL,DX
        TEST    AL,1
        JZ      L02
        POP     AX
;       OR      AL,ES:[DI]
        STOSB
        STI
        JMP     CHECK_DI
END_LIGHT:
        INC     DI
CHECK_DI:
        CMP     DI,MAX_DI
        JLE     LOOP_LIGHT
;
;               RETURN TO CALLER
;
DON_LIT:
        POPREG
        RET     08              ;pop data
LIGHT   ENDP
;----------------------------------------------------------------------;
;       CHANGE CURSOR LINE (TO MAKE IT INVISIBLE)                      ;
;                                                                      ;
;       CALL CURSOR(ICODE)                                             ;
;                                                                      ;
;               ICODE = 0 : MAKE VISIBLE                               ;
;                     > 0 : MAKE INVISIBLE                             ;
;----------------------------------------------------------------------;
CURSOR  PROC    FAR
        SAVREG
;
;               GET VALUES FOR ICODE AND SET UP CH AND CL
;
        CALL    CURLIN
        LES     BX,[BP]+X1      ;ICODE
        CMP     BYTE PTR ES:[BX],0
        JE      CHNG_CUR
        OR      CH,00100000B    ;turn off 5 bit
;       MOV     CH,33
;
;               CHANGE CURSOR
;
CHNG_CUR:
        MOV     AH,1            ;set cursor lines
        INT     10H             ;call interrupt
;
;               RETURN TO CALLER
;
        POPREG
        RET     04              ;pop data
CURSOR  ENDP
;-----------------------------------------------------------------------
;       NEAR PROCEDURE FOR GETTING THE CURSOR LINES                    ;
;-----------------------------------------------------------------------
CURLIN  PROC    NEAR
        MOV     CH,11           ;start line
        MOV     CL,12           ;end line
        INT     11H             ;check video type
        AND     AL,00110000B
        CMP     AL,00100000B
        JNE     NOT_COLOR
        MOV     CH,06           ;start line
        MOV     CL,07           ;end line
NOT_COLOR:
        RET
CURLIN  ENDP
;----------------------------------------------------------------------;
;       BYPASS OF CRITICAL ERROR HANDLER (INT 24H CALLED BY DOS)       ;
;                                                                      ;
;       CALL ERRHAN(ICOD)                                              ;
;               ICOD = OPERATION CODE                                  ;
;                       0 = SET UP VECTOR ADDRESS TO POINT TO HERE     ;
;                       1 = RETURN THE SETTING                         ;
;----------------------------------------------------------------------;
ERRHAN  PROC    FAR
        SAVREG
        JMP     SET_REG
;
;               INT 24H ADDRESS SAVE AREA
;
CS_SAV  DW      ?               ;save CS
IP_SAV  DW      ?               ;save IP
IC_SAV  DB      0               ;status of vectors
AX_SAV  DW      ?               ;save area for error codes
CRIT_IP DW      ?               ;save IP from INT 21H
CRIT_CS DW      ?               ;save CS from INT 21H
;
;               GET THE VALUE FOR ICOD AND PERFORM OPERATION
;
SET_REG:
        LES     BX,[BP]+X1
        MOV     AL,ES:[BX]      ;ICOD
        OR      AL,AL           ;is it zero ?
        JE      NEW_VECT
        JMP     OLD_VECT
;
;               GET THE INTERRUPT VECTOR AND STORE IT
;
NEW_VECT:
        CMP     IC_SAV,0        ;DOS vector ?
        JNE     RET_ERR         ;already new vector
        MOV     AX,3524H        ;get vector
        INT     21H             ;call interrupt
        MOV     CS_SAV,ES       ;save CS
        MOV     IP_SAV,BX       ;save IP
        MOV     AX,CS           ;transfer CS to DS
        MOV     DS,AX
        MOV     DX,OFFSET ERR_WORK
        MOV     AX,2524H        ;set up vector
        INT     21H             ;call interrupt
        MOV     IC_SAV,1        ;set flag
        JMP     RET_ERR
;
;               RESET THE ORIGINAL VECTOR
;
OLD_VECT:
        CMP     IC_SAV,1        ;new vector ?
        JNE     RET_ERR         ;already DOS vector
        MOV     DS,CS_SAV       ;restore CS
        MOV     DX,IP_SAV       ;restore IP
        MOV     AX,2524H        ;set up vector
        INT     21H
        MOV     IC_SAV,0
;
;               RETURN TO CALLER
;
RET_ERR:
        POPREG
        RET     04
;
;               HANDLE AN INT 24H TYPE CRITICAL ERROR
;               BY SETTING THE CARRY FLAG (FOR AN ERROR),
;               LOADING THE ERROR CODE IN AX AND BY
;               RETURNING DIRECTLY TO THE INT 21H
;
ERR_WORK:
        MOV     AX_SAV,DI       ;critical error code
        POP     AX              ;IP from INT 24H
        POP     AX              ;CS from INT 24H
        POP     AX              ;flags from INT 24H
        POP     AX              ;AX from INT 21H
        POP     BX              ;BX from INT 21H
        POP     CX              ;CX from INT 21H
        POP     DX              ;DX from INT 21H
        POP     SI              ;SI from INT 21H
        POP     DI              ;DI from INT 21H
        POP     BP              ;BP from INT 21H
        POP     DS              ;DS from INT 21H
        POP     ES              ;ES from INT 21H
        POP     AX              ;IP from INT 21H
        MOV     CRIT_IP,AX
        POP     AX              ;CS from INT 21H
        MOV     CRIT_CS,AX
        POPF                    ;FLAGS from INT 21H
        STC                     ;indicate an error
        PUSHF
        MOV     AX,CRIT_CS      ;restore CS from INT 21H
        PUSH    AX
        MOV     AX,CRIT_IP      ;restore IP from INT 21H
        PUSH    AX
        MOV     AX,AX_SAV       ;get error code
        MOV     AH,AL           ;put in high byte
        MOV     AL,19           ;critical error code
        IRET
ERRHAN  ENDP
;----------------------------------------------------------------------;
;       GET THE CURRENT VALUES FOR THE CURSOR POSITION                 ;
;                                                                      ;
;       CALL GETXY(ICOL,IROW)                                          ;
;               ICOL = INTEGER*2 VALUE FOR THE COLUMN                  ;
;               IROW = INTEGER*2 VALUE FOR THE ROW                     ;
;----------------------------------------------------------------------;
GETXY   PROC    FAR
        SAVREG
        STATUS                  ;put display page no in BH
        FIND_CUR                ;CX holds position values
        LES     BX,[BP]+X2      ;column
        MOV     ES:[BX],CL
        LES     BX,[BP]+X1      ;row
        MOV     ES:[BX],CH
        POPREG
        RET     08
GETXY   ENDP
;----------------------------------------------------------------------;
;       PRINT REAL OR INTEGER OR ALPHANUMERIC DATA TO SCREEN           ;
;                                                                      ;
;       CALL PRINT(VALUE,LEN,IFORM)                                    ;
;               VALUE = ASCIIZ STRING OR A REAL OR INTEGER NUMBER      ;
;               LEN   = LENGTH OF STRING OR FORMAT                     ;
;               IFORM = FORMAT (-2=STRING,-1=INTEGER, OR # DECIMALS    ;
;----------------------------------------------------------------------;
PRINT   PROC    FAR
        SAVREG
        JMP     SEL_OPT
;
;       DATA AREA
;
HI_WORD DW      ?               ;high word part of integer
LO_WORD DW      ?               ;low word part of integer
SAV_REM DW      ?               ;remainder after divide
PBYTE4  DB      ?               ;low order mantissa byte
PBYTE3  DB      ?               ;middle mantissa byte
PBYTE2  DB      ?               ;high order mantissa byte
PBYTE1  DB      ?               ;exponent byte
MAKCHR  DB      80 DUP (?)
LENVAL  DW      ?               ;length of string or format
DECMAL  DW      ?               ;number of decimal places
IFORM   DB      ?               ;code for type of string output
PRN_BSE DW      ?               ;6845 base address
PRN_TYP DW      0B00H           ;screen address for monochrome
STR_DI  DW      ?               ;DI offset for cursor position
PRN_PGE DB      ?               ;active page number
PRN_BH  DB      ?
;
;       DETERMINE TYPE OF DATA AND JUMP
;
SEL_OPT:
        STATUS
        MOV     PRN_BH,BH
        MOV     HI_WORD,0
        MOV     LO_WORD,0
        MOV     IFORM,0         ;initialize for normal string
        LES     BX,[BP]+X2
        MOV     AX,ES:[BX]      ;length (LEN)
        MOV     LENVAL,AX
        LES     BX,[BP]+X1
        MOV     AX,ES:[BX]      ;IFORM
        MOV     DECMAL,AX
        CMP     AX,-3
        JE      NO_EXT
        CMP     AX,-2           ;string ?
        JE      ALPHA_DAT
        CMP     AX,-1           ;integer ?
        JNE     REAL_DAT
        JMP     INT_DAT
;
;               SET CODE TO AVOID PRINTING FILE EXTENSION
;
NO_EXT:
        MOV     IFORM,1
;
;               FILL WORKING ARRAY WITH STRING DATA
;
ALPHA_DAT:
        MOV     SI,0
        PUSH    BP              ;will be destroyed
        LES     BP,[BP]+X3      ;address of string
        MOV     BX,OFFSET MAKCHR
FILL_ALPH:
        MOV     AL,ES:[BP][SI]
        CMP     AL,'$'          ;end of string ?
        JE      END_FILL
        CMP     SI,LENVAL       ;end of string ?
        JE      END_FILL
        CMP     IFORM,0         ;normal string ?
        JE      MOVE_AL
        CMP     AL,'.'
        JE      END_FILL
MOVE_AL:
        MOV     CS:[BX][SI],AL
        INC     SI
        JMP     FILL_ALPH
END_FILL:
        MOV     BYTE PTR CS:[BX][SI],'$'
        POP     BP
        JMP     SHOW_STR
;
;               GET THE REAL NUMBER AND BREAK INTO 4 BYTES
;
REAL_DAT:
        LES     BX,[BP]+X3      ;address of real
        MOV     AL,ES:[BX]
        MOV     PBYTE4,AL
        MOV     AL,ES:[BX]+1
        MOV     PBYTE3,AL
        MOV     AL,ES:[BX]+2
        MOV     PBYTE2,AL
        MOV     AL,ES:[BX]+3
        MOV     PBYTE1,AL
        CMP     AL,0            ;real number zero ?
        JNE     SHIFT
        JMP     ZERO2
SHIFT:  SHL     PBYTE2,1        ;get high bit
        RCL     PBYTE1,1        ;get sign bit into carry
        PUSHF                   ;save sign bit
        STC                     ;set high bit for PBYTE2
        RCR     PBYTE2,1        ;restore PBYTE with high bit set
        MOV     AX,DECMAL       ;save COUNT
        PUSH    AX
;
;               CHECK FOR MULTIPLY BY 10
;
BY_10:
        MOV     AX,DECMAL
        OR      AX,AX           ;look for zero
        JE      NO_MUL          ;no more multiplies
        ADD     PBYTE1,3        ;multiply by 8
        MOV     AX,WORD PTR PBYTE3
        SUB     DX,DX           ;clear high byte
        MOV     DH,PBYTE4
        SHR     AX,1            ;divide by 2
        RCR     DX,1
        SHR     AX,1            ;divide by 2
        RCR     DX,1
        ADD     PBYTE4,DH
        ADC     WORD PTR PBYTE3,AX
        JNC     DEC_DEC         ;was there a carry ?
        RCR     WORD PTR PBYTE3,1
        RCR     PBYTE4,1
        INC     PBYTE1          ;move carry to exponent
DEC_DEC:
        DEC     DECMAL
        JMP     BY_10           ;another multiply ?
;
;               CONVERT REAL NUMBER TO AN INTEGER
;
NO_MUL:
        POP     AX              ;get no of decimals again
        MOV     DECMAL,AX
        SUB     AX,AX           ;clear AX
        MOV     AL,PBYTE1
        SUB     AL,126          ;find power
        MOV     CX,AX           ;set up counter
        SUB     AX,AX           ;clear AX
        MOV     HI_WORD,AX      ;clear HI_WORD
        CMP     CX,0
        JLE     ASCII2
INT2:   SHL     WORD PTR PBYTE4,1
        RCL     PBYTE2,1
        RCL     AX,1            ;move to accumulator
        RCL     HI_WORD,1       ;get carry from accumulator
        LOOP    INT2            ;do CX times
        MOV     LO_WORD,AX
        JMP     ASCII2
;
;               HANDLE INTEGER
;
INT_DAT:
        LES     BX,[BP]+X3      ;number
        MOV     AX,ES:[BX]
        SHL     AX,1
        PUSHF                   ;save sign
        RCR     AX,1
        POPF
        PUSHF
        JNC     ASCII2
        NEG     AX              ;make positive
        JMP     ASCII2
;
;               HANDLE REAL NUMBER ZERO
;
ZERO2:  MOV     AX,0
        MOV     DECMAL,1        ;one decimal point
        CLC
        PUSHF                   ;treat as positive
;
;               CONVERT TO ASCII STRING
;
ASCII2: MOV     CX,LENVAL
        MOV     BX,OFFSET MAKCHR
        MOV     DI,0            ;index to array
FILL_CHAR:
        MOV     BYTE PTR CS:[BX][DI],' '
        INC     DI
        LOOP    FILL_CHAR
        MOV     BYTE PTR CS:[BX][DI],'$'
        MOV     SI,10           ;divisor
        DEC     DI
CLR_DEC:
        CMP     DECMAL,CX       ;look for decimal
        JNE     DIV_10
        MOV     BYTE PTR CS:[BX][DI],'.'
        DEC     DI
        INC     CX
        JMP     CLR_DEC
DIV_10: MOV     DX,HI_WORD      ;look for integer > 65536
        OR      DX,DX
        JE      LOW_ONLY
        PUSH    AX
        MOV     AX,HI_WORD
        SUB     DX,DX           ;clear high level word
        DIV     SI              ;remainder in DX
        MOV     HI_WORD,AX      ;save quotient for next divide
        POP     AX
LOW_ONLY:
        DIV     SI              ;divide by 10
NO_DIV10:
        ADD     DX,'0'          ;convert to ascii digit
        MOV     CS:[BX][DI],DL     ;store character
        DEC     DI
        INC     CX
        OR      AX,AX           ;all done ?
        JNE     CLR_DEC
        CMP     HI_WORD,0
        JNE     CLR_DEC
        CMP     DECMAL,CX
        JGE     CLR_DEC
        POPF
        JNC     SHOW_STR
        MOV     BYTE PTR CS:[BX][DI],'-'
;
;               OUTPUT THE STRING : GET STARTING POSITION
;
SHOW_STR:
        STATUS
        MOV     PRN_PGE,BH
        FIND_CUR
        MOV     AL,80
        MUL     CH
        MOV     CH,0
        ADD     AX,CX
        SHL     AX,1
        MOV     STR_DI,AX
        INT     11H
        AND     AL,00110000B
        CMP     AL,00100000B
        MOV     AX,0B000H       ;video starting address for mono
        MOV     PRN_TYP,AX
        JNE     MOVE_STR
        MOV     AX,0B800H       ;video starting address for color
        ADD     AH,PRN_BH
        MOV     PRN_TYP,AX
;
;       SET UP TRANSFER ADDRESS
;
MOVE_STR:
        PUSH    DS
        MOV     AX,40h
        MOV     DS,AX
        MOV     BX,63h
        MOV     AX,DS:[BX]
        CLC
        ADD     AX,6
        MOV     PRN_BSE,AX
        MOV     DX,AX
        POP     DS
        MOV     BX,OFFSET MAKCHR
        MOV     AX,PRN_TYP
        MOV     ES,AX
        MOV     DI,STR_DI
LOOP_MOVE_STR:
        MOV     AL,CS:[BX]
        CMP     AL,'$'
        JE      DONE_PRINT
        CMP     AL,240
        JNE     NOT_240
        MOV     AL,'$'
NOT_240:
        PUSH    AX
PRN01:
        IN      AL,DX
        TEST    AL,1
        JNZ     PRN01
        CLI
PRN02:
        IN      AL,DX
        TEST    AL,1
        JZ      PRN02
        POP     AX
        STOSB
        STI
        INC     DI
        INC     BX
        JMP     LOOP_MOVE_STR
;
;       PUT CURSOR AND END OF LINE
;
DONE_PRINT:
        MOV     AX,DI
        SHR     AX,1            ;divide by 2
        MOV     BL,80
        DIV     BL
        MOV     DH,AL
        MOV     DL,AH
        MOV     BH,PRN_PGE
        SET_CUR
;
;       RETURN TO CALLER
;
        POPREG
        RET     12              ;pop data
PRINT   ENDP
;----------------------------------------------------------------------;
;       FULL SCREEN EDITING OF THE RUN TITLE                           ;
;                                                                      ;
;       CALL GETTIT(IFLAG,TITLE)                                       ;
;            IFLAG = 0 FOR NORMAL RETURN                               ;
;                  = 1 FOR <F1> (HELP MENU)                            ;
;                  = 2 FOR <PgUp> RETURN                               ;
;                  = 3 FOR <F9> (KEYS MENU)                            ;
;                  = 4 FOR RETURN TO UPDATE DISPLAY CLOCK              ;
;            IFLAG = 0 IN INPUT FOR FIRST CALL                         ;
;                  > 0 IN INPUT FOR RETURN FROM HELP MENU              ;
;----------------------------------------------------------------------;
GETTIT  PROC    FAR
        JMP     TEST_FLAG
;
;       DATA SVAE AREA
;
ACTIVE  DB      ?               ;active page number
SAVE_XY DW      0406H           ;row-column of 1st line of current title
SHOW_XY DW      0C06H           ;row-column of 1st line of edited title
CAPS    DB      'Caps    '
INS     DB      'Ins   '
LINE_21 DB      'Edit the Title on Lines 1-5.  Press <',18H,'> <'
        DB      19H,'> <',1BH,'> <',1AH,'> Keys to Move Cursor'
LINE_22 DB      ' Press <F5> Key to Restore Original Title'
LINE_23 DB      'Press <Esc> or <PgDn> Key for Next Menu'
LINE_NO DB      '1 :2 :3 :4 :5 :'
RUNTIT  DB      680 DUP (?)
CAPS_ON DB      ?
INS_ON  DB      ?
TAB_FLG DB      ?
SAV_POS DW      ?
SAVE_SI DW      ?
IFLAG   DW      ?
MFLAG   DW      ?
SAV_CHR DB      ?
CUR_LIN DW      ?
;
;       BRANCH DEPENDING ON THE VALUE OF IFLAG
;
TEST_FLAG:
        SAVREG
        LES     BX,[BP]+X2
        MOV     AL,ES:[BX]
        CMP     AL,0
        JE      FORMAT
        PUSH    BP
        MOV     DX,SAV_POS
        MOV     BH,ACTIVE
        SET_CUR
        JMP     GET_KEY
;
;       SET UP THE REST OF THE SCREEN DISPLAY.
;
FORMAT:
        STATUS                  ;get active page number
        MOV     ACTIVE,BH
        PUSH    BP              ;save BP
;
;       SAVE THE CURSOR START AND END LINES FOR LATER CURSOR CONTROL
;
        MOV     CX,0B0CH
        INT     11H             ;equipment check
        AND     AL,00110000B
        CMP     AL,00100000B
        JNE     STOR_CUR_LIN
        MOV     CX,0707H
STOR_CUR_LIN:
        MOV     CUR_LIN,CX
        MOV     AH,1
        INT     10H             ;turn on cursor
        MOV     DX,1505H        ;beginning of line 21
        SET_CUR                 ;move cursor
        MOV     SI,0            ;offset
;
;       DISPLAY LINES 21 THRU 23
;
OUT_LINE_21:
        MOV     AH,14           ;print char and advance cursor
        MOV     AL,LINE_21[SI]
        PUSH    SI
   ;    INT     10H             ;display character
        POP     SI
        INC     SI
        CMP     SI,71
        JNE     OUT_LINE_21
        MOV     DX,1613H        ;beginning of line 22
        SET_CUR                 ;move cursor
        MOV     SI,0            ;offset
OUT_LINE_22:
        MOV     AH,14           ;print char and advance cursor
        MOV     AL,LINE_22[SI]
        PUSH    SI
        INT     10H             ;display character
        POP     SI
        INC     SI
        CMP     SI,41
        JNE     OUT_LINE_22
        MOV     DX,1714H        ;beginning of line 23
        SET_CUR                 ;move cursor
        MOV     SI,0            ;offset
OUT_LINE_23:
        MOV     AH,14           ;print char and advance cursor
        MOV     AL,LINE_23[SI]
        PUSH    SI
        INT     10H             ;display character
        POP     SI
        INC     SI
        CMP     SI,39
        JNE     OUT_LINE_23
;
;       SET THE STATUS OF THE CAPS AND INSERT KEYS
;
        MOV     CAPS_ON,0
        MOV     INS_ON,0
;
;       DISPLAY THE RUN TITLE AND SAVE IN STORAGE AREA
;
        POP     BP
        LES     BX,[BP]+X1
        PUSH    BP
        MOV     DH,04
        MOV     CX,1
        MOV     SI,0
OLD_LINE:
        MOV     DL,06
        INC     DH
        CMP     DH,10           ;6th line
        JE      OLD_DONE
SHOW_OLD:
        SET_CUR
        MOV     AH,09
        MOV     AL,ES:[BX][SI]
        PUSH    BX
        MOV     BX,OFFSET RUNTIT
        MOV     CS:[BX][SI],AL
        MOV     BL,112          ;reverse video
        MOV     BH,ACTIVE
        PUSH    SI
        INT     10H             ;show character
        POP     SI
        POP     BX
        INC     SI
        INC     DL
        CMP     DL,74           ;end of line ?
        JNE     SHOW_OLD
        JMP     OLD_LINE
;
;       WRITE THE LINE NUMBERS
;
OLD_DONE:
        MOV     SI,0
        MOV     DI,0
        MOV     DX,0C02H        ;row-column of line number
        SET_CUR
        MOV     BH,ACTIVE
SHOW_LINE:
        MOV     AH,14
        MOV     AL,LINE_NO[SI]
        PUSH    SI
        INT     10H             ;show line number
        POP     SI
        INC     SI
        CMP     SI,15
        JE      GET_OLD
        INC     DI
        CMP     DI,3
        JNE     SHOW_LINE
        MOV     DI,0
        INC     DH              ;new row
        SET_CUR
        JMP     SHOW_LINE
;
;       DISPLAY THE OLD LINE IN THE EDIT POSITION
;
GET_OLD:
        MOV     SI,0
        MOV     DX,0C06H        ;position of 1st line
        MOV     CX,1
SHOW_NEW:
        SET_CUR
        MOV     BX,OFFSET RUNTIT
        MOV     AL,CS:[BX][SI]
        MOV     CS:[BX][SI+340],AL
        MOV     BH,ACTIVE
        MOV     AH,10
        PUSH    SI
        INT     10H             ;show character
        POP     SI
        INC     SI
        CMP     SI,340
        JE      EDIT_TITLE
        INC     DL
        CMP     DL,74
        JNE     SHOW_NEW
        MOV     DL,06
        INC     DH
        JMP     SHOW_NEW
;
;       EDIT THE TITLE
;
EDIT_TITLE:
        MOV     SAV_POS,0C06H
        MOV     DX,0C06H
        SET_CUR
;
;       GET THE KEYBOARD STATUS
;
GET_CAPS_STATUS:
        KEYBOARD 2
        AND    AL,01000000B      ;test 6 bit (CAPS LOCK)
        JE     CAPS_OFF
        MOV    DI,0
        CMP    CAPS_ON,1
        JE     GET_INS_STATUS
        MOV    CAPS_ON,1
        MOV    BL,112           ;reverse video
        JMP    SHOW_CAPS
CAPS_OFF:
        MOV    DI,4
        CMP    CAPS_ON,0
        JE     GET_INS_STATUS
        MOV    CAPS_ON,0
        MOV    BL,07
SHOW_CAPS:
        MOV    SI,0
        MOV    DX,1206H         ;1st letter position
        MOV    CX,1
LOOP_CAPS:
        SET_CUR
        MOV    BH,ACTIVE
        MOV    AH,09
        MOV    AL,CAPS[DI]
        PUSH   SI
        INT    10H              ;show character and attribute
        POP    SI
        INC    SI
        INC    DI
        INC    DL
        CMP    SI,4
        JNE    LOOP_CAPS
        MOV    DX,SAV_POS
        SET_CUR
GET_INS_STATUS:
        KEYBOARD 2
        AND    AL,10000000B      ;test 7 bit (INSERT MODE)
        JE     INS_OFF
        MOV    DI,0
        CMP    INS_ON,1
        JE     GET_CLOCK
        MOV    INS_ON,1
        MOV    BL,112
        JMP    SHOW_INS
INS_OFF:
        MOV    DI,3
        CMP    INS_ON,0
        JE     GET_CLOCK
        MOV    INS_ON,0
        MOV    BL,07
SHOW_INS:
        MOV    SI,0
        MOV    DX,1247H         ;1st letter position
        MOV    CX,1
LOOP_INS:
        SET_CUR
        MOV    BH,ACTIVE
        MOV    AH,09
        MOV    AL,INS[DI]
        PUSH   SI
        INT    10H              ;show character and attribute
        POP    SI
        INC    SI
        INC    DI
        INC    DL
        CMP    SI,3
        JNE    LOOP_INS
        MOV    DX,SAV_POS
        SET_CUR
;
;       RETURN TO RESET CLOCK DISPLAY
;
GET_CLOCK:
        MOV     IFLAG,4
        POP     BP
        JMP     DONE_TITLE
;
;       DECODE KEYPRESS
;
GET_KEY:
        KEYBOARD 1
        JNE     FETCH_KEY
        JMP     GET_CAPS_STATUS
FETCH_KEY:
        KEYBOARD 0
        CMP     AH,63           ;<F5>
        JNE     TEST_ESC
        JMP     GET_OLD
TEST_ESC:
        CMP     AH,1            ;<Esc>
        JNE     TEST_LEFT_CUR
        JMP     PAGE_DN_KEY
TEST_LEFT_CUR:
        CMP     AH,75           ;left cursor
        JNE     TEST_RIGHT_CUR
        JMP     GO_LEFT
TEST_RIGHT_CUR:
        CMP     AH,77           ;right cursor
        JNE     TEST_UP_CUR
        JMP     GO_RIGHT
TEST_UP_CUR:
        CMP     AH,72           ;up cursor
        JNE     TEST_DN_CUR
        JMP     GO_UP
TEST_DN_CUR:
        CMP     AH,80           ;down cursor
        JNE     TEST_INS
        JMP     GO_DN
TEST_INS:
        CMP     AH,82           ;insert key
        JNE     TEST_CAPS
        JMP     GET_INS_STATUS
TEST_CAPS:
        CMP     AH,58           ;caps lock
        JNE     TEST_PAGE_DN
        JMP     GET_KEY
TEST_PAGE_DN:
        CMP     AH,81           ;<PgDn>
        JNE     TEST_HOME
        JMP     PAGE_DN_KEY
TEST_HOME:
        CMP     AH,71           ;<Home>
        JNE     TEST_PAGE_UP
        JMP     EDIT_TITLE
TEST_PAGE_UP:
        CMP     AH,73           ;<PgUp>
        JNE     TEST_END
        JMP     PAGE_UP_KEY
TEST_END:
        CMP     AH,79           ;<End>
        JNE     TEST_ENTER
        JMP     GO_END
TEST_ENTER:
        CMP     AH,28           ;<Enter>
        JNE     TEST_ASCII
        JMP     GO_ENTER
TEST_ASCII:
        CMP     AL,32           ;space key
        JL      TEST_HELP
        CMP     AL,127          ;^ key
        JG      TEST_HELP
        JMP     NEW_CHAR
TEST_HELP:
        CMP     AH,59           ;<F1> key
        JNE     TEST_KEY
        JMP     F1_KEY
TEST_KEY:
        CMP     AH,67           ;<F9> key
        JNE     TEST_BACK
        JMP     F9_KEY
TEST_BACK:
        CMP     AH,14           ;back space key
        JNE     TEST_DEL
        JMP     GO_BACK
TEST_DEL:
        CMP     AH,83           ;del key
        JNE     TEST_CTRL_END
        JMP     GO_DEL
TEST_CTRL_END:
        CMP     AH,75H          ;<CTRL END>
        JNE     TRY_TAB
        JMP     GO_CTRL_END
TRY_TAB:
        CMP     AH,15
        JNE     TRY_AGAIN
        JMP     SET_TAB
TRY_AGAIN:
        JMP     GET_KEY
;
;       MOVE CURSOR TO END OF INFORMATION THE CURRENT LINE
;
GO_END:
        MOV     DX,SAV_POS
        MOV     DL,49H
        PUSH    DX
        SUB     AX,AX
        MOV     AL,DH
        MOV     DH,0
        SUB     AL,0CH
        MOV     BL,68
        MUL     BL
        ADD     AX,DX
        SUB     AX,06
        MOV     SI,AX
        MOV     BX,OFFSET RUNTIT
        POP     DX
FIND_CHAR:
        MOV     SAV_POS,DX
        CMP     DL,05
        JE      MOVE_RIGHT
        MOV     AL,CS:[BX][SI+340]
        CMP     AL,32           ;space
        JNE     MOVE_RIGHT
        DEC     SI
        DEC     DL
        JMP     FIND_CHAR
MOVE_RIGHT:
        MOV     BH,ACTIVE
        JMP     GO_RIGHT
;
;       GO TO BEGINNING OF NEXT LINE
;
GO_ENTER:
        MOV     DX,SAV_POS
        MOV     DL,06
        INC     DH
        CMP     DH,11H
        JNE     GO_MOVE
        MOV     DH,0CH
GO_MOVE:
        MOV     SAV_POS,DX
        SET_CUR
        JMP     GET_KEY
;
;       UP CURSOR MOVE
;
GO_UP:
        MOV     DX,SAV_POS
        DEC     DH
        CMP     DH,0BH
        JNE     GO_MOVE
        MOV     DH,10H
        JMP     GO_MOVE
;
;       DN CURSOR MOVE
;
GO_DN:
        MOV     DX,SAV_POS
        INC     DH
        CMP     DH,11H
        JNE     GO_MOVE
        MOV     DH,0CH
        JMP     GO_MOVE
;
;       LEFT CURSOR MOVE
;
GO_LEFT:
        MOV     DX,SAV_POS
        DEC     DL
        CMP     DL,05H
        JNE     GO_MOVE
        MOV     DL,49H
        DEC     DH
        CMP     DH,0BH
        JNE     GO_MOVE
        MOV     DH,10H
        JMP     GO_MOVE
;
;       RIGHT CURSOR MOVE
;
GO_RIGHT:
        MOV     DX,SAV_POS
        INC     DL
        CMP     DL,4AH
        JNE     GO_MOVE
        MOV     DL,06H
        INC     DH
        CMP     DH,11H
        JNE     GO_MOVE
        MOV     DH,0CH
        JMP     GO_MOVE
;
;       BACK SPACE
;
GO_BACK:
        MOV     DX,SAV_POS
        CMP     DL,06
        JNE     MOVE_LEFT
        JMP     GET_KEY
MOVE_LEFT:
        DEC     DL
        MOV     SAV_POS,DX
        SET_CUR
;
;       DELETE CHARACTER AND MOVE LINE TO THE LEFT
;
GO_DEL:
        MOV     MFLAG,0
        JMP     MOVE_REST
;
;       DELETE REST OF THE LINE
;
GO_CTRL_END:
        MOV     MFLAG,1
;
;       SHIFT CHARACTERS ON LINE TO THE LEFT
;
MOVE_REST:
        MOV     CX,CUR_LIN      ;start-end lines for cursor
        MOV     CH,14
        MOV     AH,1
        INT     10H             ;turn off cursor
        MOV     DX,SAV_POS
        SUB     AX,AX
        MOV     AL,DH
        MOV     DH,0
        SUB     AL,0CH
        MOV     BL,68
        MUL     BL
        ADD     AX,DX
        SUB     AX,06
        MOV     SI,AX
        MOV     BX,OFFSET RUNTIT
LOOP_MOVE:
        MOV     AL,CS:[BX][SI+341]
        CMP     DL,49H
        JE      MAKE_BLANK
        CMP     MFLAG,1
        JE      MAKE_BLANK
        JMP     MOVE_CHAR
MAKE_BLANK:
        MOV     AL,32
MOVE_CHAR:
        MOV     CS:[BX][SI+340],AL
        PUSH    BX
        MOV     BH,ACTIVE
        MOV     AH,14
        PUSH    SI
        INT     10H
        POP     SI
        POP     BX
        INC     DL
        INC     SI
        CMP     DL,4AH
        JNE     LOOP_MOVE
        MOV     BH,ACTIVE
        MOV     DX,SAV_POS
        SET_CUR
        MOV     CX,CUR_LIN
        MOV     AH,1
        INT     10H             ;restore cursor
        JMP     GET_KEY
;
;       TAB TO NEXT WORD
;
SET_TAB:
        MOV     SAV_POS,DX
        SUB     AX,AX
        MOV     AL,DH
        MOV     DH,0
        SUB     AL,0CH
        MOV     BL,68
        MUL     BL
        ADD     AX,DX
        SUB     AX,06
        MOV     SI,AX
        MOV     BX,OFFSET RUNTIT
        MOV     AL,CS:[BX][SI+340]
        MOV     TAB_FLG,AL
GO_TAB:
        MOV     BH,ACTIVE
        MOV     DX,SAV_POS
        INC     DL
        CMP     DL,4AH
        JNE     DO_TAB
        MOV     DL,06H
        INC     DH
        CMP     DH,11H
        JNE     JMP_GO_MOVE
        MOV     DH,0CH
JMP_GO_MOVE:
        JMP     GO_MOVE
DO_TAB:
        MOV     SAV_POS,DX
        SUB     AX,AX
        MOV     AL,DH
        MOV     DH,0
        SUB     AL,0CH
        MOV     BL,68
        MUL     BL
        ADD     AX,DX
        SUB     AX,06
        MOV     SI,AX
        MOV     BX,OFFSET RUNTIT
        MOV     AL,CS:[BX][SI+340]
        MOV     BL,TAB_FLG
        MOV     TAB_FLG,AL
        CMP     BL,32
        JNE     GO_TAB
        CMP     AL,32
        JE      GO_TAB
        MOV     DX,SAV_POS
        DEC     DL
        MOV     SAV_POS,DX
        JMP     MOVE_RIGHT
;
;       PRINTABLE ASCII CHARACTER.  SHOW IT AND ADVANCE THE CURSOR
;
NEW_CHAR:
        PUSH    AX
        MOV     DX,SAV_POS
        SUB     AX,AX
        MOV     AL,DH
        MOV     DH,0
        SUB     AL,0CH
        MOV     BL,68
        MUL     BL
        ADD     AX,DX
        SUB     AX,06
        MOV     SI,AX
        MOV     BX,OFFSET RUNTIT
        POP     AX
;
;       TEST FOR INSERT MODE
;
        CMP     INS_ON,1
        JE      INSERT_MODE
ONE_CHAR:
        MOV     CS:[BX][SI+340],AL
        MOV     AH,14
        MOV     BH,ACTIVE
        INT     10H
        JMP     GO_RIGHT
;
;       INSERT MODE
;
INSERT_MODE:
        CMP     DL,49H          ;end of line ?
        JE      ONE_CHAR
        PUSH    AX
        MOV     CX,CUR_LIN      ;start-end of cursor lines
        MOV     CH,14
        MOV     AH,1
        INT     10H             ;turn off cursor
        POP     AX
LOOP_INSERT:
        PUSH    AX
        MOV     AL,CS:[BX][SI+340]
        MOV     SAV_CHR,AL
        POP     AX
        MOV     CS:[BX][SI+340],AL
        PUSH    BX
        PUSH    SI
        MOV     BH,ACTIVE
        MOV     AH,14
        INT     10H
        POP     SI
        POP     BX
        INC     SI
        INC     DL
        MOV     AL,SAV_CHR
        CMP     DL,4AH
        JNE     LOOP_INSERT
        MOV     CX,CUR_LIN
        MOV     AH,1
        INT     10H             ;restore cursor
        MOV     BH,ACTIVE
        JMP     GO_RIGHT
;
;       <F1> KEY PRESSED.  RETURN IFLAG = 1
;
F1_KEY:
        POP     BP
        MOV     IFLAG,1
        JMP     DONE_TITLE
;
;       <F1> KEY PRESSED.  RETURN IFLAG = 1
;
F9_KEY:
        POP     BP
        MOV     IFLAG,3
        JMP     DONE_TITLE
;
;       SET RETURN (IFLAG) CODE
;
PAGE_DN_KEY:
        MOV     IFLAG,0
        JMP     RETURN_TITLE
;
;       SET FLAG FOR A <PgUp> TYPE RETURN
;
PAGE_UP_KEY:
        MOV     IFLAG,2
;
;       RETURN UPDATED TITLE
;
RETURN_TITLE:
        POP     BP
        LES     BX,[BP]+X1
        MOV     SI,0
FILL_TITLE:
        PUSH    BX
        MOV     BX,OFFSET RUNTIT
        MOV     AL,CS:[BX][SI+340]
        POP     BX
        MOV     ES:[BX][SI],AL
        INC     SI
        CMP     SI,340
        JNE     FILL_TITLE
DONE_TITLE:
        LES     BX,[BP]+X2
        MOV     AX,IFLAG
        MOV     ES:[BX],AX
;
;       RETURN TO CALLER
;
        POPREG
        RET     08              ;pop data
GETTIT  ENDP
;----------------------------------------------------------------------;
;       INTERCEPT INT 10H CALLS AND MONITOR ATTRIBUTE BYTE             ;
;                                                                      ;
;       CALL SETATR1(ICOD)                                             ;
;            ICOD = 0 : SET UP THE INT 10H VECTOR TO POINT HERE        ;
;                 = 1 : RESET THE INT 10 H VECTOR                      ;
;----------------------------------------------------------------------;
SETATR1 PROC    FAR
        SAVREG
        JMP     EGA_ICOD
;
;       DATA AREA
;
CS_SET  DW      ?               ;save CS
IP_SET  DW      ?               ;save IP
IC_SET  DB      0               ;status of vector
INT_CS  DW      ?               ;save CS for INT 10H
SET_BYT DB      00H,0FH,00H,78H,80H,0F8H,00H,00H,80H
ADD_ATR DB      01H,00H,01H,00H,01H,000H,01H,01H,01H
C_ATR   DB      1EH
ICD_ATR DB      ?
;
;       INTERPRET ICOD AND PERFORM OPERATION
;
EGA_ICOD:
        LES     BX,[BP]+X1
        MOV     AL,ES:[BX]
        MOV     ICD_ATR,AL
        cmp     AL,0
        JE      CHECK_EGA       ;ICOD = 0
        CMP     AL,1
        JNE     @F
        JMP     RESET_EGA       ;ICOD = 1
;
;       RESET ATTRIBUTE CODE
;
  @@:
        MOV     C_ATR,AL
        JMP     ESTABLISH_MONITOR
;       JMP     DON_SETATR
;
;       TURN CONTROL_C OFF
;
CHECK_EGA:
;       PUSH    DX
;       MOV     AX,3301H
;       MOV     DL,00
;       INT     21H
;       POP     DX
;
;       ESTABLISH TYPE OF MONITOR
;
ESTABLISH_MONITOR:
        MOV     AX,40H          ;get the video parms
        MOV     ES,AX           ;set up segment address at 40H
        MOV     BX,87H
        MOV     AL,ES:[BX]      ;EGA information byte
        OR      AL,AL           ;is color monitor present ?
        JNE     MOV_EGA_FLG
        JMP     DON_SETATR
MOV_EGA_FLG:
        CMP     ICD_ATR,1
        JG      CHANGE_ATR
        CMP     IC_SET,1        ;already done?
        JE      GO_DON_SETATR
        MOV     IC_SET,1        ;vector has been re-directed
;
;       RESET THE LIGHT CODES FOR COLOR MONITOR
;
CHANGE_ATR:
        MOV     DI,0
LOOP_LIGHT_CODE:
        MOV     AL,ADD_ATR[DI]
        CMP     AL,1
        JNE     @F
        MOV     AL,SET_BYT[DI]
        ADD     AL,C_ATR
        MOV     ATR_COD[DI],AL
  @@:
        INC     DI
        CMP     DI,9
        JNE     LOOP_LIGHT_CODE
        MOV     AL,C_ATR
        MOV     WND_ATR,AL      ;for subroutine WINDOW
;
;       HAS VECTOR BEEN RESET?
;
        CMP     ICD_ATR,0       ;already done?
        JNE     GO_DON_SETATR
;
;
;       SET UP THE JUMP VECTOR
;
        MOV     AX,3510H        ;get INT 10H vector addresses
        INT     21H
        MOV     CS_SET,ES       ;save CS
        MOV     IP_SET,BX       ;save IP
        MOV     AX,CS
        MOV     DS,AX
        MOV     DX,OFFSET CATCH_ATR
        MOV     AX,2510H        ;setset 10H vector
        INT     21H
        MOV     DS,CS_SET
        MOV     DX,IP_SET
        MOV     AX,2518H         ;set up INT 18H vector address
        INT     21H
GO_DON_SETATR:
        JMP     DON_SETATR
;
;       RESET CONTROL_C ON
;
RESET_EGA:
;
;       RESET THE INT 10H VECTOR
;
        CMP     IC_SET,0        ;already done ?
        JE      GO_DON_SETATR
        MOV     DS,CS_SET
        MOV     DX,IP_SET
        MOV     AX,2510H
        INT     21H
        MOV     IC_SET,0        ;clear flag to show done
;
;       RETURN TO CALLER
;
DON_SETATR:
        POPREG
        RET     04
;
;       MONITOR INT 10H ATTRIBUTES
;
CATCH_ATR:
        CMP     AH,6            ;scroll up
        JL      CALL_10H
        CMP     AH,7            ;scroll down
        JG      NO_SCROLL
;
;       MONITOR SCROLL ATTRIBUTE
;
        CMP     BH,007H
        JNE     CALL_10H
        MOV     BH,C_ATR        ;character attribute
        JMP     CALL_10H
;
;       LOOK FOR CHARACTER PRINTING
;
NO_SCROLL:
        CMP     AH,9
        JNE     CALL_10H
        CMP     BL,007H
        JNE     CALL_10H
        MOV     BL,C_ATR        ;character attribute
;
;       CALL THE BIOS FUNCTION 10H
;
CALL_10H:
        INT     18H             ;call original 10H vectors
        IRET
SETATR1 ENDP
;----------------------------------------------------------------------;
;       INTERCEPT EXIT INTERRUPTS AND RESET VIDEO IF NEEDED            ;
;                                                                      ;
;       CALL TRAP                                                      ;
;----------------------------------------------------------------------;
        PUBLIC  TRAP
TRAP    PROC    FAR
        SAVREG
        JMP     SET_TRAP
;
;       DATA AREA
;
OLD_INT DD      ?               ;save area for old INT 21H vector
OLD_CNC DB      ?               ;status of Control_C Check
;
;
;       SET UP THE JUMP VECTOR
;
SET_TRAP:
        MOV     AX,3521H        ;get INT 21H vector addresses
        INT     21H
        MOV     WORD PTR CS:[OLD_INT],BX
        MOV     WORD PTR CS:[OLD_INT + 2],ES
        MOV     AX,CS
        MOV     DS,AX
        MOV     DX,OFFSET TRAP_EXIT
        MOV     AX,2521H        ;reset 21H vector
        INT     21H
;
;       DISABLE CONTROL-C CHECKING
;
        MOV     AX,3300H        ;get status of Control_C check
        INT     21H
        MOV     OLD_CNC,DL      ;save status
        MOV     AX,3301H        ;set status of Control_C check
        MOV     DL,0
        INT     21H
;
;       RETURN TO CALLER
;
        POPREG
        RET
;
;       MONITOR INT 21H CALLS
;
TRAP_EXIT:
        PUSHF
        CMP     AH,4CH          ;call exit
        JE      CALL_EXIT
        POPF
        JMP CS:[OLD_INT]
;
;       RESET THE INT 21H VECTOR
;
CALL_EXIT:
        POPF
        MOV     DS,WORD PTR CS:[OLD_INT + 2]
        MOV     DX,WORD PTR CS:[OLD_INT]
        MOV     AX,2521H
        INT     21H             ;call the interrupt
;
;       RESET THE CONTROL_C CHECK STATUS
;
        MOV     AX,3301H
        MOV     DL,OLD_CNC
        INT     21H
;
;       RESET THE CURSOR LINES
;
        CALL    CURLIN
        MOV     AH,1
        INT     10H
;
;       RESET THE VIDEO VECTORS
;
        CMP     IC_SET,0        ;already done ?
        JE      DON_VID_SET
        MOV     DS,CS_SET
        MOV     DX,IP_SET
        MOV     AX,2510H
        INT     21H
;
;       CALL EXIT (FINALLY)
;
DON_VID_SET:
        MOV     AH,4CH
        INT     21H
TRAP    ENDP
;----------------------------------------------------------------------;
;       RESET THE EXIT INTERRUPTS AND VIDEO IF NEEDED                  ;
;                                                                      ;
;       CALL UNTRAP                                                    ;
;----------------------------------------------------------------------;
        PUBLIC  UNTRAP
UNTRAP  PROC    FAR
        SAVREG
        JMP     RESET_CURSOR
;
;       LIGHT ATTRIBUTE STORAGE
;
ATR_ORG DB      07H,0FH,07H,78H,87H,0F8H,07H,0FH,87H
;
;       RESET THE CURSOR LINES
;
RESET_CURSOR:
        CALL    CURLIN
        MOV     AH,1
        INT     10H
;
;       RESET THE INTERRUPT 21 VECTORS
;
        MOV     DS,WORD PTR CS:[OLD_INT + 2]
        MOV     DX,WORD PTR CS:[OLD_INT]
        MOV     AX,2521H
        INT     21H             ;call the interrupt
;
;       RESET THE CONTROL_C CHECK STATUS
;
        MOV     AX,3301H
        MOV     DL,OLD_CNC
        INT     21H
;
;       RESET THE VIDEO VECTORS
;
        CMP     IC_SET,0        ;already done ?
        JE      DONE_UNTRAP
        MOV     DS,CS_SET
        MOV     DX,IP_SET
        MOV     AX,2510H
        MOV     IC_SET,0
        INT     21H
;
;       RESET THE ORIGINAL LIGHT CODES FOR THE MONITOR
;
        MOV     DI,0
LOOP_ATR_CODE:
        MOV     AL,ATR_ORG[DI]
        MOV     ATR_COD[DI],AL
        INC     DI
        CMP     DI,9
        JNE     LOOP_ATR_CODE
;
;       RETURN TO CALLER
;
DONE_UNTRAP:
        POPREG
        RET
UNTRAP  ENDP
;----------------------------------------------------------------------;
;       READ CONFIG.SYS FILE AND PASS BACK COLOR INDICATOR             ;
;                                                                      ;
;       CALL GETCOL(STRING,PATH,IRET)                                  ;
;               STRING = STRING VALUE TO BE MATCHED                    ;
;               IRET  = RETURN ERROR CODE (0=NO ERROR)                 ;
;               PATH  = ASCIIZ STRING FOR THE PATH                     ;
;----------------------------------------------------------------------;
        PUBLIC  GETCOL
GETCOL  PROC    FAR
        SAVREG
        PUSH    BP              ;BP will be destroyed
        JMP     PAST_SAVE_COLOR
;
;       DATA AREA
;
COLLEN  DW      ?               ;length of string
COLWRD  DW      ?
COLYES  DB      128 DUP (?)     ;example is 'color=yes'
HANCOL  DW      ?
SECCOL  DW      128             ;number of bytes per sector
BYTCOL  DB      128 DUP (?)
;
;       DETERMINE CALL OPTION AND JUMP
;
PAST_SAVE_COLOR:
;
;       LOAD THE STRING INTO COLYES
;
        LES     BX,[BP]+X3
        MOV     SI,0
        MOV     DI,0
STRCOL:
        MOV     AL,ES:[BX][SI]
        CMP     AL,'$'               ;end of string
        JE      OPEN_CONFIG
        MOV     COLYES[DI],AL
        INC     SI
        INC     DI
        JMP     STRCOL
;
;       OPEN THE FILE
;
OPEN_CONFIG:
        DEC     DI
        MOV     COLLEN,DI            ;length of string - 1
        MOV     COLWRD,0             ;string not found
;
;       SEARCH FOR A BLANK AND REPALCE WITH AN 00H
;
        LES     BX,[BP]+X2
        MOV     SI,-1
FIND_BLANK:
        INC     SI
        MOV     AL,ES:[BX][SI]
        CMP     AL,20H               ;blank
        JNE     FIND_BLANK
        MOV     BYTE PTR ES:[BX][SI],0        ;make and asciiz path
;
;       OPEN FILE
;
        LDS     DX,[BP]+X2
        MOV     AX,3D02H             ;open file for reading
        INT     21H                  ;call interrupt
        JNC     READ_COL_FILE        ;no error encountered
        JMP     DON_GETCOL
;
;       STORE THE HANDLE AND READ 128 BYTES
;
READ_COL_FILE:
        MOV     HANCOL,AX       ;store handle
        MOV     BX,HANCOL       ;get handle
        MOV     CX,SECCOL       ;128 bytes
        MOV     AX,CS
        MOV     DS,AX           ;set up DS:DX
        MOV     DX,OFFSET BYTCOL
        MOV     AH,3FH          ;read command
        INT     21H             ;call interrupt
        JC      CLOSE_COLOR
;
;       LOOK THROUGH BUFFER FOR STRING 'COLOR=YES'
;
        MOV     BX,-1
COLOR_STRING:
        INC     BX
        MOV     DI,-1
NEXT_LETTER:
        INC     DI
        MOV     AL,BYTCOL[BX][DI]
        CMP     AL,1AH            ;EOF
        JE      CLOSE_COLOR
        CMP     AL,61H            ;a
        JL      NOT_SMALL_LETTER
        CMP     AL,7AH            ;z
        JG      NOT_SMALL_LETTER
        SUB     AL,20H            ;make small letters caps
NOT_SMALL_LETTER:
        CMP     AL,COLYES[DI]
        JNE     COLOR_STRING
        CMP     DI,COLLEN         ;end of COLYES ?
        JNE     NEXT_LETTER
        MOV     COLWRD,1
;
;               CLOSE THE HANDLE
;
CLOSE_COLOR:
        MOV     BX,HANCOL
        MOV     AH,3EH          ;reset command
        INT     21H             ;call interrupt
;
;       RETURN TO CALLER
;
DON_GETCOL:
        POP     BP              ;restore BP
        LES     BX,[BP]+X1
        MOV     AX,COLWRD       ;color indicator code
        MOV     ES:[BX],AX      ;return color inidcator
        POPREG
        RET     04              ;pop data
GETCOL  ENDP
;----------------------------------------------------------------------;
;       TEMPORARY EXIT TO DOS                                          ;
;                                                                      ;
;       CALL DOS (COMMAND_STRING$)                                     ;
;----------------------------------------------------------------------;
        PUBLIC  DOS
DOS     PROC    FAR
        SAVREG
        PUSH    AX
        JMP     PARSE_COMMAND
;
;       DATA AREA
;
PGM_FILE   DB     "\COMMAND.COM",0
DOS_LINE   DB     2,"  ",0DH
CMD_LINE   DB     ?
           DB     "/C "
           DB     64 DUP (?)
PRM_BLK    DB     14 DUP (?)
;
;       SET UP COMMAND LINE
;
PARSE_COMMAND:
        MOV     SI,0
        LES     BX,[BP]+X1
        MOV     AL,ES:[BX]           ;test for call to DOS
        CMP     AL,'$'
        JE      SET_BLOCK
        MOV     SI,4
FILL_CMD_LINE:
        MOV     AL,ES:[BX+SI-4]        ;get character
        MOV     CMD_LINE[SI],AL    ;load it into command line
        INC     SI
        CMP     AL,"$"               ;end of command?
        JNE     FILL_CMD_LINE
        DEC     SI
        MOV     CMD_LINE[SI],0DH     ;send a carriage return
        MOV     AX,SI                ;get lower byte of SI
        MOV     CMD_LINE,AL          ;length of command
;
;       ALLOCATE MEMORY
;
SET_BLOCK:
  ;     MOV     BX,0FFFFH       ;call bigger space than available
  ;     MOV     AX,CS
  ;     MOV     ES,AX
  ;     MOV     DS,AX
  ;     MOV     AH,4AH
  ;     INT     21H             ;get available memory in BX
  ;     SUB     BX,0F00H        ;set aside 61440 bytes for command.com
  ;     MOV     AH,4AH
  ;     INT     21H             ;grab it
 
        MOV     AX,CS
        MOV     ES,AX
        MOV     DS,AX
;
;       SET UP THE CALL TO FUNCTION 4AH
;
        MOV     DX,OFFSET PGM_FILE
        MOV     BX,OFFSET PRM_BLK
        MOV     WORD PTR PRM_BLK[00H],0h
        MOV     WORD PTR PRM_BLK[02H],OFFSET CMD_LINE
        CMP     SI,0            ;DOS Exit?
        JNE     LOAD_CS
        MOV     WORD PTR PRM_BLK[02H],OFFSET DOS_LINE
LOAD_CS:
        MOV     WORD PTR PRM_BLK[04H],CS
        MOV     WORD PTR PRM_BLK[06H],5CH
        MOV     WORD PTR PRM_BLK[08H],ES
        MOV     WORD PTR PRM_BLK[0AH],6CH
        MOV     WORD PTR PRM_BLK[0CH],ES
;
;       CALL THE FUNCTION (TO LOAD AND EXECUTE THE PROGRAM)
;
        MOV     AL,0
        MOV     AH,4BH
        INT     21H
;
;       WE RETURN HERE.  POP THE SAVED REGISTERS AND BAIL OUT
;
        POP     AX
        POPREG
        RET     04
DOS     ENDP
;-----------------------------------------------------------------------;
;       CHANGE THE ATTRIBUTE OF A ROW OF CHARACTERS                     ;
;                                                                       ;
;       CALL ATRBUT1(NUMBER,ATR)                                        ;
;-----------------------------------------------------------------------;
        PUBLIC  ATRBUT1
ATRBUT1 PROC    FAR
        SAVREG
        JMP     START_CHANGE
;
;       DATA AREA
;
NEW_ATR DB      ?
START_CHANGE:
        LES     BX,[BP]+X2
        MOV     SI,ES:[BX]      ;number of characters
        LES     BX,[BP]+X1
        MOV     AX,ES:[BX]      ;new attribute
        MOV     NEW_ATR,AL      ;save it
 
        STATUS
CHANGE_LINE:
        MOV     AH,08
        INT     10H             ;read character and atribute
 
        MOV     BL,NEW_ATR
        MOV     CX,1
        MOV     AH,9
        INT     10H
 
        FIND_CUR
        INC     DL
        SET_CUR
 
        DEC     SI
        CMP     SI,0
        JNE     CHANGE_LINE
 
        POPREG
        RET     08              ;pop data
ATRBUT1 ENDP
 
;-----------------------------------------------------------------------;
;       FIND THE CURRENT DIRECTORY                                      ;
;                                                                       ;
;       CALL GETDIR(DL,PATH)                                            ;
;-----------------------------------------------------------------------;
        PUBLIC  GETDIR
GETDIR  PROC    FAR
        SAVREG
        JMP     FIND_DIR
;
;       DATA AREA
;
DIRBUF  DB      64 DUP(?)
FIND_DIR:
        LES     BX,[BP]+X2
        MOV     DL,ES:[BX]      ;current drive
 
        MOV     AX,CS
        MOV     DS,AX
        MOV     SI,OFFSET DIRBUF
 
        MOV     AH,47H
        INT     21H
 
;       BUILD UP THE PATH NOW
 
        LES     BX,[BP]+X1
        MOV     BYTE PTR ES:[BX],'C'
        MOV     BYTE PTR ES:[BX]+1,'D'
        MOV     BYTE PTR ES:[BX]+2,'\'
 
        MOV     SI,OFFSET DIRBUF
        MOV     DI,3
 
FILL_DIR:
        MOV     AL,DS:[SI]
        CMP     AL,' '
        JE      @F
        CMP     AL,0H
        JE      @F
        MOV     ES:[BX][DI],AL
        INC     SI
        INC     DI
        JMP     FILL_DIR
 
  @@:
        MOV     BYTE PTR ES:[BX][DI],'$'
 
        POPREG
        RET     08              ;pop data
GETDIR  ENDP
;----------------------------------------------------------------------;
;       SORT LIST                                                      ;
;                                                                      ;
;       CALL SORT(LEN,NUM,LIST)                                        ;
;              LEN  = LENGTH OF EACH ENTRY IN LIST                     ;
;              NUM  = NUMBER OF ENTRIES IN LIST                        ;
;              LIST = ASCII LIST TO BE SORTED                          ;
;----------------------------------------------------------------------;
        PUBLIC  SORT
SORT    PROC    FAR
 
        SAVREG
        JMP     SORT_BEGIN
 
;       DATA AREA
 
LEN     DW      ?                    ;Length of table entry
NUM     DW      ?                    ;Number of entries
LIST    DB      3000 DUP(?)          ;Work area for the sort
SORT_PNTR  DW   200 DUP(?)
 
;       LOAD THE CALL VARIABLES
 
SORT_BEGIN:
        LES     BX,[BP]+X3
        MOV     AX,ES:[BX]
        MOV     LEN,AX
        LES     BX,[BP]+X2
        MOV     AX,ES:[BX]
        MOV     NUM,AX
 
;       LOAD THE WORKING SORT TABLE
 
        MOV     CX,NUM
        MOV     SI,0
        MOV     DI,0
        LES     BX,[BP]+X1
 
STORE_LIST:
        MOV     AL,ES:[BX][SI]       ;Character move
        MOV     LIST[SI],AL
        INC     DI
        INC     SI
        CMP     DI,LEN               ;Complete entry moved?
        JNE     STORE_LIST
        MOV     DI,0
        LOOP    STORE_LIST           ;Get next entry
 
;       CALL LOCAL SUBOUTINE TO FIND SMALLEST ENTRY
 
        MOV     SI,0
        MOV     CX,NUM
 
NOT_DONE_YET:
        CALL    FIND_MIN
        PUSH    SI
        ADD     SI,SI
        MOV     SORT_PNTR[SI],DI
        POP     SI
        INC     SI
        LOOP    NOT_DONE_YET
 
;       RELOAD THE SORTED LIST
 
 
        MOV     CX,NUM
        MOV     SI,0
        MOV     DI,0
        LES     BX,[BP]+X1
 
RE_STORE_LIST:
        MOV     AL,ES:[BX][SI]       ;Character move
        MOV     LIST[SI],AL
        INC     DI
        INC     SI
        CMP     DI,LEN               ;Complete entry moved?
        JNE     RE_STORE_LIST
        MOV     DI,0
        LOOP    RE_STORE_LIST        ;Get next entry
 
;       SET UP TO MOVE THE SORTED LIST USING POINTERS
 
        PUSH    BP                   ;Will use as an index
        MOV     DI,0                 ;Pointer for entry
        MOV     BP,SORT_PNTR[DI]     ;Position in table entry begins
        MOV     SI,0
        MOV     CX,LEN               ;Will move byte by byte up to length
 
MOVE_SORTED_LIST:
        MOV     AL,LIST[BP]
        MOV     ES:[BX][SI],AL
        INC     SI
        INC     BP
        LOOP    MOVE_SORTED_LIST     ;Move one entry
        MOV     CX,LEN               ;Re-initialize for LOOP
        INC     DI
        PUSH    DI
        ADD     DI,DI
        MOV     BP,SORT_PNTR[DI]
        POP     DI
        CMP     DI,NUM               ;Done yet?
        JNE     MOVE_SORTED_LIST
 
        POP     BP
 
        POPREG
        RET     4*3
SORT    ENDP
 
;----------------------------------------------------------------------;
;       NEAR PROCDURE FIND_MIN.  FIND THE MINIMUM ASCII VALUES IN      ;
;       A LIST.  CALLED BY SORT.  ONLY THE FIRST 4 CHARACTERS SCANED.  ;
;       DI CONTAINS THE POINTER TO THE LIST ON RETURN.                 ;
;----------------------------------------------------------------------;
FIND_MIN  PROC    NEAR
        PUSH    SI                   ;Save SI for return
        PUSH    CX
        PUSH    BX
 
        MOV     DI,0
        MOV     SI,LEN
        MOV     CX,NUM
        DEC     CX
 
COMPARE_STRING:
        PUSH    SI
        PUSH    DI
        MOV     BX,0
COMPARE_BYTE:
        MOV     AL,LIST[DI]
        CMP     AL,LIST[SI]          ;Compare bytes
        JL      CONTINUE_COMPARE
        JG      @F
 
        INC     SI
        INC     DI
        INC     BX
        CMP     BX,LEN
        JL      COMPARE_BYTE
 
   @@:                               ;Found a lower value
        POP     DI
        POP     SI
        MOV     DI,SI
        JMP     NEXT_SI
 
CONTINUE_COMPARE:
        POP     DI
        POP     SI
 
NEXT_SI:
        ADD     SI,LEN
        LOOP    COMPARE_STRING
 
        MOV     LIST[DI],07FH        ;Mark found
        MOV     LIST[DI+1],07FH
 
        POP     BX
        POP     CX
        POP     SI
        RET
FIND_MIN  ENDP
;----------------------------------------------------------------------;
;       CHECK STATUS OF THE PRINTER                                    ;
;                                                                      ;
;       CALL PRNTST(IRET)                                              ;
;               IRET = PRINTER NUMBER (INPUT)                          ;
;               IRET = RETURN CODE (OUTPUT)                            ;
;----------------------------------------------------------------------;
        PUBLIC  PRNTST
PRNTST  PROC    FAR
        SAVREG
;
;       EVALUATE ICODE AND BRANCH
;
        LES     BX,[BP]+X1
        MOV     DX,ES:[BX]
        MOV     AX,ES
        MOV     DS,AX
        MOV     AL,12
        MOV     AH,0
 
        INT     17H
        MOV     AL,AH
        MOV     AH,0
        LES     BX,[BP]+X1
        MOV     ES:[BX],AX
        POPREG
        RET     04              ;pop data
PRNTST  ENDP
;----------------------------------------------------------------------;
;       DELETE FILE AND MAKE BACKUP.  REMOVE BACKUP FIRST IF PRESENT.  ;
;                                                                      ;
;       CALL BACKUP(PATH)                                              ;
;               PATH = ASCIIZ STRING CONTAINING FILE PATH              ;
;----------------------------------------------------------------------;
        PUBLIC  BACKUP
BACKUP  PROC    FAR
        SAVREG
        JMP     LOAD_NAME
;
;               NAME WORK AREA
;
OLDNAME DB      32 DUP (?)
NEWNAME DB      32 DUP (?)
;
;               LOAD THE NAME OF THE FILE
;
LOAD_NAME:
        LES     BP,[BP]+X1
        MOV     BX,OFFSET OLDNAME
        MOV     SI,0
        MOV     DI,32
        MOV     CX,32
LOOP_NAME:
        MOV     AL,ES:[BP][SI]
        MOV     CS:[BX][SI],AL
        MOV     CS:[BX][DI],AL
        INC     SI
        INC     DI
        LOOP    LOOP_NAME
        MOV     AL,0
        MOV     CS:[BX+31],AL
        MOV     CS:[BX+63],AL
;
;               RESET THE NEWNAME FILE EXTENSION TO .BAK
;
        MOV     SI,32
FIND_DOT:
        INC     SI
        MOV     AL,CS:[BX][SI]
        CMP     AL,'.'
        JNE     FIND_DOT
        INC     SI
        MOV     AL,'B'
        MOV     CS:[BX][SI],AL
        INC     SI
        MOV     AL,'A'
        MOV     CS:[BX][SI],AL
        INC     SI
        MOV     AL,'K'
        MOV     CS:[BX][SI],AL
;
;               REMOVE THE EXISTING BACKUP FILE
;
        MOV     AX,CS
        MOV     DS,AX
        MOV     DX,OFFSET NEWNAME
        MOV     AH,41H
        INT     21H
;
;               RENAME THE FILE
;
        MOV     AX,CS
        MOV     ES,AX
        MOV     DI,OFFSET NEWNAME
        MOV     DS,AX
        MOV     DX,OFFSET OLDNAME
        MOV     AH,56H
        INT     21H
        POPREG
        RET     04              ;pop data
BACKUP  ENDP
;----------------------------------------------------------------------;
;       RETURN DATE AS YEAR, MONTH AND DAY                             ;
;                                                                      ;
;       CALL GETDAT(YEAR,MONTH,DAY)                                    ;
;               ALL VALUES ARE IN FORM OF 4 BYTE INTEGER               ;
;----------------------------------------------------------------------;
        public  getdat
GETDAT  PROC    FAR
        SAVREG
;
;       CALL INTERRUPT 21H TO GET DATE (AH = 2H)
;
        MOV     AH,2AH
        INT     21H
        LES     BX,[BP]+X1
        MOV     ES:[BX],DL      ;Day
        LES     BX,[BP]+X2
        MOV     ES:[BX],DH      ;Month
        LES     BX,[BP]+X3
        MOV     ES:[BX],CX      ;Year
        POPREG
        RET     12              ;pop data
GETDAT  ENDP
;----------------------------------------------------------------------;
;       RETURN TIME AS HOUR, MINUTES AND SECONDS                       ;
;                                                                      ;
;       CALL GETTIM(HOUR,MINUTES,SECONDS)                              ;
;               ALL VALUES ARE IN FORM OF 4 BYTE INTEGER               ;
;----------------------------------------------------------------------;
        public  gettim
GETTIM  PROC    FAR
        SAVREG
;
;       CALL INTERRUPT 21H TO GET TIME (AH = 2C)
;
        MOV     AH,2CH
        INT     21H
        LES     BX,[BP]+X1
        MOV     ES:[BX],DH      ;Hour
        LES     BX,[BP]+X2
        MOV     ES:[BX],CL      ;Minutes
        LES     BX,[BP]+X3
        MOV     ES:[BX],CH      ;seconds
        POPREG
        RET     12              ;pop data
GETTIM  ENDP
CSEG    ENDS
        END
