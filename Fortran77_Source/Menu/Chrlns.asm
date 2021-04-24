 
;       DEFINITION OF MACROS
 
SAVREG  MACRO
        PUSH    BP
        PUSH    SI
        PUSH    DS
        MOV     BP,SP
        ENDM
 
POPREG  MACRO
        POP     DS
        POP     SI
        POP     BP
        ENDM
 
;       USE STRUC PSEUDO-OP TO DEFINE WORD LOCATIONS FOR OFFSETS
 
FRAME   STRUC
        DW      ?       ;save area for DS register
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
 
;       DEFINE THE SEGMENTS
 
DGROUP  GROUP   DATA
 
DATA    SEGMENT  PARA
 
DATA    ENDS
 
STACK   SEGMENT PARA 'STACK'
        DW      512 DUP(?)
STACK   ENDS
 
CSEG    SEGMENT PARA 'CODE'
        ASSUME  CS:CSEG,DS:DGROUP,ES:DGROUP,SS:DGROUP
 
;----------------------------------------------------------------------;
;       CHRLNS                                                         ;
;                                                                      ;
;       CALL CHRLNS(CHRHLP, LINES, LMAX)                               ;
;              CHRHLP = BINARY ARRAY OF CHARACTERS                     ;
;              LINES  = CHARACTER*72 LINES                             ;
;              LMAX   = NUMBER OF LINES READ                           ;
;----------------------------------------------------------------------;
        PUBLIC  CHRLNS
CHRLNS  PROC    FAR
 
        SAVREG
        PUSH    BP
        JMP     BEGIN_MOVE
 
LMAX    DW      ?                      ;data area
 
BEGIN_MOVE:
        LES     BX,[BP]+X3             ;starting address for CHRHLP
        LDS     BP,[BP]+X2             ;starting address for LINES
 
        MOV     LMAX, 0
        MOV     DI,0
        MOV     SI,0
MOVCHR:
        MOV     AL,ES:[BX][SI]         ;get character
        CMP     AL,1AH                 ;EOF
        JE      END_MOVE
 
        CMP     AL,0DH                 ;CR
        JE      AFTER_MOVE
        CMP     AL,0AH                 ;LF
        JE      AFTER_MOVE
 
        MOV     DS:[BP][DI],AL         ;move to memory
        INC     DI
 
AFTER_MOVE:
        INC     SI
 
        CMP     AL,0AH                 ;LF
        JNE     MOVCHR
 
FILL_LINE:
        MOV     BYTE PTR DS:[BP][DI],32  ;blank
        INC     DI
        CMP     DI, 73
        JNE     FILL_LINE
 
        MOV     DI,0
        ADD     BP,72                  ;next line
        INC     LMAX
        JMP     MOVCHR
 
END_MOVE:
        POP     BP
        LES     BX,[BP]+X1             ;address for LMAX
        MOV     AX,LMAX
        MOV     ES:[BX],AX
 
        POPREG
        RET     4*3
CHRLNS  ENDP
 
CSEG    ENDS
        END
