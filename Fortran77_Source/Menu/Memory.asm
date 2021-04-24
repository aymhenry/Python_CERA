TITLE   MEMORY FUNCTION
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
        DW      ?
        DW      ?
DATA    ENDS
STACK   SEGMENT PARA 'STACK'
        DW      256 DUP(?)
STACK   ENDS
 
CSEG    SEGMENT PARA 'CODE'
        ASSUME  CS:CSEG,DS:DGROUP,ES:DGROUP,SS:DGROUP
;----------------------------------------------------------------------;
;       Test the Available Memory to See if an Auxiliary Program       ;
;       Can be Loaded Without Crashing                                 ;
;                                                                      ;
;       CALL MEMORY (SIZE)                                             ;
;----------------------------------------------------------------------;
        PUBLIC  MEMORY
MEMORY  PROC    FAR
        SAVREG
 
        MOV     BX, -1
        MOV     Ah, 48H
        INT     21H
        xchg    ax, bx
 
        LES     BX, [BP]+X1
        MOV     ES:[BX], ax
 
        POPREG
        RET     04              ;pop data
MEMORY  ENDP
CSEG    ENDS
        END
