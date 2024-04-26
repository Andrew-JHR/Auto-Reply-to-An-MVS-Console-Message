//ANDREWJA JOB  CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//ASM      EXEC PGM=ASMA90,PARM='OBJECT,NODECK,XREF(FULL),RENT,FLAG(NOC-
//             ONT)'
//SYSLIB   DD   DISP=SHR,DSN=ANDREWJ.SOURCE.MAC#
//         DD   DISP=SHR,DSN=SYS1.MACLIB
//         DD   DISP=SHR,DSN=SYS1.MODGEN
//SYSUT1   DD   UNIT=SYSDA,SPACE=(CYL,(10,5)),DSN=&SYSUT1
//SYSIN    DD   *
*
*  Module name       = ICH3024D
*
*  Descrpition       = Issuing R XX,Y to ICH302D or ICH304D
*
*  Function          = Automatically issues R XX,Y
*                      when the ICH302D or ICH304D appears

*
*  Operation         = R1  points to the addr of the CTXT
*                      R13 points to the addr of the standard save area
*                      R14 return point
*                      R15 entry  point
*
*  Register usage    = R5  - addr of the CTXT
*                      R10 - module data register
*                      R11 - potential 2nd base
*                      R12 - module base register
*                      R13 - pointer of a standard save area
*                      R14 - return point
*                      R15 - entry  point
*
*  CONTROL  BLOCK    = R5  - pointer to the address of the CTXT
*    name     mapping macro    reason used                  usage
*   ------    -------------    ---------------------------  -----
*    CTXT       IEZVX100       WTO USER EXIT PARAMETER LIST  R,W
*    MGCR       IEZMGCR        SVC 34 PARAMETER LIST         C,D
*
*    KEY = R-READ, W-WRITE, C-CREATE, D-DELETE
*
*    macros          =  GETMAIN, FREEMAIN, MGCR
*
* Author : Andrew Jan   20240217
* Update : check CTXTJBNM to see if it is from CICSxxxx         U240220
*          only executed when it is from CICSxxxx               U240220
*

         PRINT OFF               bypass inline macro expansion
         LCLA  &REG
.LOOP    ANOP                    inline macro to generate registers
R&REG    EQU   &REG              generate the equates
&REG     SETA  &REG+1            next
         AIF   (&REG LE 15).LOOP if not yet finished, loop it
         PRINT ON                trigger printing
         PRINT GEN               not allow macro expansion

*   Work area
DATAAREA DSECT
         DS    0F
SAVEAREA DS    18F               standard save area
         DS    0F
MGCR     IEZMGCR DSECT=NO        for issuing MVS command
         ORG   MGCRTEXT
COMMAND      DS  CL2             'R '
REPLYID      DS  CL2
REPLYMSG     DS  CL2             ',Y'
UTOKEN       DS  CL80            'CONSOLE1'
         ORG
DATALEN  EQU   *-DATAAREA



         IEZVX100                DSECT for CTXT

ICH3024D CSECT
ICH3024D AMODE 31
ICH3024D RMODE ANY

         USING *,R15              setup addressibility
         STM   R14,R12,12(R13)    save parent's register
         B     CMNTTAIL           skip over the remarks
*
CMNTHEAD EQU   *
         PRINT GEN                print out remarks
         DC    CL8'&SYSDATE'      compiling date
         DC    C' '
         DC    CL5'&SYSTIME'      compiling time
         DC    C'ANDREW JAN'      author
         CNOP  2,4                ensure half word boundary
         PRINT NOGEN              disable macro expansion
CMNTTAIL EQU   *

         BALR  R12,0              module base
         DROP  R15                avoid compiling warning
         USING *,R12              addressibility

         L     R5,0(,R1)          establish addressability
         USING CTXT,R5            to the CTXT

         CLC   CTXTJBNM(4),=C'CICS' if the job name is CICSxxxx U240220
         BE    GO_ON               yes,branch                   U240220
         BR    R14                 no,go back to caller         U240220

GO_ON    EQU   *                                                U240220
         GETMAIN RU,LV=DATALEN
         LR    R10,R1              address return in R1
         USING DATAAREA,R10        addressability to dynmaic           X
                                   storage
         ST    R13,SAVEAREA+4      set backward ptr
         LA    R15,SAVEAREA        get address of out own savearea
         ST    R15,8(,R13)         save ours to caller's
         LR    R13,R15             R13 points to our own savearea


         #SPM PRINT=GEN            generate smp macros

         #PERF R14,PROCESS_RXXY
         #PERF R14,ISSUE_MGCR

FINISH   EQU   *
         L     R13,4(R13)
         FREEMAIN RU,LV=DATALEN,A=(R10) free the storage
         LM    R14,R12,12(R13)        restore caller's register values
         BR    R14                    go back to caller



*  PROCEDURE  -  issue 'R XX,Y via MGCR
        #SUBR  ISSUE_MGCR,R14
                 STC   R1,MGCRLGTH         save length in the MGCRPL
                 SR    R0,R0
                 MGCR  MGCRPL              issue the command
        #ESUB

        #SUBR  PROCESS_RXXY,R14
                 XC    MGCRPL(MGCRLTH),MGCRPL  clear parm list
                 MVC   REPLYID,CTXTRPID        insert the reply ID
                 MVC   COMMAND,=C'R '          set the command
                 MVC   REPLYMSG,=C',Y'         always answer yes
                 LA    R1,(MGCRTEXT-MGCRPL)    get length
                 LA    R1,6(,R1)               total length
                 MVC   UTOKEN,BLANKS           clear
                 MVC   UTOKEN(8),=C'CONSOLE1'  console id
                 MVI   MGCRFLG1,X'80'          having flag2
                 MVC   MGCRFLG2,=X'0008'       having utoken
        #ESUB

*  constants
        LTORG   ,
BLANKS  DS   0CL80
        DC   80C' '
         END   ICH3024D
/*
//SYSPRINT DD   SYSOUT=*
//SYSLIN   DD   DSN=&OBJ,SPACE=(3040,(40,40),,,ROUND),UNIT=VIO,
//         DISP=(NEW,PASS),
//         DCB=(BLKSIZE=3040,LRECL=80,RECFM=FBS,BUFNO=1)
//
//LKED     EXEC PGM=IEWL,PARM='MAP,LET,LIST,NCAL,RENT',REGION=4096K,
//         COND=(4,LT,ASM)
//SYSLIN   DD   DSN=&OBJ,DISP=(OLD,DELETE)
//SYSLMOD  DD   DSN=SYS1.USER.LINKLIB(ICH3024D),DISP=SHR
//SYSPRINT DD   SYSOUT=*
//
