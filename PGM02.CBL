      ******************************************************************PGM02
       IDENTIFICATION                  DIVISION.                        PGM02
      ******************************************************************PGM02
       PROGRAM-ID. PGM0201.                                             PGM0201
       AUTHOR.       NEUCLAIR J. ANGELE JR.                             PGM02
       DATE-WRITTEN. 08 MAR 2011.                                       PGM02
       DATE-COMPILED.                                                   PGM02
      ******************************************************************PGM02
      *REMARKS.                                                         PGM02
      *     *----------------------------------------------------------*PGM02
      *     *#NOME     : PGM02                                         *PGM02
      *     *----------------------------------------------------------*PGM02
      *     *#TIPO     : BATCH                                         *PGM02
      *     *----------------------------------------------------------*PGM02
      *     *#ANALISTA : JOSE L. S. GOMES                              *PGM02
      *     *----------------------------------------------------------*PGM02
      *     *#FUNCAO   : BALANCE LINE DE DOIS ARQUIVOS                 *PGM02
      *     *----------------------------------------------------------*PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       ENVIRONMENT                     DIVISION.                        PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       CONFIGURATION                   SECTION.                         PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
       SPECIAL-NAMES.                                                   PGM02
           DECIMAL-POINT IS COMMA.                                      PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       INPUT-OUTPUT                    SECTION.                         PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
       FILE-CONTROL.                                                    PGM02
           SELECT ESYS010 ASSIGN TO SYS010                              PGM02
                  FILE    STATUS IS WS-FS-SYS010.                       PGM02
      *                                                                 PGM02
           SELECT ESYS020 ASSIGN TO SYS020                              PGM02
                  FILE    STATUS IS WS-FS-SYS020.                       PGM02
      *                                                                 PGM02
           SELECT SSYS030 ASSIGN TO SYS030                              PGM02
                  FILE    STATUS IS WS-FS-SYS030.                       PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       DATA                            DIVISION.                        PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       FILE                            SECTION.                         PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
       FD  ESYS010                                                      PGM02
           BLOCK     CONTAINS 0  RECORDS                                PGM02
           RECORDING MODE     IS F                                      PGM02
           LABEL     RECORD   IS STANDARD.                              PGM02
           COPY ECADCLI.                                                PGM02
      *                                                                 PGM02
       FD  ESYS020                                                      PGM02
           BLOCK     CONTAINS 0  RECORDS                                PGM02
           RECORDING MODE     IS F                                      PGM02
           LABEL     RECORD   IS STANDARD.                              PGM02
           COPY ECADCLI.                                                PGM02
      *                                                                 PGM02
       FD  SSYS030                                                      PGM02
           BLOCK     CONTAINS 0  RECORDS                                PGM02
           RECORDING MODE     IS F                                      PGM02
           LABEL     RECORD   IS STANDARD.                              PGM02
           COPY ECADCLI.                                                PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       WORKING-STORAGE                 SECTION.                         PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       77  FILLER                  PIC     X(32)         VALUE          PGM02
           'III WORKING-STORAGE SECTION III'.                           PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    AREA DE FILE STATUS                                         *PGM02
      ******************************************************************PGM02
       77  WS-FS-SYS010            PIC X(02)             VALUE SPACES.  PGM02
       77  WS-FS-SYS020            PIC X(02)             VALUE SPACES.  PGM02
       77  WS-FS-SYS030            PIC X(02)             VALUE SPACES.  PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    AREA DE CONTADORES                                          *PGM02
      ******************************************************************PGM02
       77  ACU-LIDOS-SYS010        PIC     9(06) COMP-3  VALUE ZEROS.   PGM02
       77  ACU-LIDOS-SYS020        PIC     9(06) COMP-3  VALUE ZEROS.   PGM02
       77  ACU-GRAVADOS            PIC     9(06) COMP-3  VALUE ZEROS.   PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    AREA DE AUXILIARES                                          *PGM02
      ******************************************************************PGM02
       01  WS-MSG-SYS010           PIC     X(19)         VALUE          PGM02
           ' DO ARQUIVO SYS010'.                                        PGM02
       01  WS-MSG-SYS020           PIC     X(19)         VALUE          PGM02
           ' DO ARQUIVO SYS020'.                                        PGM02
       01  WS-MSG-SYS030           PIC     X(19)         VALUE          PGM02
           ' DO ARQUIVO SYS030'.                                        PGM02
      *                                                                 PGM02
       01  WS-DISPLAY-LG           PIC     9(06)         VALUE ZEROS.   PGM02
      *                                                                 PGM02
       01  WS-DATA-MAQ             PIC     9(08)         VALUE ZEROS.   PGM02
      *                                                                 PGM02
       01  WS-DATA-MAQ-R REDEFINES WS-DATA-MAQ.                         PGM02
           03  WS-DT-ANO-R         PIC     9(04).                       PGM02
           03  WS-DT-MES-R         PIC     9(02).                       PGM02
           03  WS-DT-DIA-R         PIC     9(02).                       PGM02
      *                                                                 PGM02
       01  WS-HORA-MAQ             PIC     9(08)         VALUE ZEROS.   PGM02
                                                                        PGM02
       01  WS-HORA-MAQ-R REDEFINES WS-HORA-MAQ.                         PGM02
           03  WS-HR-HOR-R         PIC     9(02).                       PGM02
           03  WS-HR-MIN-R         PIC     9(02).                       PGM02
           03  WS-HR-SEG-R         PIC     9(02).                       PGM02
           03  WS-HR-MIL-R         PIC     9(02).                       PGM02
      *                                                                 PGM02
       01  WS-MENSAGEM             PIC     X(40)         VALUE SPACES.  PGM02
       01  WS-DATA-DISPLAY.                                             PGM02
           03  WS-DATA-BR.                                              PGM02
               05  WS-DIA          PIC     9(02)         VALUE ZEROS.   PGM02
               05  FILLER          PIC     X             VALUE '/'.     PGM02
               05  WS-MES          PIC     9(02)         VALUE ZEROS.   PGM02
               05  FILLER          PIC     X             VALUE '/'.     PGM02
               05  WS-ANO          PIC     9(04)         VALUE ZEROS.   PGM02
               05  FILLER          PIC     X             VALUE ' '.     PGM02
           03  WS-HORA-BR.                                              PGM02
               05  WS-HORA         PIC     9(02)         VALUE ZEROS.   PGM02
               05  FILLER          PIC     X             VALUE ':'.     PGM02
               05  WS-MINUTO       PIC     9(02)         VALUE ZEROS.   PGM02
               05  FILLER          PIC     X             VALUE ':'.     PGM02
               05  WS-SEGUNDO      PIC     9(02)         VALUE ZEROS.   PGM02
      *                                                                 PGM02
       01  WS-CHAVE-SYS010.                                             PGM02
           05  WS-CH-SYS010        PIC     9(06)         VALUE ZEROS.   PGM02
      *                                                                 PGM02
       01  WS-CHAVE-SYS020.                                             PGM02
           05  WS-CH-SYS020        PIC     9(06)         VALUE ZEROS.   PGM02
      *                                                                 PGM02
       77  WS-FIM                  PIC     9(01)         VALUE ZEROS.   PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    AREA DE LINKAGE                                             *PGM02
      ******************************************************************PGM02
       01  WS-SAIDA.                                                    PGM02
           03  WS-MSG              PIC     X(70)         VALUE SPACES.  PGM02
           03  WS-COD-RET          PIC     X(02)         VALUE ZEROS.   PGM02
           03  WS-COD-RET-R        PIC     9(02) REDEFINES WS-COD-RET.  PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       77  FILLER                  PIC     X(32)         VALUE          PGM02
           'FFF FIM DA WORKING-STORAGE FFF'.                            PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       LINKAGE                         SECTION.                         PGM02
      ******************************************************************PGM02
       01  LK-MSG                  PIC     X(70).                       PGM02
       01  LK-COD-RET              PIC     9(02).                       PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
       PROCEDURE                       DIVISION USING LK-MSG            PGM02
                                                      LK-COD-RET.       PGM02
      ******************************************************************PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA PRINCIPAL                                            *PGM02
      ******************************************************************PGM02
       RT-PRINCIPAL                    SECTION.                         PGM02
      *                                                                 PGM02
           PERFORM RT-INICIALIZA.                                       PGM02
      *                                                                 PGM02
           PERFORM RT-PROCESSA.                                         PGM02
      *                                                                 PGM02
           PERFORM RT-FINALIZA.                                         PGM02
      *                                                                 PGM02
       RT-PRINCIPALX.                  EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE INICIALIZACAO                                     *PGM02
      ******************************************************************PGM02
       RT-INICIALIZA                   SECTION.                         PGM02
      *                                                                 PGM02
           DISPLAY ERASE.                                               PGM02
      *                                                                 PGM02
           INITIALIZE ACU-LIDOS-SYS010                                  PGM02
                      ACU-LIDOS-SYS020                                  PGM02
                      ACU-GRAVADOS.                                     PGM02
      *                                                                 PGM02
           DISPLAY '**************************************************'.PGM02
      *                                                                 PGM02
           MOVE '#PGM02.900I - INICIO DO PROCESSAMENTO '                PGM02
                                       TO WS-MENSAGEM.                  PGM02
      *                                                                 PGM02
           ACCEPT WS-DATA-MAQ          FROM DATE YYYYMMDD.              PGM02
           ACCEPT WS-HORA-MAQ          FROM TIME.                       PGM02
      *                                                                 PGM02
           MOVE WS-DT-ANO-R            TO WS-ANO.                       PGM02
           MOVE WS-DT-MES-R            TO WS-MES.                       PGM02
           MOVE WS-DT-DIA-R            TO WS-DIA.                       PGM02
           MOVE WS-HR-HOR-R            TO WS-HORA.                      PGM02
           MOVE WS-HR-MIN-R            TO WS-MINUTO.                    PGM02
           MOVE WS-HR-SEG-R            TO WS-SEGUNDO.                   PGM02
      *                                                                 PGM02
           DISPLAY WS-MENSAGEM                                          PGM02
                   WS-DATA-DISPLAY.                                     PGM02
           DISPLAY '**************************************************'.PGM02
      *                                                                 PGM02
           MOVE SPACES                 TO WS-MSG.                       PGM02
           MOVE ZERO                   TO WS-COD-RET.                   PGM02
      *                                                                 PGM02
           OPEN INPUT  ESYS010                                          PGM02
                       ESYS020                                          PGM02
                OUTPUT SSYS030.                                         PGM02
      *                                                                 PGM02
           IF WS-FS-SYS010             EQUAL ZEROS                      PGM02
               CONTINUE                                                 PGM02
           ELSE                                                         PGM02
               DISPLAY '#PGM02.901I - ERRO NA ABERTURA' WS-MSG-SYS010   PGM02
               DISPLAY '#PGM02.901I - FILE STATUS = '   WS-FS-SYS010    PGM02
               MOVE   WS-FS-SYS010     TO WS-COD-RET                    PGM02
               STRING '#PGM02.901I - ERRO NA ABERTURA DO ARQUIVO SYS010'PGM02
                      '! FILE STATUS = ' WS-FS-SYS010                   PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
           IF WS-FS-SYS020             EQUAL ZEROS                      PGM02
               CONTINUE                                                 PGM02
           ELSE                                                         PGM02
               DISPLAY '#PGM02.902I - ERRO NA ABERTURA' WS-MSG-SYS020   PGM02
               DISPLAY '#PGM02.902I - FILE STATUS = '   WS-FS-SYS020    PGM02
               MOVE   WS-FS-SYS020     TO WS-COD-RET                    PGM02
               STRING '#PGM02.902I - ERRO NA ABERTURA DO ARQUIVO SYS020'PGM02
                      '! FILE STATUS = ' WS-FS-SYS020                   PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FINALIZA
           END-IF.                                                      PGM02
      *                                                                 PGM02
           IF WS-FS-SYS030            EQUAL ZEROS                       PGM02
               CONTINUE                                                 PGM02
           ELSE                                                         PGM02
               DISPLAY '#PGM02.903I - ERRO NA ABERTURA' WS-MSG-SYS030   PGM02
               DISPLAY '#PGM02.903I - FILE STATUS = '   WS-FS-SYS030    PGM02
               MOVE   WS-FS-SYS030     TO WS-COD-RET                    PGM02
               STRING '#PGM02.903I - ERRO NA ABERTURA DO ARQUIVO SYS030'PGM02
                      '! FILE STATUS = ' WS-FS-SYS030                   PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
           PERFORM RT-LE-SYS010                                         PGM02
      *                                                                 PGM02
           IF WS-FS-SYS010             EQUAL '10'                       PGM02
               DISPLAY '#PGM02.904I - ARQUIVO SYS010 VAZIO'             PGM02
               DISPLAY '#PGM02.904I - FILE STATUS = '   WS-FS-SYS010    PGM02
               MOVE   WS-FS-SYS010     TO WS-COD-RET                    PGM02
               STRING '#PGM02.904I - ARQUIVO SYS010 VAZIO! '            PGM02
                      'FILE STATUS = ' WS-FS-SYS010                     PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FECHA                                         PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
           PERFORM RT-LE-SYS020                                         PGM02
      *                                                                 PGM02
           IF WS-FS-SYS020             EQUAL '10'                       PGM02
               DISPLAY '#PGM02.905I - ARQUIVO SYS020 VAZIO'             PGM02
               DISPLAY '#PGM02.905I - FILE STATUS = '   WS-FS-SYS020    PGM02
               MOVE   WS-FS-SYS020     TO WS-COD-RET                    PGM02
               STRING '#PGM02.905I - ARQUIVO SYS020 VAZIO! '            PGM02
                      'FILE STATUS = ' WS-FS-SYS020                     PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FECHA                                         PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
       RT-INICIALIZAX.                 EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE LEITURA DO ARQUIVO SYS010                         *PGM02
      ******************************************************************PGM02
       RT-LE-SYS010                    SECTION.                         PGM02
      *                                                                 PGM02
           READ ESYS010.                                                PGM02
      *                                                                 PGM02
           EVALUATE WS-FS-SYS010                                        PGM02
               WHEN ZEROS                                               PGM02
                   MOVE FD-RC-CODIGO   OF ESYS010                       PGM02
                                       TO WS-CHAVE-SYS010               PGM02
                   ADD 1               TO ACU-LIDOS-SYS010              PGM02
               WHEN '10'                                                PGM02
                   MOVE HIGH-VALUE     TO WS-CHAVE-SYS010               PGM02
                   ADD 1               TO WS-FIM                        PGM02
               WHEN OTHER                                               PGM02
                   DISPLAY '#PGM02.906 - ERRO NA LEITURA' WS-MSG-SYS010 PGM02
                   DISPLAY '#PGM02.906 - FILE STATUS = '  WS-FS-SYS010  PGM02
                   MOVE   WS-FS-SYS010 TO WS-COD-RET                    PGM02
                   STRING '#PGM02.906I - ERRO NA LEITURA DO ARQUIVO SYS'PGM02
                          '010! FILE STATUS = ' WS-FS-SYS010            PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
                   PERFORM RT-FECHA                                     PGM02
                   PERFORM RT-FINALIZA                                  PGM02
           END-EVALUATE.                                                PGM02
      *                                                                 PGM02
       RT-LE-SYS010X.                  EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE LEITURA DO ARQUIVO SYS020                         *PGM02
      ******************************************************************PGM02
       RT-LE-SYS020                    SECTION.                         PGM02
      *                                                                 PGM02
           READ ESYS020.                                                PGM02
      *                                                                 PGM02
           EVALUATE WS-FS-SYS020                                        PGM02
               WHEN ZEROS                                               PGM02
                   MOVE FD-RC-CODIGO   OF ESYS020                       PGM02
                                       TO WS-CHAVE-SYS020               PGM02
                   ADD 1               TO ACU-LIDOS-SYS020              PGM02
               WHEN '10'                                                PGM02
                   MOVE HIGH-VALUE     TO WS-CHAVE-SYS020               PGM02
                   ADD 1               TO WS-FIM                        PGM02
               WHEN OTHER                                               PGM02
                   DISPLAY '#PGM02.907 - ERRO NA LEITURA' WS-MSG-SYS020 PGM02
                   DISPLAY '#PGM02.907 - FILE STATUS = '  WS-FS-SYS020  PGM02
                   MOVE   WS-FS-SYS020 TO WS-COD-RET                    PGM02
                   STRING '#PGM02.907I - ERRO NA LEITURA DO ARQUIVO SYS'PGM02
                          '020! FILE STATUS = ' WS-FS-SYS020            PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
                   PERFORM RT-FECHA                                     PGM02
                   PERFORM RT-FINALIZA                                  PGM02
           END-EVALUATE.                                                PGM02
      *                                                                 PGM02
       RT-LE-SYS020X.                  EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE INICIALIZACAO                                     *PGM02
      ******************************************************************PGM02
       RT-PROCESSA                     SECTION.                         PGM02
      *                                                                 PGM02
           PERFORM RT-COMPARA          UNTIL WS-FIM EQUAL 2.            PGM02
      *                                                                 PGM02
           PERFORM RT-FECHA.                                            PGM02
      *                                                                 PGM02
           PERFORM RT-CONTABILIZA.                                      PGM02
      *                                                                 PGM02
       RT-PROCESSAX.                   EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE MONTAGEM DO RELATORIO                             *PGM02
      ******************************************************************PGM02
       RT-COMPARA                      SECTION.                         PGM02
      *                                                                 PGM02
           IF WS-CH-SYS010             EQUAL WS-CH-SYS020               PGM02
               MOVE FD-REG-CADCLI      OF ESYS020                       PGM02
                                       TO FD-REG-CADCLI                 PGM02
                                       OF SSYS030                       PGM02
               PERFORM RT-GRAVA                                         PGM02
               PERFORM RT-LE-SYS010                                     PGM02
               PERFORM RT-LE-SYS020                                     PGM02
           ELSE                                                         PGM02
               IF WS-CH-SYS010         LESS WS-CH-SYS020                PGM02
                   MOVE FD-REG-CADCLI  OF ESYS010                       PGM02
                                       TO FD-REG-CADCLI                 PGM02
                                       OF SSYS030                       PGM02
                   PERFORM RT-GRAVA                                     PGM02
                   PERFORM RT-LE-SYS010                                 PGM02
               ELSE                                                     PGM02
                   IF WS-CH-SYS010     GREATER WS-CH-SYS020             PGM02
                       MOVE FD-REG-CADCLI                               PGM02
                                       OF ESYS020                       PGM02
                                       TO FD-REG-CADCLI                 PGM02
                                       OF SSYS030                       PGM02
                       PERFORM RT-GRAVA                                 PGM02
                       PERFORM RT-LE-SYS020                             PGM02
                   END-IF                                               PGM02
               END-IF                                                   PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
       RT-COMPARAX.                    EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE GRAVACAO DO ARQUIVO SYS030                        *PGM02
      ******************************************************************PGM02
       RT-GRAVA                        SECTION.                         PGM02
      *                                                                 PGM02
           WRITE FD-REG-CADCLI         OF SSYS030.                      PGM02
      *                                                                 PGM02
           IF WS-FS-SYS030             EQUAL '00'                       PGM02
               ADD 1                   TO ACU-GRAVADOS                  PGM02
           ELSE                                                         PGM02
               DISPLAY '#PGM02.908 - ERRO NA GRAVACAO' WS-MSG-SYS030    PGM02
               DISPLAY '#PGM02.908 - FILE STATUS = '   WS-FS-SYS030     PGM02
               MOVE   WS-FS-SYS030     TO WS-COD-RET                    PGM02
               STRING '#PGM02.908I - ERRO NA GRAVACAO DO ARQUIVO SYS030'PGM02
                      '! FILE STATUS = ' WS-FS-SYS030                   PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FECHA                                         PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
       RT-GRAVAX.                      EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE FECHAMENTO DOS ARQUIVOS                           *PGM02
      ******************************************************************PGM02
       RT-FECHA                        SECTION.                         PGM02
      *                                                                 PGM02
           CLOSE ESYS010                                                PGM02
                 ESYS020                                                PGM02
                 SSYS030.                                               PGM02
      *                                                                 PGM02
           IF WS-FS-SYS010             EQUAL ZEROS                      PGM02
               CONTINUE                                                 PGM02
           ELSE                                                         PGM02
               DISPLAY '#PGM02.909I - ERRO NO FECHAMENTO' WS-MSG-SYS010 PGM02
               DISPLAY '#PGM02.909I - FILE STATUS = '     WS-FS-SYS010  PGM02
               MOVE   WS-FS-SYS010     TO WS-COD-RET                    PGM02
               STRING '#PGM02.909I - ERRO NO FECHAMENTO DO ARQUIVO SYS0'PGM02
                      '10! FILE STATUS = ' WS-FS-SYS010                 PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FECHA                                         PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
           IF WS-FS-SYS020             EQUAL ZEROS                      PGM02
               CONTINUE                                                 PGM02
           ELSE                                                         PGM02
               DISPLAY '#PGM02.910I - ERRO NO FECHAMENTO' WS-MSG-SYS020 PGM02
               DISPLAY '#PGM02.910I - FILE STATUS = '     WS-FS-SYS020  PGM02
               MOVE   WS-FS-SYS020     TO WS-COD-RET                    PGM02
               STRING '#PGM02.910I - ERRO NO FECHAMENTO DO ARQUIVO SYS0'PGM02
                      '20! FILE STATUS = ' WS-FS-SYS020                 PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FECHA                                         PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
           IF WS-FS-SYS030             EQUAL ZEROS                      PGM02
               CONTINUE                                                 PGM02
           ELSE                                                         PGM02
               DISPLAY '#PGM02.911I - ERRO NO FECHAMENTO' WS-MSG-SYS030 PGM02
               DISPLAY '#PGM02.911I - FILE STATUS = '     WS-FS-SYS030  PGM02
               MOVE   WS-FS-SYS030     TO WS-COD-RET                    PGM02
               STRING '#PGM02.911I - ERRO NO FECHAMENTO DO ARQUIVO SYS0'PGM02
                      '30! FILE STATUS = ' WS-FS-SYS030                 PGM02
                                       DELIMITED BY SIZE                PGM02
                                       INTO WS-MSG                      PGM02
               PERFORM RT-FECHA                                         PGM02
               PERFORM RT-FINALIZA                                      PGM02
           END-IF.                                                      PGM02
      *                                                                 PGM02
       RT-FECHAX.                      EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE CONTABILIZACAO DOS REGISTROS                      *PGM02
      ******************************************************************PGM02
       RT-CONTABILIZA                  SECTION.                         PGM02
      *                                                                 PGM02
           MOVE ACU-LIDOS-SYS010       TO WS-DISPLAY-LG.                PGM02
      *                                                                 PGM02
           DISPLAY '#PGM02.912I - TOTAL DE REGISTROS LIDOS    SYS010 = 'PGM02
                   WS-DISPLAY-LG.                                       PGM02
      *                                                                 PGM02
           MOVE ACU-LIDOS-SYS020       TO WS-DISPLAY-LG.                PGM02
      *                                                                 PGM02
           DISPLAY '#PGM02.913I - TOTAL DE REGISTROS LIDOS    SYS020 = 'PGM02
                   WS-DISPLAY-LG.                                       PGM02
      *                                                                 PGM02
           MOVE ACU-GRAVADOS           TO WS-DISPLAY-LG.                PGM02
      *                                                                 PGM02
           DISPLAY '#PGM02.914I - TOTAL DE REGISTROS GRAVADOS SYS030 = 'PGM02
                   WS-DISPLAY-LG.                                       PGM02
      *                                                                 PGM02
       RT-CONTABILIZAX.                EXIT.                            PGM02
      *                                                                 PGM02
      ******************************************************************PGM02
      *    ROTINA DE FINALIZACAO                                       *PGM02
      ******************************************************************PGM02
       RT-FINALIZA                     SECTION.                         PGM02
      *                                                                 PGM02
           DISPLAY '**************************************************'.PGM02
      *                                                                 PGM02
           MOVE '#PGM02.999I - FIM DO PROCESSAMENTO '                   PGM02
                                       TO WS-MENSAGEM.                  PGM02
      *                                                                 PGM02
           ACCEPT WS-DATA-MAQ          FROM DATE YYYYMMDD.              PGM02
           ACCEPT WS-HORA-MAQ          FROM TIME.                       PGM02
      *                                                                 PGM02
           MOVE WS-DT-ANO-R            TO WS-ANO.                       PGM02
           MOVE WS-DT-MES-R            TO WS-MES.                       PGM02
           MOVE WS-DT-DIA-R            TO WS-DIA.                       PGM02
           MOVE WS-HR-HOR-R            TO WS-HORA.                      PGM02
           MOVE WS-HR-MIN-R            TO WS-MINUTO.                    PGM02
           MOVE WS-HR-SEG-R            TO WS-SEGUNDO.                   PGM02
      *                                                                 PGM02
           DISPLAY WS-MENSAGEM                                          PGM02
                   WS-DATA-DISPLAY.                                     PGM02
           DISPLAY '**************************************************'.PGM02
      *                                                                 PGM02
           MOVE WS-MSG                 TO LK-MSG.                       PGM02
           MOVE WS-COD-RET-R           TO LK-COD-RET.                   PGM02
      *                                                                 PGM02
           STOP '<ENTER> PARA CONTINUAR...'.                            PGM02
      *                                                                 PGM02
           MOVE ZEROS                  TO RETURN-CODE.                  PGM02
      *                                                                 PGM02
           GOBACK.                                                      PGM02
      *                                                                 PGM02
       RT-FINALIZAX.                   EXIT.                            PGM02
      ******************************************************************PGM02
      *    FIM DO PROGRAMA                                             *PGM02
      ******************************************************************PGM02
