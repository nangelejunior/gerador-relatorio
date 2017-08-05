      ******************************************************************PGM01
       IDENTIFICATION                  DIVISION.                        PGM01
      ******************************************************************PGM01
       PROGRAM-ID. PGM0102.                                             PGM0102
       AUTHOR.       NEUCLAIR J. ANGELE JR.                             PGM01
       DATE-WRITTEN. 08 MAR 2011.                                       PGM01
       DATE-COMPILED.                                                   PGM01
      ******************************************************************PGM01
      *REMARKS.                                                         PGM01
      *     *----------------------------------------------------------*PGM01
      *     *#NOME     : PGM01                                         *PGM01
      *     *----------------------------------------------------------*PGM01
      *     *#TIPO     : BATCH                                         *PGM01
      *     *----------------------------------------------------------*PGM01
      *     *#ANALISTA : JOSE L. S. GOMES                              *PGM01
      *     *----------------------------------------------------------*PGM01
      *     *#FUNCAO   : GERAR RELATORIO DE CLIENTES                   *PGM01
      *     *----------------------------------------------------------*PGM01
      *     * VERSAO 01    - NEUCLAIR J. ANGELE JR    -     08.03.2011 *PGM01
      *     *              - GERAR RELATORIO DE CLINTES COM TELEFONES  *PGM01
      *     *----------------------------------------------------------*PGM01
      *     * VERSAO 02    - NEUCLAIR J ANGELE JR.    -     08.03.2011 *PGM0102
      *     *              - ALTERAR O FORMATO DA DATA DO RELATORIO    *PGM0102
      *     *----------------------------------------------------------*PGM0102
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       ENVIRONMENT                     DIVISION.                        PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       CONFIGURATION                   SECTION.                         PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
       SPECIAL-NAMES.                                                   PGM01
           DECIMAL-POINT IS COMMA.                                      PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       INPUT-OUTPUT                    SECTION.                         PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
       FILE-CONTROL.                                                    PGM01
           SELECT ECADCLI ASSIGN TO SYS010                              PGM01
                  FILE    STATUS IS WS-FS-ECADCLI.                      PGM01
      *                                                                 PGM01
           SELECT SRELATO ASSIGN TO SPRINT                              PGM01
                  FILE    STATUS IS WS-FS-SRELATO.                      PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       DATA                            DIVISION.                        PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       FILE                            SECTION.                         PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
       FD  ECADCLI                                                      PGM01
           BLOCK     CONTAINS 0  RECORDS                                PGM01
           RECORDING MODE     IS F                                      PGM01
           LABEL     RECORD   IS STANDARD.                              PGM01
       01  FD-REG-CADCLI.                                               PGM01
           03  FD-RC-CODIGO        PIC     9(06).                       PGM01
           03  FD-RC-NOME          PIC     X(40).                       PGM01
           03  FD-RC-ENDERECO.                                          PGM01
               05  FD-RC-RUA       PIC     X(30).                       PGM01
               05  FD-RC-NUMERO    PIC     X(05).                       PGM01
               05  FD-RC-COMPLEMENTO                                    PGM01
                                   PIC     X(15).                       PGM01
           03  FD-RC-CEP           PIC     9(09).                       PGM01
           03  FD-RC-BAIRRO        PIC     X(20).                       PGM01
           03  FD-RC-CIDADE        PIC     X(20).                       PGM01
           03  FD-RC-ESTADO        PIC     X(02).                       PGM01
           03  FD-RC-TEL-RESIDENCIAL.                                   PGM01
               05  FD-RC-DDD-RES   PIC     9(03).                       PGM01
               05  FD-RC-NUM-RES   PIC     9(08).                       PGM01
           03  FD-RC-TEL-CELULAR.                                       PGM01
               05  FD-RC-DDD-CEL   PIC     9(03).                       PGM01
               05  FD-RC-NUM-CEL   PIC     9(08).                       PGM01
           03  FD-RC-TEL-COMERCIAL.                                     PGM01
               05  FD-RC-DDD-COM   PIC     9(03).                       PGM01
               05  FD-RC-NUM-COM   PIC     9(08).                       PGM01
           03  FD-RC-PESSOA        PIC     9(01).                       PGM01
           03  FD-RC-CPF-CNPJ      PIC     9(15).                       PGM01
      *                                                                 PGM01
       FD  SRELATO.                                                     PGM01
       01  FD-REG-RELATO           PIC     X(95).                       PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       WORKING-STORAGE                 SECTION.                         PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       77  FILLER                  PIC     X(32)         VALUE          PGM01
           'III WORKING-STORAGE SECTION III'.                           PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    AREA DE FILE STATUS                                         *PGM01
      ******************************************************************PGM01
       77  WS-FS-ECADCLI           PIC     X(02)         VALUE ZEROS.   PGM01
       77  WS-FS-SRELATO           PIC     X(02)         VALUE ZEROS.   PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    AREA DE CONTADORES                                          *PGM01
      ******************************************************************PGM01
       77  ACU-CT-LIDOS            PIC     9(07) COMP-3  VALUE ZEROS.   PGM01
       77  ACU-CT-LINHA            PIC     9(02) COMP-3  VALUE ZEROS.   PGM01
       77  ACU-CT-PAGINA           PIC     9(06) COMP-3  VALUE ZEROS.   PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    AREA DE AUXILIARES                                          *PGM01
      ******************************************************************PGM01
       01  WS-MSG-ECADCLI          PIC     X(18)         VALUE          PGM01
           ' DO ARQUIVO CADCLI'.                                        PGM01
       01  WS-MSG-SRELATO          PIC     X(18)         VALUE          PGM01
           ' DO ARQUIVO RELATO'.                                        PGM01
      *                                                                 PGM01
       01  WS-DISPLAY-LIDOS        PIC     9(06)         VALUE ZEROS.   PGM01
      *                                                                 PGM01
       01  WS-DATA-MAQ             PIC     9(08)         VALUE ZEROS.   PGM01
      *                                                                 PGM01
       01  WS-DATA-MAQ-R REDEFINES WS-DATA-MAQ.                         PGM01
           03  WS-DT-ANO-R         PIC     9(04).                       PGM01
           03  WS-DT-MES-R         PIC     9(02).                       PGM01
           03  WS-DT-DIA-R         PIC     9(02).                       PGM01
      *                                                                 PGM01
       01  WS-HORA-MAQ             PIC     9(08)         VALUE ZEROS.   PGM01
                                                                        PGM01
       01  WS-HORA-MAQ-R REDEFINES WS-HORA-MAQ.                         PGM01
           03  WS-HR-HOR-R         PIC     9(02).                       PGM01
           03  WS-HR-MIN-R         PIC     9(02).                       PGM01
           03  WS-HR-SEG-R         PIC     9(02).                       PGM01
           03  WS-HR-MIL-R         PIC     9(02).                       PGM01
      *                                                                 PGM01
       01  WS-MENSAGEM             PIC     X(40)         VALUE SPACES.  PGM01
       01  WS-DATA-DISPLAY.                                             PGM01
           03  WS-DATA-BR.                                              PGM01
               05  WS-DIA          PIC     9(02)         VALUE ZEROS.   PGM01
               05  FILLER          PIC     X             VALUE '/'.     PGM01
               05  WS-MES          PIC     9(02)         VALUE ZEROS.   PGM01
               05  FILLER          PIC     X             VALUE '/'.     PGM01
               05  WS-ANO          PIC     9(04)         VALUE ZEROS.   PGM01
               05  FILLER          PIC     X             VALUE ' '.     PGM01
           03  WS-HORA-BR.                                              PGM01
               05  WS-HORA         PIC     9(02)         VALUE ZEROS.   PGM01
               05  FILLER          PIC     X             VALUE ':'.     PGM01
               05  WS-MINUTO       PIC     9(02)         VALUE ZEROS.   PGM01
               05  FILLER          PIC     X             VALUE ':'.     PGM01
               05  WS-SEGUNDO      PIC     9(02)         VALUE ZEROS.   PGM01
      *                                                                 PGM01
       01  CAB1.                                                        PGM01
           05  CB1-DIA             PIC     9(02)         VALUE ZEROS.   PGM01
           05  FILLER              PIC     X(01)         VALUE '/'.     PGM01
           05  CB1-MES             PIC     X(03)         VALUE SPACES.  PGM0102
           05  FILLER              PIC     X(01)         VALUE '/'.     PGM01
           05  CB1-ANO             PIC     9(04)         VALUE ZEROS.   PGM01
           05  FILLER              PIC     X(11)         VALUE SPACES.  PGM0102
           05  FILLER              PIC     X(63)         VALUE          PGM01
               'RELATORIO DE CLIENTES COM TELEFONES'.                   PGM01
           05  FILLER              PIC     X(06)         VALUE 'PAG.:'. PGM01
           05  CB1-PAGINA          PIC  ZZZ9.                           PGM01
      *                                                                 PGM01
       01  TABELA-MES.                                                  PGM0102
           05 FILLER               PIC     X(36)         VALUE          PGM0102
              'JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ'.                   PGM0102
      *                                                                 PGM0102
       01  MES-TABELA REDEFINES TABELA-MES.                             PGM0102
           05  TB-MES              PIC     X(03) OCCURS 12 TIMES.       PGM0102
      *                                                                 PGM0102
       01  CAB2.                                                        PGM01
           05  FILLER              PIC     X(95)         VALUE SPACES.  PGM01
      *                                                                 PGM01
       01  CAB3.                                                        PGM01
           05  FILLER              PIC     X(22)         VALUE          PGM01
               'COD CLI'.                                               PGM01
           05  FILLER              PIC     X(31)         VALUE          PGM01
               'NOME DO CLIENTE'.                                       PGM01
           05  FILLER              PIC     X(14)         VALUE          PGM01
               'TEL.RES.'.                                              PGM01
           05  FILLER              PIC     X(17)         VALUE          PGM01
               'TEL.CEL.'.                                              PGM01
           05  FILLER              PIC     X(11)         VALUE          PGM01
               'TEL.COM.'.                                              PGM01
      *                                                                 PGM01
       01  CAB4.                                                        PGM01
           05  FILLER              PIC     X(51)         VALUE SPACES.  PGM01
           05  FILLER              PIC     X(06)         VALUE 'DDD'.   PGM01
           05  FILLER              PIC     X(10)         VALUE 'NUM.'.  PGM01
           05  FILLER              PIC     X(06)         VALUE 'DDD'.   PGM01
           05  FILLER              PIC     X(10)         VALUE 'NUM.'.  PGM01
           05  FILLER              PIC     X(06)         VALUE 'DDD'.   PGM01
           05  FILLER              PIC     X(07)         VALUE 'NUM.'.  PGM01
      *                                                                 PGM01
       01  CAB5.                                                        PGM01
           05  FILLER              PIC     X(95)         VALUE ALL '-'. PGM01
      *                                                                 PGM01
       01  LINDEF1.                                                     PGM01
           05  FILLER              PIC     X(01)         VALUE SPACES.  PGM01
           05  LD1-CODIGO          PIC     999.999       VALUE ZEROS.   PGM01
           05  FILLER              PIC     X(02)         VALUE SPACES.  PGM01
           05  LD1-NOME            PIC     X(40)         VALUE SPACES.  PGM01
           05  FILLER              PIC     X(01)         VALUE SPACES.  PGM01
           05  LD1-DDD-RES         PIC     999           VALUE ZEROS.   PGM01
           05  FILLER              PIC     X(01)         VALUE SPACE.   PGM01
           05  LD1-NUM-RES         PIC     9999.9999     VALUE ZEROS.   PGM01
           05  FILLER              PIC     X(02)         VALUE SPACES.  PGM01
           05  LD1-DDD-CEL         PIC     999           VALUE ZEROS.   PGM01
           05  FILLER              PIC     X(01)         VALUE SPACES.  PGM01
           05  LD1-NUM-CEL         PIC     9999.9999     VALUE ZERO.    PGM01
           05  FILLER              PIC     X(03)         VALUE SPACES.  PGM01
           05  LD1-DDD-COM         PIC     999           VALUE ZEROS.   PGM01
           05  FILLER              PIC     X(01)         VALUE SPACE.   PGM01
           05  LD1-NUM-COM         PIC     9999.9999     VALUE ZEROS.   PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       77  FILLER                  PIC     X(32)         VALUE          PGM01
           'FFF FIM DA WORKING-STORAGE FFF'.                            PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    AREA DE LINKAGE                                             *PGM01
      ******************************************************************PGM01
       01  WS-SAIDA.                                                    PGM01
           03  WS-MSG              PIC     X(70)         VALUE SPACES.  PGM01
           03  WS-COD-RET          PIC     X(02)         VALUE ZEROS.   PGM01
           03  WS-COD-RET-R        PIC     9(02) REDEFINES WS-COD-RET.  PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       LINKAGE                         SECTION.                         PGM01
      ******************************************************************PGM01
       01  LK-MSG                  PIC     X(70).                       PGM01
       01  LK-COD-RET              PIC     9(02).                       PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
       PROCEDURE                       DIVISION USING LK-MSG            PGM01
                                                      LK-COD-RET.       PGM01
      ******************************************************************PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA PRINCIPAL                                            *PGM01
      ******************************************************************PGM01
       RTPRINCIPAL                     SECTION.                         PGM01
      *                                                                 PGM01
           PERFORM RTINICIALIZA.                                        PGM01
      *                                                                 PGM01
           PERFORM RTPROCESSA.                                          PGM01
      *                                                                 PGM01
           PERFORM RTFINALIZA.                                          PGM01
      *                                                                 PGM01
       RTPRINCIPALX.                   EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE INICIALIZACAO                                     *PGM01
      ******************************************************************PGM01
       RTINICIALIZA                    SECTION.                         PGM01
      *                                                                 PGM01
           DISPLAY ERASE.                                               PGM01
      *                                                                 PGM01
           INITIALIZE ACU-CT-LIDOS                                      PGM01
                      ACU-CT-LINHA                                      PGM01
                      ACU-CT-PAGINA                                     PGM01
                      CAB1                                              PGM01
                      CAB2                                              PGM01
                      CAB3                                              PGM01
                      CAB4                                              PGM01
                      CAB5                                              PGM01
                      LINDEF1.                                          PGM01
      *                                                                 PGM01
           DISPLAY '**************************************************'.PGM01
      *                                                                 PGM01
           MOVE '#PGM01.900I - INICIO DO PROCESSAMENTO '                PGM01
                                       TO WS-MENSAGEM.                  PGM01
      *                                                                 PGM01
           ACCEPT WS-DATA-MAQ          FROM DATE YYYYMMDD.              PGM01
           ACCEPT WS-HORA-MAQ          FROM TIME.                       PGM01
      *                                                                 PGM01
           MOVE WS-DT-ANO-R            TO WS-ANO.                       PGM01
           MOVE WS-DT-MES-R            TO WS-MES.                       PGM01
           MOVE WS-DT-DIA-R            TO WS-DIA.                       PGM01
           MOVE WS-HR-HOR-R            TO WS-HORA.                      PGM01
           MOVE WS-HR-MIN-R            TO WS-MINUTO.                    PGM01
           MOVE WS-HR-SEG-R            TO WS-SEGUNDO.                   PGM01
      *                                                                 PGM01
           DISPLAY WS-MENSAGEM                                          PGM01
                   WS-DATA-DISPLAY.                                     PGM01
           DISPLAY '**************************************************'.PGM01
      *                                                                 PGM01
           MOVE SPACES                 TO WS-MSG.                       PGM01
           MOVE ZEROS                  TO WS-COD-RET.                   PGM01
           MOVE 55                     TO ACU-CT-LINHA.                 PGM01
      *                                                                 PGM01
           OPEN INPUT  ECADCLI                                          PGM01
                OUTPUT SRELATO.                                         PGM01
      *                                                                 PGM01
           IF WS-FS-ECADCLI            EQUAL ZEROS                      PGM01
               CONTINUE                                                 PGM01
           ELSE                                                         PGM01
               DISPLAY '#PGM01.901I - ERRO NA ABERTURA' WS-MSG-ECADCLI  PGM01
               DISPLAY '#PGM01.901I - FILE STATUS = '   WS-FS-ECADCLI   PGM01
               MOVE   WS-FS-ECADCLI    TO WS-COD-RET                    PGM01
               STRING '#PGM01.901I - ERRO NA ABERTURA DO ARQUIVO CADCLI'PGM01
                      '! FILE STATUS = ' WS-FS-ECADCLI                  PGM01
                                       DELIMITED BY SIZE                PGM01
                                       INTO WS-MSG                      PGM01
               PERFORM RTFINALIZA                                       PGM01
           END-IF.                                                      PGM01
      *                                                                 PGM01
           IF WS-FS-SRELATO            EQUAL ZEROS                      PGM01
               CONTINUE                                                 PGM01
           ELSE                                                         PGM01
               DISPLAY '#PGM01.902I - ERRO NA ABERTURA' WS-MSG-SRELATO  PGM01
               DISPLAY '#PGM01.902I - FILE STATUS = '   WS-FS-SRELATO   PGM01
               MOVE   WS-FS-SRELATO    TO WS-COD-RET                    PGM01
               STRING '#PGM01.902I - ERRO NA ABERTURA DO ARQUIVO RELATO'PGM01
                      '! FILE STATUS = ' WS-FS-SRELATO                  PGM01
                                       DELIMITED BY SIZE                PGM01
                                       INTO WS-MSG                      PGM01
               PERFORM RTFINALIZA                                       PGM01
           END-IF.                                                      PGM01
      *                                                                 PGM01
           PERFORM RTLECADCLI.                                          PGM01
      *                                                                 PGM01
           IF WS-FS-ECADCLI            EQUAL '10'                       PGM01
               DISPLAY '#PGM01.903I - ARQUIVO ECADCLI VAZIO'            PGM01
               DISPLAY '#PGM01.903I - FILE STATUS = '   WS-FS-ECADCLI   PGM01
               MOVE   WS-FS-ECADCLI    TO WS-COD-RET                    PGM01
               STRING '#PGM01.903I - ARQUIVO CADCLI VAZIO! '            PGM01
                      'FILE STATUS = ' WS-FS-ECADCLI                    PGM01
                                       DELIMITED BY SIZE                PGM01
                                       INTO WS-MSG                      PGM01
               PERFORM RTFECHA                                          PGM01
               PERFORM RTFINALIZA                                       PGM01
           END-IF.                                                      PGM01
      *                                                                 PGM01
       RTINICIALIZAX.                  EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE LEITURA DO ARQUIVO ECADCLI                        *PGM01
      ******************************************************************PGM01
       RTLECADCLI                      SECTION.                         PGM01
      *                                                                 PGM01
           READ ECADCLI.                                                PGM01
      *                                                                 PGM01
           EVALUATE WS-FS-ECADCLI                                       PGM01
               WHEN ZEROS                                               PGM01
                   ADD 1               TO ACU-CT-LIDOS                  PGM01
               WHEN '10'                                                PGM01
                   CONTINUE                                             PGM01
               WHEN OTHER                                               PGM01
                   DISPLAY '#PGM01.904I - ERRO NA LEITURA'              PGM01
                           WS-MSG-ECADCLI                               PGM01
                   DISPLAY '#PGM01.904I - FILE STATUS = '               PGM01
                           WS-FS-ECADCLI                                PGM01
                   MOVE  WS-FS-ECADCLI TO WS-COD-RET                    PGM01
                   STRING '#PGM01.904I - ERRO NA LEITURA DO ARQUIVO CAD'PGM01
                          'CLI! FILE STATUS = ' WS-FS-ECADCLI           PGM01
                                       DELIMITED BY SIZE                PGM01
                                       INTO WS-MSG                      PGM01
                   PERFORM RTFECHA                                      PGM01
                   PERFORM RTFINALIZA                                   PGM01
           END-EVALUATE.                                                PGM01
      *                                                                 PGM01
       RTLECADCLIX.                    EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE INICIALIZACAO                                     *PGM01
      ******************************************************************PGM01
       RTPROCESSA                      SECTION.                         PGM01
      *                                                                 PGM01
           PERFORM RTMONTAREL          UNTIL WS-FS-ECADCLI EQUAL '10'.  PGM01
      *                                                                 PGM01
           PERFORM RTFECHA.                                             PGM01
      *                                                                 PGM01
           PERFORM RTCONTABILIZA.                                       PGM01
      *                                                                 PGM01
       RTPROCESSAX.                    EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE MONTAGEM DO RELATORIO                             *PGM01
      ******************************************************************PGM01
       RTMONTAREL                      SECTION.                         PGM01
      *                                                                 PGM01
           MOVE FD-RC-CODIGO           TO LD1-CODIGO.                   PGM01
           MOVE FD-RC-NOME             TO LD1-NOME.                     PGM01
           MOVE FD-RC-DDD-RES          TO LD1-DDD-RES.                  PGM01
           MOVE FD-RC-NUM-RES          TO LD1-NUM-RES.                  PGM01
           MOVE FD-RC-DDD-CEL          TO LD1-DDD-CEL.                  PGM01
           MOVE FD-RC-NUM-CEL          TO LD1-NUM-CEL.                  PGM01
           MOVE FD-RC-DDD-COM          TO LD1-DDD-COM.                  PGM01
           MOVE FD-RC-NUM-COM          TO LD1-NUM-COM.                  PGM01
                                                                        PGM01
           PERFORM RTIMPREL.                                            PGM01
           PERFORM RTLECADCLI.                                          PGM01
      *                                                                 PGM01
       RTMONTARELX.                    EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE IMPRESSAO DO RELATORIO                            *PGM01
      ******************************************************************PGM01
       RTIMPREL                        SECTION.                         PGM01
      *                                                                 PGM01
           IF ACU-CT-LINHA             EQUAL 55                         PGM01
               PERFORM RTMONTACAB                                       PGM01
           END-IF.                                                      PGM01
      *                                                                 PGM01
           MOVE LINDEF1                TO FD-REG-RELATO.                PGM01
      *                                                                 PGM01
           WRITE FD-REG-RELATO         AFTER 1 LINE.                    PGM01
      *                                                                 PGM01
           ADD 1                       TO ACU-CT-LINHA.                 PGM01
      *                                                                 PGM01
       RTIMPRELX.                      EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE MONTAGEM DO CABECALHO                             *PGM01
      ******************************************************************PGM01
       RTMONTACAB                      SECTION.                         PGM01
      *                                                                 PGM01
           MOVE  WS-DIA                TO CB1-DIA.                      PGM01
           MOVE  TB-MES(WS-MES)        TO CB1-MES.                      PGM0102
           MOVE  WS-ANO                TO CB1-ANO.                      PGM01
      *                                                                 PGM01
           ADD 1                       TO ACU-CT-PAGINA.                PGM01
      *                                                                 PGM01
           MOVE ACU-CT-PAGINA          TO CB1-PAGINA.                   PGM01
           MOVE CAB1                   TO FD-REG-RELATO.                PGM01
      *                                                                 PGM01
           IF ACU-CT-PAGINA            EQUAL 1                          PGM01
               WRITE FD-REG-RELATO     AFTER 0 LINE                     PGM01
           ELSE                                                         PGM01
               WRITE FD-REG-RELATO     AFTER PAGE                       PGM01
           END-IF.                                                      PGM01
      *                                                                 PGM01
           MOVE CAB2                   TO FD-REG-RELATO.                PGM01
      *                                                                 PGM01
           WRITE FD-REG-RELATO         AFTER 1 LINE.                    PGM01
      *                                                                 PGM01
           MOVE CAB3                   TO FD-REG-RELATO.                PGM01
      *                                                                 PGM01
           WRITE FD-REG-RELATO         AFTER 1 LINE.                    PGM01
      *                                                                 PGM01
           MOVE CAB4                   TO FD-REG-RELATO.                PGM01
      *                                                                 PGM01
           WRITE FD-REG-RELATO         AFTER 1 LINE.                    PGM01
      *                                                                 PGM01
           MOVE CAB5                   TO FD-REG-RELATO.                PGM01
      *                                                                 PGM01
           WRITE FD-REG-RELATO         AFTER 1 LINE.                    PGM01
      *                                                                 PGM01
           MOVE 5                      TO ACU-CT-LINHA.                 PGM01
      *                                                                 PGM01
       RTMONTACABX.                    EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE FECHAMENTO DOS ARQUIVOS                           *PGM01
      ******************************************************************PGM01
       RTFECHA                         SECTION.                         PGM01
      *                                                                 PGM01
           CLOSE ECADCLI                                                PGM01
                 SRELATO.                                               PGM01
      *                                                                 PGM01
           IF WS-FS-ECADCLI            EQUAL ZEROS                      PGM01
               CONTINUE                                                 PGM01
           ELSE                                                         PGM01
               DISPLAY '#PGM01.905I - ERRO NO FECHAMENTO' WS-MSG-ECADCLIPGM01
               DISPLAY '#PGM01.905I - FILE STATUS = '     WS-FS-ECADCLI PGM01
               MOVE   WS-FS-ECADCLI    TO WS-COD-RET                    PGM01
               STRING '#PGM01.905I - ERRO NO FECHAMENTO DO ARQUIVO CADC'PGM01
                      'LI! FILE STATUS = ' WS-FS-ECADCLI                PGM01
                                       DELIMITED BY SIZE                PGM01
                                       INTO WS-MSG                      PGM01
               PERFORM RTFINALIZA                                       PGM01
           END-IF.                                                      PGM01
      *                                                                 PGM01
           IF WS-FS-SRELATO            EQUAL ZEROS                      PGM01
               CONTINUE                                                 PGM01
           ELSE                                                         PGM01
               DISPLAY '#PGM01.906I - ERRO NO FECHAMENTO' WS-MSG-SRELATOPGM01
               DISPLAY '#PGM01.906I - FILE STATUS = '     WS-FS-SRELATO PGM01
               MOVE   WS-FS-SRELATO    TO WS-COD-RET                    PGM01
               STRING '#PGM01.906I - ERRO NO FECHAMENTO DO ARQUIVO RELA'PGM01
                      'TO! FILE STATUS = ' WS-FS-SRELATO                PGM01
                                       DELIMITED BY SIZE                PGM01
                                       INTO WS-MSG                      PGM01
               PERFORM RTFINALIZA                                       PGM01
           END-IF.                                                      PGM01
      *                                                                 PGM01
       RTFECHAX.                       EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA QUE CONTABILIZA OS REGISTROS                         *PGM01
      ******************************************************************PGM01
       RTCONTABILIZA                   SECTION.                         PGM01
      *                                                                 PGM01
           MOVE ACU-CT-LIDOS           TO WS-DISPLAY-LIDOS.             PGM01
      *                                                                 PGM01
           DISPLAY '#PGM01.907I - TOTAL DE REGISTROS LIDOS = '          PGM01
                   WS-DISPLAY-LIDOS.                                    PGM01
      *                                                                 PGM01
       RTCONTABILIZAX.                 EXIT.                            PGM01
      *                                                                 PGM01
      ******************************************************************PGM01
      *    ROTINA DE FINALIZACAO                                       *PGM01
      ******************************************************************PGM01
       RTFINALIZA                      SECTION.                         PGM01
      *                                                                 PGM01
           DISPLAY '**************************************************'.PGM01
      *                                                                 PGM01
           MOVE '#PGM01.999I - FIM DO PROCESSAMENTO '                   PGM01
                                       TO WS-MENSAGEM.                  PGM01
      *                                                                 PGM01
           ACCEPT WS-DATA-MAQ          FROM DATE YYYYMMDD.              PGM01
           ACCEPT WS-HORA-MAQ          FROM TIME.                       PGM01
      *                                                                 PGM01
           MOVE WS-DT-ANO-R            TO WS-ANO.                       PGM01
           MOVE WS-DT-MES-R            TO WS-MES.                       PGM01
           MOVE WS-DT-DIA-R            TO WS-DIA.                       PGM01
           MOVE WS-HR-HOR-R            TO WS-HORA.                      PGM01
           MOVE WS-HR-MIN-R            TO WS-MINUTO.                    PGM01
           MOVE WS-HR-SEG-R            TO WS-SEGUNDO.                   PGM01
      *                                                                 PGM01
           DISPLAY WS-MENSAGEM                                          PGM01
                   WS-DATA-DISPLAY.                                     PGM01
           DISPLAY '**************************************************'.PGM01
      *                                                                 PGM01
           MOVE WS-MSG                 TO LK-MSG.                       PGM01
           MOVE WS-COD-RET-R           TO LK-COD-RET.                   PGM01
      *                                                                 PGM01
           STOP '<ENTER> PARA CONTINUAR...'.                            PGM01
      *                                                                 PGM01
           MOVE ZEROS                  TO RETURN-CODE.                  PGM01
      *                                                                 PGM01
           GOBACK.                                                      PGM01
      *                                                                 PGM01
       RTFINALIZAX.                    EXIT.                            PGM01
      ******************************************************************PGM01
      *    FIM DO PROGRAMA                                             *PGM01
      ******************************************************************PGM01
