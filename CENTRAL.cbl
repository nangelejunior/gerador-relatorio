      ******************************************************************CENTRAL
       IDENTIFICATION                  DIVISION.                        CENTRAL
      ******************************************************************CENTRAL
       PROGRAM-ID. CENTRAL01.                                           CENTRAL01
       AUTHOR.       NEUCLAIR J. ANGELE JR.                             CENTRAL
       DATE-WRITTEN. 08 MAR 2011.                                       CENTRAL
       DATE-COMPILED.                                                   CENTRAL
      ******************************************************************CENTRAL
      *REMARKS.                                                         CENTRAL
      *     *----------------------------------------------------------*CENTRAL
      *     *#NOME     : CENTRAL                                       *CENTRAL
      *     *----------------------------------------------------------*CENTRAL
      *     *#TIPO     : BATCH                                         *CENTRAL
      *     *----------------------------------------------------------*CENTRAL
      *     *#ANALISTA : JOSE L. S. GOMES                              *CENTRAL
      *     *----------------------------------------------------------*CENTRAL
      *     *#FUNCAO   : CHAMAR SUBPROGRAMAS                           *CENTRAL
      *     *----------------------------------------------------------*CENTRAL
      *     * VERSAO 01    - NEUCLAIR J. ANGELE JR.   -     08.03.2011 *CENTRAL
      *     *              - CHAMAR SUBPROGRAMAS CRIADOS DURANTE O     *CENTRAL
      *     *              - TREINAMENTO DA GDSOLUTIONS                *CENTRAL
      *     *----------------------------------------------------------*CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       ENVIRONMENT                     DIVISION.                        CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       CONFIGURATION                   SECTION.                         CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
       SPECIAL-NAMES.                                                   CENTRAL
           DECIMAL-POINT IS COMMA.                                      CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       DATA                            DIVISION.                        CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       WORKING-STORAGE                 SECTION.                         CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       77  FILLER                  PIC     X(32)         VALUE          CENTRAL
           'III WORKING-STORAGE SECTION III'.                           CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    AREA DE CONTADORES                                          *CENTRAL
      ******************************************************************CENTRAL
       77  WS-CT-OPCAO             PIC     9(03) COMP-3  VALUE ZEROS.   CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    AREA DE AUXILIARES                                          *CENTRAL
      ******************************************************************CENTRAL
       01  WS-MSG                  PIC     X(73)         VALUE SPACES.  CENTRAL
       01  WS-OPCAO                PIC     X(02)         VALUE SPACES.  CENTRAL
       01  WS-TECLA                PIC     X(02).                       CENTRAL
           88  WS-BAIXO                                  VALUE '00'.    CENTRAL
           88  WS-ESC                                    VALUE '01'.    CENTRAL
           88  WS-PF12                                   VALUE '93'.    CENTRAL
           88  WS-CIMA                                   VALUE '99'.    CENTRAL
       01  WS-SAIDA.                                                    CENTRAL
           03  WS-SUB-MSG          PIC     X(70)         VALUE SPACES.  CENTRAL
           03  WS-SUB-COD-RET      PIC     9(02)         VALUE ZEROS.   CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       77  FILLER                  PIC     X(32)         VALUE          CENTRAL
           'FFF FIM DA WORKING-STORAGE FFF'.                            CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       SCREEN                          SECTION.                         CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    BORDAS                                                      *CENTRAL
      ******************************************************************CENTRAL
       01  SC-TELA.                                                     CENTRAL
           03  BLANK SCREEN FOREGROUND-COLOR 7 BACKGROUND-COLOR 1.      CENTRAL
           03  SC-BR-SUPERIOR1.                                         CENTRAL
               05  LINE 02 COLUMN 02                     VALUE          CENTRAL
                   'ษอออออออออออออออออออออออออออออออออออออออออออออออออออCENTRAL
      -            'อออออออออออออออออออออออออป'.                        CENTRAL
           03  SC-BR-SUPERIOR2.                                         CENTRAL
               05  LINE 04 COLUMN 02                     VALUE          CENTRAL
                   'ออออออออออออออออออออออออออออออออออออออออออออออออออออCENTRAL
      -            'อออออออออออออออออออออออออ'.                         CENTRAL
           03  SC-BR-ESQUERDA.                                          CENTRAL
               05  LINE 03 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 04 COLUMN 02                     VALUE 'ฬ'.     CENTRAL
               05  LINE 05 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 06 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 07 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 08 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 09 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 10 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 11 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 12 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 13 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 14 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 15 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 16 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 17 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 18 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 19 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 20 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 21 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 22 COLUMN 02                     VALUE 'บ'.     CENTRAL
               05  LINE 23 COLUMN 02                     VALUE 'บ'.     CENTRAL
           03  SC-BR-DIREITA.                                           CENTRAL
               05  LINE 03 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 04 COLUMN 79                     VALUE 'น'.     CENTRAL
               05  LINE 05 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 06 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 07 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 08 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 09 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 10 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 11 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 12 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 13 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 14 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 15 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 16 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 17 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 18 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 19 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 20 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 21 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 22 COLUMN 79                     VALUE 'บ'.     CENTRAL
               05  LINE 23 COLUMN 79                     VALUE 'บ'.     CENTRAL
           03  SC-BR-INFERIOR.                                          CENTRAL
               05  LINE 24 COLUMN 02                     VALUE          CENTRAL
                   'ศอออออออออออออออออออออออออออออออออออออออออออออออออออCENTRAL
      -            'อออออออออออออออออออออออออผ'.                        CENTRAL
           03  SC-TX-TITULO.                                            CENTRAL
               05  LINE 03 COLUMN 22                     VALUE          CENTRAL
                   '* P R O G R A M A   C E N T R A L *'.               CENTRAL
           03  SC-TX-MSG1.                                              CENTRAL
               05  LINE 25 COLUMN 01                     VALUE ' MSG.: 'CENTRAL
                   HIGHLIGHT FOREGROUND-COLOR 6 BACKGROUND-COLOR 4.     CENTRAL
           03  SC-TX-MSG1.                                              CENTRAL
               05  LINE 25 COLUMN 08                                    CENTRAL
                                   PIC X(73)             FROM WS-MSG    CENTRAL
                   HIGHLIGHT FOREGROUND-COLOR 6 BACKGROUND-COLOR 4.     CENTRAL
           03  SC-TX-OPCAO01.                                           CENTRAL
               05  LINE 10 COLUMN 16                     VALUE          CENTRAL
                   'REL. CLIENTES: DATA DO CAB. NO FORMATO DD/MM/AAAA '.CENTRAL
           03  SC-TX-OPCAO02.                                           CENTRAL
               05  LINE 12 COLUMN 16                     VALUE          CENTRAL
                   'REL. CLIENTES: DATA DO CAB. NO FORMATO DD/MMM/AAAA'.CENTRAL
           03  SC-TX-OPCAO03.                                           CENTRAL
               05  LINE 14 COLUMN 16                     VALUE          CENTRAL
                   'VISUALIZAR RELATORIO DE CLIENTES                  '.CENTRAL
           03  SC-TX-OPCAO04.                                           CENTRAL
               05  LINE 16 COLUMN 16                     VALUE          CENTRAL
                   'BALANCE LINE DOS ARQUIVOS SYS010 E SYS020         '.CENTRAL
           03  SC-TX-AUTOR.                                             CENTRAL
               05  LINE 23 COLUMN 04                     VALUE          CENTRAL
                   'NEUCLAIR. J. ANGELE JR.'.                           CENTRAL
           03  SC-TX-SELECIONA.                                         CENTRAL
               05  LINE 23 COLUMN 50                     VALUE          CENTRAL
                   'F12 - SELECIONAR'.                                  CENTRAL
           03  SC-TX-SAIR.                                              CENTRAL
               05  LINE 23 COLUMN 68                     VALUE          CENTRAL
                   'ESC - SAIR'.                                        CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
       PROCEDURE                       DIVISION.                        CENTRAL
      ******************************************************************CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    ROTINA PRINCIPAL                                            *CENTRAL
      ******************************************************************CENTRAL
       RTPRINCIPAL                     SECTION.                         CENTRAL
      *                                                                 CENTRAL
           PERFORM RTINICIALIZA.                                        CENTRAL
      *                                                                 CENTRAL
           PERFORM RTPROCESSA          UNTIL WS-ESC.                    CENTRAL
      *                                                                 CENTRAL
           PERFORM RTFINALIZA.                                          CENTRAL
      *                                                                 CENTRAL
       RTPRINCIPALX.                   EXIT.                            CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    ROTINA DE INICIALIZACAO                                     *CENTRAL
      ******************************************************************CENTRAL
       RTINICIALIZA                    SECTION.                         CENTRAL
      *                                                                 CENTRAL
           DISPLAY SC-TELA.                                             CENTRAL
      *                                                                 CENTRAL
           INITIALIZE WS-SAIDA.                                         CENTRAL
      *                                                                 CENTRAL
           MOVE 1                      TO WS-CT-OPCAO.                  CENTRAL
      *                                                                 CENTRAL
           PERFORM RTSELECIONA.                                         CENTRAL
      *                                                                 CENTRAL
       RTINICIALIZAX.                  EXIT.                            CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    ROTINA DE INICIALIZACAO                                     *CENTRAL
      ******************************************************************CENTRAL
       RTPROCESSA                      SECTION.                         CENTRAL
      *                                                                 CENTRAL
           ACCEPT WS-OPCAO             AT 2580 WITH AUTO UPDATE.        CENTRAL
           ACCEPT WS-TECLA             FROM ESCAPE KEY.                 CENTRAL
      *                                                                 CENTRAL
           MOVE SPACES                 TO WS-MSG.                       CENTRAL
      *                                                                 CENTRAL
           EVALUATE WS-TECLA                                            CENTRAL
               WHEN '00'                                                CENTRAL
                   ADD 1               TO WS-CT-OPCAO                   CENTRAL
                   IF WS-CT-OPCAO      GREATER 4                        CENTRAL
                       MOVE 1          TO WS-CT-OPCAO                   CENTRAL
                   END-IF                                               CENTRAL
                   PERFORM RTSELECIONA                                  CENTRAL
               WHEN '93'                                                CENTRAL
                   PERFORM RTCHAMASUB                                   CENTRAL
               WHEN '99'                                                CENTRAL
                   SUBTRACT 1          FROM WS-CT-OPCAO                 CENTRAL
                   IF WS-CT-OPCAO      LESS 1                           CENTRAL
                       MOVE 4          TO WS-CT-OPCAO                   CENTRAL
                   END-IF                                               CENTRAL
                   PERFORM RTSELECIONA                                  CENTRAL
           END-EVALUATE.                                                CENTRAL
      *                                                                 CENTRAL
       RTPROCESSAX.                    EXIT.                            CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    ROTINA DE SELECAO                                           *CENTRAL
      ******************************************************************CENTRAL
       RTSELECIONA                     SECTION.                         CENTRAL
      *                                                                 CENTRAL
           DISPLAY SC-TELA.                                             CENTRAL
      *                                                                 CENTRAL
           EVALUATE WS-CT-OPCAO                                         CENTRAL
               WHEN 1                                                   CENTRAL
                   DISPLAY '  REL. CLIENTES: DATA DO CAB. NO FORMATO DD/CENTRAL
      -            'MM/AAAA   '                                         CENTRAL
                                       WITH REVERSE-VIDEO AT 1014       CENTRAL
               WHEN 2                                                   CENTRAL
                   DISPLAY '  REL. CLIENTES: DATA DO CAB. NO FORMATO DD/CENTRAL
      -            'MMM/AAAA  '                                         CENTRAL
                                       WITH REVERSE-VIDEO AT 1214       CENTRAL
               WHEN 3                                                   CENTRAL
                   DISPLAY '  VISUALIZAR RELATORIO DE CLIENTES          CENTRAL
      -            '          '        WITH REVERSE-VIDEO AT 1414       CENTRAL
               WHEN 4                                                   CENTRAL
                   DISPLAY '  BALANCE LINE DOS ARQUIVOS SYS010 E SYS020 CENTRAL
      -            '          '        WITH REVERSE-VIDEO AT 1614       CENTRAL
           END-EVALUATE.                                                CENTRAL
      *                                                                 CENTRAL
       RTSELECIONAX.                   EXIT.                            CENTRAL
      *                                                                 CENTRAL
      ******************************************************************CENTRAL
      *    ROTINA DE CHAMADA DOS SUBPROGRAMAS                          *CENTRAL
      ******************************************************************CENTRAL
       RTCHAMASUB                      SECTION.                         CENTRAL
      *                                                                 CENTRAL
           INITIALIZE WS-SAIDA.                                         CENTRAL
      *                                                                 CENTRAL
           EVALUATE WS-CT-OPCAO                                         CENTRAL
               WHEN 1                                                   CENTRAL
                   CALL 'PGM01.INT'    USING WS-SUB-MSG                 CENTRAL
                                             WS-SUB-COD-RET             CENTRAL
                   IF WS-SUB-COD-RET   EQUAL ZEROS                      CENTRAL
                       MOVE 'REL. GERADO! ARQUIVO: SPRINT.'             CENTRAL
                                       TO WS-MSG                        CENTRAL
                   ELSE                                                 CENTRAL
                       MOVE WS-SUB-MSG TO WS-MSG                        CENTRAL
                   END-IF                                               CENTRAL
               WHEN 2                                                   CENTRAL
                   CALL 'PGM0102.INT'  USING WS-SUB-MSG                 CENTRAL
                                             WS-SUB-COD-RET             CENTRAL
                   IF WS-SUB-COD-RET   EQUAL ZEROS                      CENTRAL
                       MOVE 'REL. GERADO! ARQUIVO: SPRINT.'             CENTRAL
                                       TO WS-MSG                        CENTRAL
                   ELSE                                                 CENTRAL
                       MOVE WS-SUB-MSG TO WS-MSG                        CENTRAL
                   END-IF                                               CENTRAL
               WHEN 3                                                   CENTRAL
                   CALL 'VWRLT.INT'    USING WS-SUB-MSG                 CENTRAL
                                             WS-SUB-COD-RET             CENTRAL
                   IF WS-SUB-COD-RET   EQUAL ZEROS                      CENTRAL
                       CONTINUE                                         CENTRAL
                   ELSE                                                 CENTRAL
                       MOVE WS-SUB-MSG TO WS-MSG                        CENTRAL
                   END-IF                                               CENTRAL
               WHEN 4                                                   CENTRAL
                   CALL 'PGM02.INT'    USING WS-SUB-MSG                 CENTRAL
                                             WS-SUB-COD-RET             CENTRAL
                   IF WS-SUB-COD-RET   EQUAL ZEROS                      CENTRAL
                       MOVE 'BALANCE LINE EXECUTADO! ARQUIVO: SYS030.'  CENTRAL
                                       TO WS-MSG                        CENTRAL
                   ELSE                                                 CENTRAL
                       MOVE WS-SUB-MSG TO WS-MSG                        CENTRAL
                   END-IF                                               CENTRAL
           END-EVALUATE.                                                CENTRAL
      *                                                                 CENTRAL
           PERFORM RTSELECIONA.                                         CENTRAL
      *                                                                 CENTRAL
       RTCHAMASUBX.                    EXIT.                            CENTRAL
      ******************************************************************CENTRAL
      *    ROTINA DE FINALIZACAO                                       *CENTRAL
      ******************************************************************CENTRAL
       RTFINALIZA                      SECTION.                         CENTRAL
      *                                                                 CENTRAL
           STOP RUN.                                                    CENTRAL
      *                                                                 CENTRAL
       RTFINALIZAX.                    EXIT.                            CENTRAL
      ******************************************************************CENTRAL
      *    FIM DO PROGRAMA                                             *CENTRAL
      ******************************************************************CENTRAL
