       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-PROCESSOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Mapping du nom logique (TRANSACTION-FILE) vers le dataset physique
           SELECT TRANSACTION-FILE ASSIGN TO 'transactions.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * FD (File Descriptor) : Définition du buffer d'entrée
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TR-ACCOUNT-NUM     PIC 9(10).
           05  TR-DATE            PIC 9(8).
           05  TR-TYPE            PIC X(1).
      * V = Point décimal implicite (pas de stockage physique du point)
           05  TR-AMOUNT          PIC 9(5)V99.

       WORKING-STORAGE SECTION.
      * Variables de contrôle de flux (Switch/Flags)
       01  WS-FLAGS.
           05  WS-EOF-FLAG        PIC X(1) VALUE 'N'.
               88  END-OF-FILE    VALUE 'Y'.

      * Stockage interne en COMP-3 (Packed-Decimal) pour l'arithmétique
       01  WS-ACCUMULATORS.
           05  WS-TOTAL-CREDITS   PIC 9(7)V99  COMP-3 VALUE ZERO.
           05  WS-TOTAL-DEBITS    PIC 9(7)V99  COMP-3 VALUE ZERO.
           05  WS-CURRENT-BALANCE PIC S9(7)V99 COMP-3 VALUE ZERO.

      * Editing Masks (Masques d'édition) pour le reporting (Numeric-Edited)
       01  WS-REPORTS-FIELDS.
           05  WS-DISP-CREDITS    PIC 9(7).99.
           05  WS-DISP-DEBITS     PIC 9(7).99.
           05  WS-DISP-BALANCE    PIC -9(7).99.

       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           OPEN INPUT TRANSACTION-FILE.

      * Main Loop : Itération séquentielle jusqu'à la levée du flag EOF
           PERFORM UNTIL END-OF-FILE
               READ TRANSACTION-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 1000-PROCESS-RECORD
               END-READ
           END-PERFORM.

           CLOSE TRANSACTION-FILE.
           PERFORM 9000-GENERATE-REPORT.
           STOP RUN.

       1000-PROCESS-RECORD.
      * Logique de traitement des flux selon le type de transaction
           EVALUATE TR-TYPE
               WHEN 'C'
                   ADD TR-AMOUNT TO WS-TOTAL-CREDITS
               WHEN 'D'
                   ADD TR-AMOUNT TO WS-TOTAL-DEBITS
               WHEN OTHER
                   DISPLAY "ERREUR : TYPE INCONNU SUR COMPTE " 
                           TR-ACCOUNT-NUM
           END-EVALUATE.

       9000-GENERATE-REPORT.
      * Arithmétique finale et transfert vers les zones de sortie (Move & Edit)
           COMPUTE WS-CURRENT-BALANCE = WS-TOTAL-CREDITS 
                                      - WS-TOTAL-DEBITS.
           
           MOVE WS-TOTAL-CREDITS   TO WS-DISP-CREDITS.
           MOVE WS-TOTAL-DEBITS    TO WS-DISP-DEBITS.
           MOVE WS-CURRENT-BALANCE TO WS-DISP-BALANCE.

           DISPLAY "===================================".
           DISPLAY "     RAPPORT BATCH JOURNALIER      ".
           DISPLAY "===================================".
           DISPLAY "TOTAL CREDITS : + " WS-DISP-CREDITS.
           DISPLAY "TOTAL DEBITS  : - " WS-DISP-DEBITS.
           DISPLAY "-----------------------------------".
           DISPLAY "SOLDE GLOBAL  :   " WS-DISP-BALANCE.
           DISPLAY "===================================".
           