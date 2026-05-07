IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-QUERY.
       AUTHOR. MICHAEL MISI.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Déclaration des variables hôtes pour recevoir les données SQL
       01  WS-CUSTOMER-REC.
           05 WS-CUST-ID          PIC X(10).
           05 WS-CUST-NAME        PIC X(30).
           05 WS-CUST-STATUS      PIC X(1).

      * Variables de gestion d'erreur SQL
       01  WS-SQL-STATUS          PIC S9(9) COMP.

      * Inclusion de la zone de communication SQL (SQLCA)
           EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY "--- RECHERCHE CLIENT DANS DB2 ---".

      * Requête SQL intégrée au COBOL
           EXEC SQL
               SELECT CUSTOMER_NAME, CUSTOMER_STATUS
               INTO :WS-CUST-NAME, :WS-CUST-STATUS
               FROM BANK_CUSTOMERS
               WHERE CUSTOMER_ID = '1234567890'
           END-EXEC.

      * Vérification du code de retour SQL (SQLCODE)
           EVALUATE SQLCODE
               WHEN 0
                   DISPLAY "CLIENT TROUVE : " WS-CUST-NAME
                   DISPLAY "STATUT        : " WS-CUST-STATUS
               WHEN 100
                   DISPLAY "ERREUR : AUCUN CLIENT CORRESPONDANT."
               WHEN OTHER
                   DISPLAY "ERREUR CRITIQUE DB2. SQLCODE : " SQLCODE
           END-EVALUATE.

           STOP RUN.