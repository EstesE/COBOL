       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROG2.
      *AUTHOR.        ERIC.
      *INSTALLATION.  WSC.
      *DATE-WRITTEN.  OCT 24,2002.
      *DATE-COMPILED. OCT 24,2002.
      *SECURITY.      NONE.
      *
      *
      *
      *
      *
      *
      *
      *
      *
       ENVIRONMENT DIVISION.
      *
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT PURCHASES-FILE-IN
               ASSIGN TO TW5C5D.
           SELECT PURCHASES-FILE-OUT
               ASSIGN TO PROG2-OUT.
      *
      *
      *
       DATA DIVISION.
      *
      *
       FILE SECTION.
      *
       FD  PURCHASES-FILE-IN
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS PURCHASES-REC-IN.
      *
       01  PURCHASES-REC-IN.
           05  PR-REC-CODE-IN                   PIC 9(2).
           05  FILLER                           PIC X(3).
           05  PR-CUST-NAME-IN                  PIC X(24).
           05  FILLER                           PIC X(38).
           05  PR-DISC-PERCENT-IN               PIC V99.
           05  FILLER                           PIC X(2).
           05  PR-PURCH-AMT-IN                  PIC S9(7)V99.
      *
       FD  PURCHASES-FILE-OUT
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS SALES-REPORT-OUT.
      *
       01  SALES-REPORT-OUT.
           05  FILLER                           PIC X(2).
           05  SR-CUST-NAME-OUT                 PIC X(24).
           05  FILLER                           PIC X(3).
           05  SR-PURCH-AMT-OUT                 PIC Z,ZZZ,ZZ9.99-.
           05  FILLER                           PIC X(4).
           05  SR-DISC-AMT-OUT                  PIC ZZZ,ZZ9.99.
           05  FILLER                           PIC X(4).
           05  SR-NET-AMT-OUT                   PIC ZZZZ,ZZ9.99-.
           05  FILLER                           PIC X(60).
      *
       01  TOT-TRANS-OUT.
           05  FILLER                           PIC X(4).
           05  TT-TITLE-OUT                     PIC X(18).
           05  FILLER                           PIC X(1).
           05  TT-TOT-TRANS-OUT                 PIC ZZZ9.
           05  FILLER                           PIC X(1).
           05  TT-TOT-PURCH-AMT-OUT             PIC ZZ,ZZZ,ZZ9.99.
           05  TT-ASTERISK-01-OUT               PIC X(1).
           05  FILLER                           PIC X(1).
           05  TT-TOT-DISC-AMT-OUT              PIC Z,ZZZ,ZZ9.99.
           05  TT-ASTERISK-02-OUT               PIC X(1).
           05  FILLER                           PIC X(1).
           05  TT-TOT-NET-PURCH-AMT-OUT         PIC ZZ,ZZZ,ZZ9.99.
           05  TT-ASTERISK-03-OUT               PIC X(1).
           05  FILLER                           PIC X(61).
      *
       01  AVG-AMTS-OUT.
           05  FILLER                           PIC X(4).
           05  AA-TITLE-OUT                     PIC X(15).
           05  FILLER                           PIC X(10).
           05  AA-AVG-PURCH-AMT-OUT             PIC Z,ZZZ,ZZ9.99.
           05  FILLER                           PIC X(4).
           05  AA-AVG-DISC-AMT-OUT              PIC ZZZ,ZZ9.99.
           05  FILLER                           PIC X(3).
           05  AA-AVG-NET-PURCH-AMT-OUT         PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                           PIC X(61).
      *
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SWITCH                    PIC X(3).
      *
       01  WS-TEMP-AMTS.
           05  WS-DISC-AMT                      PIC S9(7)V99.
           05  WS-NET-AMT                       PIC S9(7)V99.
      *
       01  WS-TOT-AMTS.
           05  WS-TOT-TRANSACTIONS              PIC 9(4).
           05  WS-TOT-PURCH-AMT                 PIC S9(7)V99.
           05  WS-TOT-DISC-AMT                  PIC S9(7)V99.
           05  WS-TOT-NET-AMT                   PIC S9(8)V99.
      *
       01  WS-AVERAGES.
           05  WS-AVG-PURCH-AMT                 PIC ZZZ,ZZ9.99.
           05  WS-AVG-DISC-AMT                  PIC ZZZ,ZZ9.99.
           05  WS-AVG-NET-AMT                   PIC ZZZ,ZZ9.99.
      *
      *
      *
       PROCEDURE DIVISION.
      *
      *
       A00-MAINLINE-PARA.
      *
           OPEN INPUT PURCHASES-FILE-IN
                OUTPUT PURCHASES-FILE-OUT.
           PERFORM B10-INIT-PARA.
           READ PURCHASES-FILE-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH.
           PERFORM B20-PROCESS-DETAILS-PARA UNTIL
               WS-EOF-SWITCH IS EQUAL TO "YES".
           PERFORM C10-PROCESS-TOTALS-PARA.
           PERFORM C20-PROCESS-AVGS-PARA.
      *
           CLOSE PURCHASES-FILE-IN
                 PURCHASES-FILE-OUT.
      *
           STOP RUN.
      *
      *
       B10-INIT-PARA.
      *
           MOVE "NO" TO WS-EOF-SWITCH.
      *
      *
       B20-PROCESS-DETAILS-PARA.
      *
           MOVE SPACES TO SALES-REPORT-OUT.
           ADD 1 TO WS-TOT-TRANSACTIONS.
           MULTIPLY PR-PURCH-AMT-IN BY PR-DISC-PERCENT-IN
               GIVING WS-DISC-AMT ROUNDED.
           SUBTRACT WS-DISC-AMT  FROM PR-PURCH-AMT-IN GIVING WS-NET-AMT.
           ADD WS-NET-AMT TO WS-TOT-NET-AMT.
           ADD WS-DISC-AMT TO WS-TOT-DISC-AMT.
           ADD PR-PURCH-AMT-IN TO WS-TOT-PURCH-AMT.
           MOVE PR-CUST-NAME-IN TO SR-CUST-NAME-OUT.
           MOVE PR-PURCH-AMT-IN TO SR-PURCH-AMT-OUT.
           MOVE WS-DISC-AMT TO SR-DISC-AMT-OUT.
           MOVE WS-NET-AMT TO SR-NET-AMT-OUT.
           WRITE SALES-REPORT-OUT
               AFTER ADVANCING 2 LINES.
           READ PURCHASES-FILE-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH.
      *
      *
       C10-PROCESS-TOTALS-PARA.
      *
           MOVE SPACES TO SALES-REPORT-OUT.
           MOVE "TOTAL TRANSACTIONS" TO TT-TITLE-OUT.
           MOVE WS-TOT-TRANSACTIONS TO TT-TOT-TRANS-OUT.
           MOVE WS-TOT-PURCH-AMT TO TT-TOT-PURCH-AMT-OUT.
           MOVE "*" TO TT-ASTERISK-01-OUT.
           MOVE WS-TOT-DISC-AMT TO TT-TOT-DISC-AMT-OUT.
           MOVE "*" TO TT-ASTERISK-02-OUT.
           MOVE WS-TOT-NET-AMT TO TT-TOT-NET-PURCH-AMT-OUT.
           MOVE "*" TO TT-ASTERISK-03-OUT.
           WRITE TOT-TRANS-OUT
               AFTER ADVANCING 3 LINES.
      *
      *
       C20-PROCESS-AVGS-PARA.
      *
           DIVIDE WS-TOT-PURCH-AMT BY WS-TOT-TRANSACTIONS
               GIVING WS-AVG-PURCH-AMT.
           DIVIDE WS-TOT-DISC-AMT BY WS-TOT-TRANSACTIONS
               GIVING WS-AVG-DISC-AMT.
           DIVIDE WS-TOT-NET-AMT BY WS-TOT-TRANSACTIONS
               GIVING WS-AVG-NET-AMT.
           MOVE SPACES TO SALES-REPORT-OUT.
           MOVE "AVERAGE AMOUNTS" TO AA-TITLE-OUT.
           MOVE WS-AVG-PURCH-AMT TO AA-AVG-PURCH-AMT-OUT.
           MOVE WS-AVG-DISC-AMT TO AA-AVG-DISC-AMT-OUT.
           MOVE WS-AVG-NET-AMT TO AA-AVG-NET-PURCH-AMT-OUT.
           WRITE AVG-AMTS-OUT
               AFTER ADVANCING 2 LINES. 