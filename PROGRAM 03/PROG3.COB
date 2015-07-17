       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROG3.
      *AUTHOR.        ERIC.
      *INSTALLATION.  WSC.
      *DATE-WRITTEN.  NOV 10,2002.
      *DATE-COMPILED. NOV 10,2002.
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
           SELECT PAYROLL-FILE-IN
               ASSIGN TO TW8B8C.
           SELECT PAYROLL-REPORT-OUT
               ASSIGN TO PROG3-OUT.
      *
      *
      *
       DATA DIVISION.
      *
      *
       FILE SECTION.
      *
       FD  PAYROLL-FILE-IN
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS PR-PAYROLL-RECORD-IN.
      *
       01  PR-PAYROLL-RECORD-IN.
           05  PR-REC-CODE-IN                PIC X(2).
           05  PR-SSN-IN.
               10  PR-SSN-THREE-IN           PIC X(3).
               10  PR-SSN-TWO-IN             PIC X(2).             
               10  PR-SSN-FOUR-IN            PIC X(4).
           05  PR-EMPLOYEE-NAME-IN           PIC X(18).
           05  PR-HOURS-IN                   PIC 999V99.
           05  PR-OVERTIME-HOURS-IN          PIC 999V99.
           05  FILLER                        PIC X(6).
           05  PR-RATE-IN                    PIC 99V99.
           05  FILLER                        PIC X(21).
           05  PR-SHIFT-NUMBER-IN            PIC X(1).
           05  FILLER                        PIC X(1).
           05  PR-YEAR-TO-DATE-IN            PIC 9(6)V99.
      *
       FD  PAYROLL-REPORT-OUT
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS ER-EARNINGS-REPORT-OUT.
      *
       01  ER-EARNINGS-REPORT-OUT            PIC X(132).
      *
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SWITCH                 PIC X(3).
      *
       01  WS-PAGE-LAYOUT.
           05  WS-NUMBER                     PIC 99.                    
           05  WS-LINES-IN-USE               PIC 99.
           05  WS-LINES-ON-PAGE              PIC 99.
           05  WS-SPACING                    PIC 99.
           05  WS-PAGE-COUNTER               PIC 99.
      *
       01  WS-DATE.
           05  WS-DATE-ACCEPT                PIC X(6).
           05  WS-DATES REDEFINES WS-DATE-ACCEPT.
               10  WS-YEAR-IN                PIC 9(2).
               10  WS-MONTH-IN               PIC 9(2).
               10  WS-DAY-IN                 PIC 9(2).
      *
       01  WS-CALCULATIONS.
           05  WS-SHIFT                      PIC S999V99.
           05  WS-REGULAR-EARNINGS           PIC S9999V99.
           05  WS-TAX-AMOUNT                 PIC S9999V99.
           05  WS-TAXES                      PIC S9999V99.
           05  WS-TOTAL-HOURS                PIC S9999V99.
           05  WS-OVERTIME-EARNINGS          PIC S9999V99.
           05  WS-TOTAL-EARNINGS             PIC S9999V99.
           05  WS-FICA                       PIC S9999V99.
           05  WS-ALL-DIFF-EARNINGS          PIC S9999V99.
           05  WS-ALL-REGULAR-EARNINGS       PIC S9999V99.
           05  WS-ALL-OVERTIME-EARNINGS      PIC S9999V99.
           05  WS-ALL-TOTAL-EARNINGS         PIC S9999V99.
           05  WS-ALL-FICA                   PIC S9999V99.
           05  WS-AMOUNT-TAXED               PIC S9999V99.
           05  WS-NYTD                       PIC 9(6)V99.
      *
       01  WS-HEADINGS-1.
           05  WS-DATE-1.
               10  WS-MONTH                  PIC 9(2).
               10  FILLER                    PIC X(1) VALUE "/".
               10  WS-DAY                    PIC 9(2).
               10  FILLER                    PIC X(1) VALUE "/".
               10  WS-YEAR                   PIC 9(2).
           05  FILLER                        PIC X(33) VALUE SPACES.
           05  FILLER  PIC X(15) VALUE "E A R N I N G S".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER  PIC X(11) VALUE "R E P O R T".
           05  FILLER                        PIC X(45) VALUE SPACES.
           05  FILLER                        PIC X(4) VALUE "PAGE".
           05  WS-PAGE-OUT                   PIC Z9.
           05  FILLER                        PIC X(12) VALUE SPACES.
      *
       01  WS-HEADINGS-2.
           05  FILLER                        PIC X(50) VALUE SPACES.
           05  FILLER                        PIC X(10)
               VALUE "ERIC ESTES".
           05  FILLER                        PIC X(72) VALUE SPACES.
      *
       01  WS-HEADINGS-3                     PIC X(132) VALUE SPACES.
      *
       01  COLUMN-HEADINGS-1.
           05  FILLER                        PIC X(1) VALUE SPACES.
           05  FILLER                        PIC X(9) VALUE "SOC. SEC.".
           05  FILLER                        PIC X(5) VALUE SPACES.
           05  FILLER                        PIC X(8) VALUE "EMPLOYEE".
           05  FILLER                        PIC X(6) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "SHIFT".
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  FILLER                        PIC X(6) VALUE "HOURLY".
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "TOTAL".
           05  FILLER                        PIC X(4) VALUE SPACES.
           05  FILLER                        PIC X(4) VALUE "REG.".
           05  FILLER                        PIC X(4) VALUE SPACES.
           05  FILLER                        PIC X(4) VALUE "O.T.".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "SHIFT".
           05  FILLER                        PIC X(4) VALUE SPACES.
           05  FILLER                        PIC X(7) VALUE "REGULAR".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(8) VALUE "OVERTIME".
           05  FILLER                        PIC X(5) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "TOTAL".
           05  FILLER                        PIC X(9) VALUE SPACES.
           05  FILLER                        PIC X(4) VALUE "FICA".
           05  FILLER                        PIC X(14) VALUE SPACES.
      *
       01  COLUMN-HEADINGS-2.
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  FILLER                        PIC X(6) VALUE "NUMBER".
           05  FILLER                        PIC X(9) VALUE SPACES.
           05  FILLER                        PIC X(4) VALUE "NAME".
           05  FILLER                        PIC X(9) VALUE SPACES.
           05  FILLER                        PIC X(4) VALUE "CODE".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(4) VALUE "RATE".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "HOURS".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "HOURS".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "HOURS".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(5) VALUE "DIFF.".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(8) VALUE "EARNINGS".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(8) VALUE "EARNINGS".
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  FILLER                        PIC X(8) VALUE "EARNINGS".
           05  FILLER                        PIC X(9) VALUE SPACES.
           05  FILLER                        PIC X(3) VALUE "TAX".
           05  FILLER                        PIC X(16) VALUE SPACES.
      *
       01  WS-DETAIL-LINE.
           05  WS-SSN-OUT.
               10  WS-SSN-THREE-OUT          PIC 9(3).
               10  HYPHEN1-OUT               PIC X(1).
               10  WS-SSN-TWO-OUT            PIC 9(2).
               10  HYPHEN2-OUT               PIC X(1).
               10  WS-SSN-FOUR-OUT           PIC X(4).
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  WS-EMPLOYEE-NAME-OUT           PIC X(18).
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  WS-SHIFT-NUMBER-OUT           PIC 9(1).
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  WS-RATE-OUT                   PIC Z9.99.
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  WS-TOTAL-HOURS-OUT            PIC ZZZ.99-.
           05  FILLER                        PIC X(1) VALUE SPACES.
           05  WS-HOURS-OUT                  PIC ZZ9.99-.
           05  FILLER                        PIC X(1) VALUE SPACES.
           05  WS-OVERTIME-HOURS-OUT         PIC ZZ9.99-.
           05  FILLER                        PIC X(1) VALUE SPACES.
           05  WS-SHIFT-OUT                  PIC ZZ9.99-.
           05  FILLER                        PIC X(1) VALUE SPACES.
           05  WS-REGULAR-EARNINGS-OUT       PIC Z,ZZ9.99.
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  WS-OVERTIME-EARNINGS-OUT      PIC Z,ZZ9.99-.
           05  FILLER                        PIC X(3) VALUE SPACES.
           05  WS-TOTAL-EARNINGS-OUT         PIC Z,ZZ9.99-.
           05  FILLER                        PIC X(6) VALUE SPACES.
           05  WS-FICA-OUT                   PIC Z,ZZ9.99.
           05  FILLER                        PIC X(11) VALUE SPACES.
      *
       01  WS-DETAIL-LINE-TWO.
           05  FILLER                        PIC X(66) VALUE SPACES.
           05  WS-ALL-DIFF-EARNINGS-OUT      PIC ZZZ9.99.
           05  FILLER                        PIC X(1) VALUE SPACES.
           05  WS-ALL-REGULAR-EARNINGS-OUT   PIC ZZ,ZZ9.99.
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  WS-ALL-OVERTIME-EARNINGS-OUT  PIC ZZ,ZZ9.99-.
           05  FILLER                        PIC X(2) VALUE SPACES.
           05  WS-ALL-TOTAL-EARNINGS-OUT     PIC ZZ,ZZ9.99.
           05  FILLER                        PIC X(6) VALUE SPACES.
           05  WS-ALL-FICA-OUT               PIC ZZ,ZZ9.99.
           05  FILLER                        PIC X(13) VALUE SPACES.
      *
      *
      * 
       PROCEDURE DIVISION.
      *
      *
       A00-MAINLINE-PARA.
           OPEN INPUT PAYROLL-FILE-IN
               OUTPUT PAYROLL-REPORT-OUT.
           PERFORM B10-INIT-PARA.
           READ PAYROLL-FILE-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH.
           PERFORM B20-PROCESS-PARA
               UNTIL WS-EOF-SWITCH = "YES".
           PERFORM B30-TOTAL-PARA.
           CLOSE PAYROLL-FILE-IN
               PAYROLL-REPORT-OUT.
           STOP RUN.
      *
      *
       B10-INIT-PARA.
           MOVE "NO" TO WS-EOF-SWITCH.
           MOVE ZEROS TO WS-LINES-IN-USE
                         WS-SHIFT
                         WS-REGULAR-EARNINGS
                         WS-TOTAL-EARNINGS
                         WS-TOTAL-HOURS
                         WS-OVERTIME-EARNINGS
                         WS-FICA
                         WS-ALL-DIFF-EARNINGS
                         WS-ALL-REGULAR-EARNINGS
                         WS-ALL-OVERTIME-EARNINGS
                         WS-ALL-TOTAL-EARNINGS
                         WS-ALL-FICA
                         WS-AMOUNT-TAXED
                         WS-NYTD.
           ACCEPT WS-DATE-ACCEPT FROM DATE.
           MOVE WS-MONTH-IN TO WS-MONTH.
           MOVE WS-DAY-IN TO WS-DAY.
           MOVE WS-YEAR-IN TO WS-YEAR.
           PERFORM C10-HEADINGS-PARA.
      *
      *
       B20-PROCESS-PARA.
           COMPUTE WS-TOTAL-HOURS = PR-HOURS-IN + PR-OVERTIME-HOURS-IN.
           MOVE SPACES TO WS-DETAIL-LINE
                          WS-DETAIL-LINE-TWO.
           
           IF PR-SHIFT-NUMBER-IN = 3 THEN
               COMPUTE WS-SHIFT = (.125 * PR-RATE-IN) * WS-TOTAL-HOURS
           ELSE IF PR-SHIFT-NUMBER-IN = 2 THEN
                   COMPUTE WS-SHIFT =
                       (.10 * PR-RATE-IN) * WS-TOTAL-HOURS
               ELSE
                   MOVE ZERO TO WS-SHIFT
           END-IF.


           COMPUTE WS-REGULAR-EARNINGS = PR-HOURS-IN * PR-RATE-IN.
           COMPUTE WS-OVERTIME-EARNINGS =
                PR-OVERTIME-HOURS-IN * (PR-RATE-IN * 1.5).
           COMPUTE WS-TOTAL-EARNINGS =
                WS-SHIFT + WS-REGULAR-EARNINGS + WS-OVERTIME-EARNINGS.
           
           COMPUTE WS-NYTD = PR-YEAR-TO-DATE-IN + WS-TOTAL-EARNINGS.


           IF PR-YEAR-TO-DATE-IN >= 29700 THEN
               MOVE ZERO TO WS-FICA
           END-IF.


           IF WS-NYTD <= 29700 THEN
               COMPUTE WS-FICA = (29700 - WS-NYTD) * .0665
           END-IF.


           IF PR-YEAR-TO-DATE-IN < 29700 AND WS-NYTD > 29700
               COMPUTE WS-FICA = (29700 - PR-YEAR-TO-DATE-IN) * .0665
           END-IF.


           ADD WS-AMOUNT-TAXED TO WS-ALL-FICA.
           ADD WS-SHIFT TO WS-ALL-DIFF-EARNINGS.
           ADD WS-REGULAR-EARNINGS TO WS-ALL-REGULAR-EARNINGS.
           ADD WS-OVERTIME-EARNINGS TO WS-ALL-OVERTIME-EARNINGS.
           ADD WS-TOTAL-EARNINGS TO WS-ALL-TOTAL-EARNINGS.
           ADD WS-FICA TO WS-ALL-FICA.
           MOVE PR-SSN-THREE-IN TO WS-SSN-THREE-OUT.
           MOVE "-" TO HYPHEN1-OUT.
           MOVE PR-SSN-TWO-IN TO WS-SSN-TWO-OUT.
           MOVE "-" TO HYPHEN2-OUT.
           MOVE PR-SSN-FOUR-IN TO WS-SSN-FOUR-OUT.
           MOVE PR-EMPLOYEE-NAME-IN TO WS-EMPLOYEE-NAME-OUT.
           MOVE PR-HOURS-IN TO WS-HOURS-OUT.
           MOVE PR-OVERTIME-HOURS-IN TO WS-OVERTIME-HOURS-OUT.
           MOVE PR-SHIFT-NUMBER-IN TO WS-SHIFT-NUMBER-OUT.
           MOVE PR-RATE-IN TO WS-RATE-OUT.
           MOVE WS-TOTAL-HOURS TO WS-TOTAL-HOURS-OUT.
           MOVE WS-SHIFT TO WS-SHIFT-OUT.
           MOVE WS-OVERTIME-EARNINGS TO WS-OVERTIME-EARNINGS-OUT.
           MOVE WS-REGULAR-EARNINGS TO WS-REGULAR-EARNINGS-OUT.
           MOVE WS-TOTAL-EARNINGS TO WS-TOTAL-EARNINGS-OUT.
           MOVE WS-FICA TO WS-FICA-OUT.
           MOVE 2 TO WS-SPACING.
           WRITE ER-EARNINGS-REPORT-OUT FROM WS-DETAIL-LINE
               AFTER ADVANCING WS-SPACING.
           MOVE 1 TO WS-NUMBER.
           ADD WS-SPACING TO WS-LINES-IN-USE.
           
           IF WS-LINES-IN-USE >= 60 THEN
               MOVE ZERO TO WS-LINES-IN-USE
               PERFORM C10-HEADINGS-PARA
           END-IF.


           READ PAYROLL-FILE-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH.
      *
      *
       B30-TOTAL-PARA.
           MOVE WS-ALL-DIFF-EARNINGS TO WS-ALL-DIFF-EARNINGS-OUT.
           MOVE WS-ALL-REGULAR-EARNINGS TO WS-ALL-REGULAR-EARNINGS-OUT.
           MOVE WS-ALL-OVERTIME-EARNINGS TO WS-ALL-OVERTIME-EARNINGS-OUT.
           MOVE WS-ALL-TOTAL-EARNINGS TO WS-ALL-TOTAL-EARNINGS-OUT.
           MOVE WS-ALL-FICA TO WS-ALL-FICA-OUT.
           
           IF WS-LINES-IN-USE >= 60 THEN
               MOVE ZERO TO WS-LINES-IN-USE
               PERFORM C10-HEADINGS-PARA
           END-IF.


           MOVE 3 TO WS-SPACING.
           WRITE ER-EARNINGS-REPORT-OUT FROM WS-DETAIL-LINE-TWO
      *        AFTER ADVANCING WS-SPACING.
               AFTER ADVANCING 3 LINES.
           ADD WS-SPACING TO WS-LINES-IN-USE.           
      *
      *
       C10-HEADINGS-PARA.
           ADD 1 TO WS-NUMBER.
           ADD 1 TO WS-PAGE-COUNTER.
           MOVE WS-PAGE-COUNTER TO WS-PAGE-OUT.
           MOVE WS-NUMBER TO WS-NUMBER.
           WRITE ER-EARNINGS-REPORT-OUT FROM WS-HEADINGS-1
               AFTER ADVANCING PAGE.
           MOVE 1 TO WS-SPACING.
           WRITE ER-EARNINGS-REPORT-OUT FROM WS-HEADINGS-2
               AFTER ADVANCING WS-SPACING.
           WRITE ER-EARNINGS-REPORT-OUT FROM WS-HEADINGS-3
               AFTER ADVANCING WS-SPACING.
           ADD WS-SPACING TO WS-LINES-IN-USE.
           MOVE 2 TO WS-SPACING.
           WRITE ER-EARNINGS-REPORT-OUT FROM COLUMN-HEADINGS-1
               AFTER ADVANCING WS-SPACING.
           ADD WS-SPACING TO WS-LINES-IN-USE.
           MOVE 2 TO WS-SPACING.
           WRITE ER-EARNINGS-REPORT-OUT FROM COLUMN-HEADINGS-2
           WRITE ER-EARNINGS-REPORT-OUT FROM WS-HEADINGS-3
               AFTER ADVANCING WS-SPACING. 