       IDENTIFICATION DIVISION.
      *
      *
       PROGRAM-ID.    PROG12-2.
      *AUTHOR.        ERIC.
      *INSTALLATION.  WSC.
      *DATE-WRITTEN.  JAN 30,2003.
      *DATE-COMPILED. JAN 30,2003.
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
      *  
      *
      *
       ENVIRONMENT DIVISION.
      *
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT EARNINGS-FILE-IN
               ASSIGN TO "EARNINGS.DAT".
           SELECT PAY-RATE-TABLE-FILE-IN
               ASSIGN TO "PAYTABLE1.DAT".
           SELECT INCOME-TAX-REPORT-OUT
               ASSIGN TO "PROG12-2_OUT.DAT".
      *
      *
      *
       DATA DIVISION.
      *
      *
       FILE SECTION.
      *
       FD  EARNINGS-FILE-IN
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS ER-EARNINGS-REC-IN.
      *
       01  ER-EARNINGS-REC-IN               PIC X(80).
      *
       FD  PAY-RATE-TABLE-FILE-IN
               RECORD CONTAINS 134 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS PAY-RATE-TABLE-REC.
      *
           01  PAY-RATE-TABLE-REC           PIC X(134).
      *
       FD  INCOME-TAX-REPORT-OUT
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS IT-REPORT-OUT.
      *
       01  IT-REPORT-OUT                   PIC X(132).
      *
      *
       WORKING-STORAGE SECTION.
      *
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW                    PIC X(1).
      *
       01  WS-PAGE-LAYOUT.
           05  WS-LINES-USED                PIC 9(2).
           05  WS-PAGE-COUNT                PIC 9(2).
           05  WS-SPACING                   PIC 9 VALUE 2.
      *
       01  WS-CALCULATIONS.
           05  WS-ANNUAL-EARN               PIC 9(6)V99.
           05  WS-TAXABLE-EARN              PIC 9(6)V99.
           05  WS-TMP-TAXABLE-EARN          PIC 9(6)V99.
           05  WS-TMP-TAX-AMNT              PIC 9(6)V99.
           05  WS-TEMP-TAX-EARN             PIC 9(6)V99.
           05  WS-TAX-HOLD                  PIC 9(6)V99.
           05  WS-PERC-TAX-EARN             PIC 9(6)V99.
      *
       01  WS-DATE-ACCEPT.
           05  DATE-ACCEPT                  PIC X(6).
           05  DATE-WS REDEFINES DATE-ACCEPT.
               10  WS-YEAR                  PIC X(2).
               10  WS-MONTH                 PIC X(2).
               10  WS-DAY                   PIC X(2).
      *
       01  WS-TABLE-CONTROLS.
           05  WS-ROW-FOUND-SW              PIC X(1).
           05  WS-EOT-SW                    PIC X(1).
           05  WS-TABLE-LOAD-SW             PIC X(1).
           05  WS-SEARCH-EOT-SW             PIC X(1).
           05  WS-STAT-FOUND-SW             PIC X(1).
           05  WS-COUNT1                    PIC 9.
           05  WS-COUNT2                    PIC 9.
      *
       01  IT-RATE-TABLE.
           05  IT-PAY-STATUS-IN    OCCURS 3 TIMES
                                   INDEXED BY IT-STATUS-INDEX.
               10  IT-STATUS-CODE-IN        PIC X.
               10  IT-TAX-RATE-IN  OCCURS 7 TIMES
                                   INDEXED BY IT-RATE-INDEX.
                   15  IT-LL-IN             PIC 9(5).
                   15  IT-PERCENT-IN        PIC V99.
                   15  IT-ADDED-TAX-IN      PIC 9(5)V99.
                   15  IT-UL-IN             PIC 9(5).
      *
       01  WS-ER-EARNINGS-REC-IN.
           05  FILLER                       PIC X(10) VALUE SPACES.
           05  WS-ER-EMPL-NUM-IN.
               10  WS-ER-EMPL-NUM-3-IN      PIC X(3).
               10  WS-ER-EMPL-NUM-2-IN      PIC X(2).
               10  WS-ER-EMPL-NUM-4-IN      PIC X(4).
           05  WS-ER-EMPL-LNAME-IN          PIC X(12).
           05  WS-ER-EMPL-FNAME-IN          PIC X(9).
           05  FILLER                       PIC X(9) VALUE SPACES.
           05  WS-ER-MAR-STAT-IN            PIC X(1).
           05  WS-ER-EXEMPTIONS-IN          PIC 9(2).
           05  FILLER                       PIC X(7) VALUE SPACES.
           05  WS-ER-EARN-THIS-PER-IN       PIC 9(5)V99.
           05  FILLER                       PIC X(14) VALUE SPACES.
      *
       01  WS-HEADING.
           05  FILLER                       PIC X(7) VALUE "FEDERAL".
           05  FILLER                       PIC X(1) VALUE SPACES.
           05  FILLER                       PIC X(6) VALUE "INCOME".
           05  FILLER                       PIC X(1) VALUE SPACES.
           05  FILLER                       PIC X(3) VALUE "TAX".
           05  FILLER                       PIC X(1) VALUE SPACES.
           05  FILLER                       PIC X(8) VALUE "REGISTER".
           05  FILLER                       PIC X(1) VALUE SPACES.
           05  FILLER                       PIC X(6) VALUE "(12-2)".
           05  FILLER                       PIC X(37) VALUE SPACES.
           05  WS-RUN-DATE-OUT.
               10  WS-RUN-MONTH-OUT         PIC X(2).
               10  FILLER                   PIC X(1) VALUE "/".
               10  WS-RUN-DAY-OUT           PIC X(2).
               10  FILLER                   PIC X(1) VALUE "/".
               10  WS-RUN-YR-OUT            PIC X(2).
           05  FILLER                       PIC X(3) VALUE SPACES.
           05  FILLER                       PIC X(4) VALUE "PAGE".
           05  FILLER                       PIC X(1) VALUE SPACES.
           05  WS-PAGE-OUT                  PIC ZZ9.
           05  FILLER                       PIC X(42) VALUE SPACES.
      *
       01  WS-COL-HEADING-01.
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EMPLOYEE".
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "-----".
           05  FILLER                     PIC X(8) VALUE "EMPLOYEE".
           05  FILLER                     PIC X(1) VALUE "-".
           05  FILLER                     PIC X(4) VALUE "NAME".
           05  FILLER                     PIC X(4) VALUE "----".
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "M".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(3) VALUE "NO.".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "CURR".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "PER.".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE "ANNUALIZED".
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  FILLER                     PIC X(7) VALUE "TAXABLE".
           05  FILLER                     PIC X(8) VALUE SPACES.
           05  FILLER                     PIC X(3) VALUE "TAX".
           05  FILLER                     PIC X(42) VALUE SPACES.
      *
       01  WS-COL-HEADING-02.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "NUMBER".
           05  FILLER                     PIC X(4) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "LAST".
           05  FILLER                     PIC X(9) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "FIRST".
           05  FILLER                     PIC X(7) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "S".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(3) VALUE "EX.".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EARNINGS".
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EARNINGS".
           05  FILLER                     PIC X(4) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EARNINGS".
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "AMOUNT".
           05  FILLER                     PIC X(42) VALUE SPACES.
      *
       01  WS-DETAIL-LINE.
           05  WS-SSN-OUT.
               10  WS-SSN-3-OUT           PIC X(3).
               10  WS-DASH-1-OUT          PIC X(1) VALUE "-".
               10  WS-SSN-2-OUT           PIC X(2).
               10  WS-DASH-2-OUT          PIC X(1) VALUE "-".
               10  WS-SSN-4-OUT           PIC X(4).
           05  FILLER                     PIC X(2).
           05  WS-EMPL-LNAME-OUT          PIC X(12).
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-EMPL-FNAME-OUT          PIC X(9).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-MAR-STAT-OUT            PIC X(1).
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-EXEMPTIONS-OUT          PIC 9(2).
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-EARN-THIS-PER-OUT       PIC ZZ,ZZ9.99.
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ANNUAL-EARN-OUT         PIC ZZZ,ZZ9.99.
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-TAXABLE-EARN-OUT        PIC ZZZ,ZZ9.99.
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-TAX-AMOUNT-OUT          PIC ZZ,ZZ9.99.
           05  WS-TAX-ERROR-OUT REDEFINES WS-TAX-AMOUNT-OUT PIC X(9).
           05  FILLER                     PIC X(42) VALUE SPACES.
      *
      *
      *
       PROCEDURE DIVISION.
      *
      *
       A00-MAINLINE-PARA.
           OPEN INPUT  EARNINGS-FILE-IN
                OUTPUT INCOME-TAX-REPORT-OUT.
           PERFORM B10-INIT-PARA.
           READ EARNINGS-FILE-IN INTO WS-ER-EARNINGS-REC-IN
                AT END MOVE "Y" TO WS-EOF-SW.
           PERFORM B30-LOAD-TABLE-PARA
                UNTIL WS-TABLE-LOAD-SW = "Y".
           PERFORM B20-PROCESS-PARA
                UNTIL WS-EOF-SW = "Y". 
           CLOSE EARNINGS-FILE-IN
                 INCOME-TAX-REPORT-OUT.
           STOP RUN.
      *
      *
       B10-INIT-PARA.
           MOVE ZEROS TO WS-LINES-USED.
           MOVE 1 TO WS-PAGE-COUNT.
           ACCEPT DATE-ACCEPT FROM DATE.
               MOVE WS-MONTH TO WS-RUN-MONTH-OUT
               MOVE WS-DAY TO WS-RUN-DAY-OUT
               MOVE WS-YEAR TO WS-RUN-YR-OUT.
           PERFORM C10-HEADINGS-PARA.
      *
      *
       B20-PROCESS-PARA.
           MOVE SPACES TO WS-DETAIL-LINE.
           MOVE ZEROS TO WS-TAXABLE-EARN.
           MULTIPLY WS-ER-EARN-THIS-PER-IN BY 12 GIVING
               WS-ANNUAL-EARN ROUNDED.
           MULTIPLY WS-ER-EXEMPTIONS-IN BY 1000 GIVING
               WS-TMP-TAXABLE-EARN ROUNDED.


           IF WS-TMP-TAXABLE-EARN IS GREATER THAN WS-ANNUAL-EARN THEN
               MOVE ZEROS TO WS-TAXABLE-EARN
           ELSE
               SUBTRACT WS-TMP-TAXABLE-EARN FROM WS-ANNUAL-EARN GIVING
                   WS-TAXABLE-EARN
           END-IF.
           
           MOVE "N" TO WS-EOT-SW.
           PERFORM B40-SEARCH-TABLE-PARA
               UNTIL WS-EOT-SW = "Y"        
           MOVE WS-ANNUAL-EARN TO WS-ANNUAL-EARN-OUT
           MOVE WS-ER-EMPL-NUM-3-IN TO WS-SSN-3-OUT
           MOVE "-" TO WS-DASH-1-OUT
           MOVE WS-ER-EMPL-NUM-2-IN TO WS-SSN-2-OUT
           MOVE "-" TO WS-DASH-2-OUT
           MOVE WS-ER-EMPL-NUM-4-IN TO WS-SSN-4-OUT
           MOVE WS-ER-EMPL-LNAME-IN TO WS-EMPL-LNAME-OUT
           MOVE WS-ER-EMPL-FNAME-IN TO WS-EMPL-FNAME-OUT
           MOVE WS-ER-MAR-STAT-IN TO WS-MAR-STAT-OUT
           MOVE WS-ER-EXEMPTIONS-IN TO WS-EXEMPTIONS-OUT
           MOVE WS-ER-EARN-THIS-PER-IN TO WS-EARN-THIS-PER-OUT.
           MOVE WS-TAXABLE-EARN TO WS-TAXABLE-EARN-OUT.


           IF WS-LINES-USED >= 57 THEN
               MOVE ZEROS TO WS-LINES-USED
               PERFORM C10-HEADINGS-PARA
           END-IF.


           WRITE IT-REPORT-OUT FROM WS-DETAIL-LINE
               AFTER ADVANCING  WS-SPACING.
           ADD 2 TO WS-LINES-USED.
           READ EARNINGS-FILE-IN INTO WS-ER-EARNINGS-REC-IN
               AT END MOVE "Y" TO WS-EOF-SW.
      *
      *
       C10-HEADINGS-PARA.
           MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
           WRITE IT-REPORT-OUT FROM WS-HEADING
               AFTER ADVANCING PAGE.
           MOVE SPACES TO IT-REPORT-OUT.
           WRITE IT-REPORT-OUT
               AFTER ADVANCING 1 LINE.
           WRITE IT-REPORT-OUT FROM WS-COL-HEADING-01
               AFTER ADVANCING 1 LINE.
           WRITE IT-REPORT-OUT FROM WS-COL-HEADING-02
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO IT-REPORT-OUT.
           WRITE IT-REPORT-OUT
               AFTER ADVANCING 1 LINE.
           ADD 5 TO WS-LINES-USED.
           ADD 1 TO WS-PAGE-COUNT.
      *
      *
       B30-LOAD-TABLE-PARA.
           OPEN INPUT PAY-RATE-TABLE-FILE-IN
           MOVE "N" TO WS-TABLE-LOAD-SW.


           PERFORM VARYING IT-STATUS-INDEX
               FROM 1 BY 1
                   UNTIL WS-TABLE-LOAD-SW = "Y"
               READ PAY-RATE-TABLE-FILE-IN
                   AT END MOVE "Y" TO WS-TABLE-LOAD-SW
                   NOT AT END
                   MOVE PAY-RATE-TABLE-REC
                       TO IT-PAY-STATUS-IN(IT-STATUS-INDEX)
      *         DISPLAY IT-PAY-STATUS-IN(IT-STATUS-INDEX)
               END-READ
           END-PERFORM.
           CLOSE PAY-RATE-TABLE-FILE-IN.
      *
      *
       B40-SEARCH-TABLE-PARA.
           PERFORM VARYING IT-STATUS-INDEX FROM 1 BY 1 UNTIL
               IT-STATUS-INDEX = 4 OR WS-EOT-SW = "Y"


               IF WS-ER-MAR-STAT-IN = IT-STATUS-CODE-IN(IT-STATUS-INDEX)
                   THEN
               PERFORM VARYING IT-RATE-INDEX FROM 1 BY 1
                   UNTIL IT-RATE-INDEX = 8 OR WS-EOT-SW = "Y"
                   
                   IF WS-TAXABLE-EARN >= 
                       IT-LL-IN(IT-STATUS-INDEX,IT-RATE-INDEX) AND
                      WS-TAXABLE-EARN <
                       IT-UL-IN(IT-STATUS-INDEX,IT-RATE-INDEX) THEN    


                    MOVE "Y" TO WS-EOT-SW


                    SUBTRACT IT-LL-IN(IT-STATUS-INDEX,IT-RATE-INDEX)
                        FROM
                    WS-TAXABLE-EARN GIVING WS-TEMP-TAX-EARN


                    MULTIPLY WS-TEMP-TAX-EARN
                    BY IT-PERCENT-IN(IT-STATUS-INDEX,IT-RATE-INDEX)
                    GIVING WS-PERC-TAX-EARN


                    COMPUTE WS-TAX-HOLD = 
                      (IT-ADDED-TAX-IN(IT-STATUS-INDEX,IT-RATE-INDEX) +
                      WS-PERC-TAX-EARN) / 12


      *              ADD IT-ADDED-TAX-IN(IT-STATUS-INDEX,IT-RATE-INDEX)
      *              TO WS-PERC-TAX-EARN GIVING WS-TAX-HOLD


                    MOVE WS-TAX-HOLD TO WS-TAX-AMOUNT-OUT
                   
                   ELSE
                    MOVE "TAX ERROR" TO WS-TAX-ERROR-OUT
                   END-IF


               END-PERFORM
               END-IF
               IF IT-STATUS-INDEX >= 3 AND IT-RATE-INDEX >= 7 THEN
                   MOVE "Y" TO WS-EOT-SW
               END-IF
            END-PERFORM. 