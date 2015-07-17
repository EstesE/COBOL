       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROG5.
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
               ASSIGN TO "TW10C12C.DAT".
           SELECT EARNINGS-REPORT-OUT
               ASSIGN TO "PROG5_OUT.DAT".
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
               DATA RECORDS ARE PR-PAYROLL-REC-IN-01
                                PR-PAYROLL-REC-IN-02.
      *
       01  PR-PAYROLL-REC-IN-01           PIC X(80).
       01  PR-PAYROLL-REC-IN-02           PIC X(80).
      *
       FD  EARNINGS-REPORT-OUT
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS ER-REPORT-OUT.
      *
       01  ER-REPORT-OUT                  PIC X(132).
      *
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SWITCH              PIC X(1).
           05  WS-DEPT-GRP-SWITCH         PIC X(1).
           05  WS-PLANT-GRP-SWITCH        PIC X(1).
      *
       01  WS-PAGE-LAYOUT.
           05  WS-LINES-PER-PAGE          PIC 9(2) VALUE 50.
           05  WS-LINES-USED              PIC 9(2).
           05  WS-PAGE-COUNT              PIC 9(2).
           05  WS-SPACING                 PIC 9(2).
           05  WS-PREV-DEPT               PIC X(4).
           05  WS-PREV-PLANT              PIC X(3).
      *
       01  WS-ACCUMULATORS.
           05  WS-DEPT-EARN-THIS-PER      PIC 9(5)V99.
           05  WS-DEPT-YTD-EARN           PIC 9(6)V99.
      *
       01  WS-TOTALS.
           05  WS-DEPT-PER-TOT            PIC 9(6)V99.
           05  WS-DEPT-YTD-TOT            PIC 9(7)V99.
           05  WS-PLANT-PER-TOT           PIC 9(6)V99.
           05  WS-PLANT-YTD-TOT           PIC 9(7)V99.
           05  WS-REPORT-PER-TOT          PIC 9(6)V99.
           05  WS-REPORT-YTD-TOT          PIC 9(7)V99.
      *
       01  WS-DATE-ACCEPT.
           05  DATE-ACCEPT                PIC X(6).
           05  DATE-WS REDEFINES DATE-ACCEPT.
               10  YEAR-WS                PIC X(2).
               10  MONTH-WS               PIC X(2).
               10  DAY-WS                 PIC X(2).
      *
       01  WS-PR-PAYROLL-REC-IN-01.
           05  WS-PR-REC-CODE-IN-01       PIC X(2).
           05  WS-PR-P-ENDING-DATE-IN-01.
               10  WS-PR-MONTH-IN-01      PIC X(2).
               10  WS-PR-DAY-IN-01        PIC X(2).
               10  WS-PR-YEAR-IN-01       PIC X(2).
           05  FILLER                     PIC X(72).
      *
       01  WS-PR-PAYROLL-REC-IN-02.
           05  WS-PR-REC-CODE-IN-02       PIC X(2).
           05  WS-PR-SSN-IN-02.
               10  WS-PR-SSN-3-IN-02      PIC X(3).
               10  WS-PR-SSN-2-IN-02      PIC X(2).
               10  WS-PR-SSN-4-IN-02      PIC X(4).
           05  WS-PR-EMPL-NAME-IN-02      PIC X(18).
           05  FILLER                     PIC X(20).
           05  WS-EARN-THIS-PER-IN-02     PIC 9(5)V99.
           05  WS-PLANT-CODE-IN-02        PIC X(3).
           05  WS-DEPT-NUM-IN-02          PIC X(4).
           05  FILLER                     PIC X(9).
           05  WS-YTD-EARNINGS-IN-02      PIC 9(6)V99.
      *
       01  WS-HEADINGS-01.
           05  FILLER                  PIC X(12) VALUE "DEPARTMENTAL".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EARNINGS".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "REPORT".
           05  FILLER                     PIC X(23) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "PERIOD".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "ENDING".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "DATE".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-P-ENDING-DATE-OUT.
               10  WS-P-END-MONTH-OUT     PIC X(2).
               10  FILLER                 PIC X(1) VALUE "/".
               10  WS-P-END-DAY-OUT       PIC X(2).
               10  FILLER                 PIC X(1) VALUE "/".
               10  WS-P-END-YEAR-OUT      PIC X(2).
           05  FILLER                     PIC X(54).
      *
       01  WS-HEADINGS-02.
           05  FILLER                     PIC X(4) VALUE "PAGE".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-PAGE-OUT                PIC ZZ9.
           05  FILLER                     PIC X(53) VALUE SPACES.
           05  FILLER                     PIC X(3) VALUE "RUN".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "DATE".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-RUN-DATE-OUT.
               10  WS-RUN-DATE-MONTH-OUT  PIC X(2).
               10  FILLER                 PIC X(1) VALUE "/".
               10  WS-RUN-DATE-DAY-OUT    PIC X(2).
               10  FILLER                 PIC X(1) VALUE "/".
               10  WS-RUN-DATE-YEAR-OUT   PIC X(2).
           05  FILLER                     PIC X(54).
      *
       01  WS-BLANK-LINE                  PIC X(132) VALUE SPACES.
      *
       01  WS-COLUMN-HEADING-01.
           05  FILLER                     PIC X(5) VALUE "PLANT".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "DEPT".
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "SOCIAL".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "SEC.".
           05  FILLER                     PIC X(24) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EARNINGS".
           05  FILLER                     PIC X(6) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE "YEAR-TO-DATE".
           05  FILLER                     PIC X(57) VALUE SPACES.
      *
       01  WS-COLUMN-HEADING-02.
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "CODE".
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(3) VALUE "NBR".
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "NUMBER".
           05  FILLER                     PIC X(8) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EMPLOYEE".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "NAME".
           05  FILLER                     PIC X(6) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "THIS".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(3) VALUE "PER".
           05  FILLER                     PIC X(8) VALUE SPACES.
           05  FILLER                     PIC X(8) VALUE "EARNINGS".
           05  FILLER                     PIC X(59) VALUE SPACES.
      *
       01  WS-DETAIL-LINE-01.
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-PLANT-CODE-OUT          PIC X(3).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-DEPT-NUM-OUT            PIC X(4).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-SSN-OUT.
               10  WS-SSN-3-OUT           PIC X(3).
               10  FILLER                 PIC X(1) VALUE "-".
               10  WS-SSN-2-OUT           PIC X(2).
               10  FILLER                 PIC X(1) VALUE "-".
               10  WS-SSN-4-OUT           PIC X(4).
           05  FILLER                     PIC X(3).
           05  WS-EMPL-NAME-OUT           PIC X(18).
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-EARN-THIS-PER-OUT       PIC ZZ,ZZZ.99-.
           05  FILLER                     PIC X(6) VALUE SPACES.
           05  WS-YTD-EARN-OUT            PIC ZZZ,ZZZ.99-.
           05  FILLER                     PIC X(57) VALUE SPACES.
      *
       01  WS-TOTAL-DEPT.
           05  FILLER                     PIC X(7) VALUE SPACES.
           05  WS-TOT-DEPT-OUT            PIC 9(4).
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE "DEPARTMENT".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "TOTAL".
           05  FILLER                     PIC X(15) VALUE SPACES.
           05  WS-TD-EARN-PER-OUT         PIC ZZZ,ZZZ.99-.
           05  FILLER                     PIC X(1) VALUE "*".
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-TD-YTD-EARN-OUT         PIC Z,ZZZ,ZZZ.99-.
           05  FILLER                     PIC X(1) VALUE "*".
           05  FILLER                     PIC X(56) VALUE SPACES.
      *
       01  WS-TOTAL-PLANT.
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-TP-PLANT-CODE-OUT       PIC X(3).
           05  FILLER                     PIC X(12) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "PLANT".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "TOTAL".
           05  FILLER                     PIC X(20) VALUE SPACES.
           05  WS-TP-EARN-PER-OUT         PIC ZZZ,ZZZ.99-.
           05  FILLER                     PIC X(2) VALUE "**".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-TP-YTD-EARN-OUT         PIC Z,ZZZ,ZZZ.99-.
           05  FILLER                     PIC X(2) VALUE "**".
           05  FILLER                     PIC X(55) VALUE SPACES.
      *
       01  WS-REPORT-TOTAL.
           05  FILLER                     PIC X(16) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "REPORT".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "TOTAL".
           05  FILLER                     PIC X(19) VALUE SPACES.
           05  WS-RT-EARN-PER-OUT         PIC ZZZ,ZZZ.99-.
           05  FILLER                     PIC X(3) VALUE "***".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-RT-YTD-EARN-OUT         PIC Z,ZZZ,ZZZ.99-.
           05  FILLER                     PIC X(3) VALUE "***".
           05  FILLER                     PIC X(54) VALUE SPACES.
      *
      *
      *
       PROCEDURE DIVISION.
      *
      *
       A00-MAIN-LINE-PARA.
           OPEN INPUT PAYROLL-FILE-IN
                OUTPUT EARNINGS-REPORT-OUT.
           READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN-01
               AT END MOVE "Y" TO WS-EOF-SWITCH.
           MOVE WS-PR-MONTH-IN-01 TO WS-P-END-MONTH-OUT.   
           MOVE WS-PR-DAY-IN-01 TO WS-P-END-DAY-OUT.
           MOVE WS-PR-YEAR-IN-01 TO WS-P-END-YEAR-OUT.
           PERFORM B10-INIT-PARA.
           READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN-02
               AT END MOVE "Y" TO WS-EOF-SWITCH.
           MOVE WS-DEPT-NUM-IN-02 TO WS-PREV-DEPT
           MOVE WS-PLANT-CODE-IN-02 TO WS-PREV-PLANT
           PERFORM B20-PROCESS-PARA
               UNTIL WS-EOF-SWITCH = "Y".
           PERFORM B30-TOTAL-PARA.
           CLOSE PAYROLL-FILE-IN
                 EARNINGS-REPORT-OUT.
           STOP RUN.
      *
      *
       B10-INIT-PARA.
           MOVE "N" TO WS-EOF-SWITCH.
           MOVE ZEROS TO WS-DEPT-EARN-THIS-PER
                         WS-DEPT-YTD-EARN
                         WS-DEPT-PER-TOT
                         WS-DEPT-YTD-TOT
                         WS-PLANT-PER-TOT
                         WS-PLANT-YTD-TOT
                         WS-REPORT-PER-TOT
                         WS-REPORT-YTD-TOT.
           ACCEPT DATE-ACCEPT FROM DATE.
               MOVE MONTH-WS TO WS-RUN-DATE-MONTH-OUT.
               MOVE DAY-WS TO WS-RUN-DATE-DAY-OUT.
               MOVE YEAR-WS TO WS-RUN-DATE-YEAR-OUT.
           PERFORM C10-HEADINGS-PARA.
      *
       B20-PROCESS-PARA.
            IF WS-PLANT-CODE-IN-02 IS NOT EQUAL WS-PREV-PLANT
                PERFORM C15-L2-PLANT-CTRL-BRK-PARA
            END-IF.   
            IF WS-DEPT-NUM-IN-02 IS NOT EQUAL WS-PREV-DEPT
                PERFORM C20-L1-DEPT-CTRL-BRK-PARA
            END-IF.
            IF WS-DEPT-GRP-SWITCH = "Y"
                MOVE WS-DEPT-NUM-IN-02 TO WS-DEPT-NUM-OUT
                MOVE "N" TO WS-DEPT-GRP-SWITCH
            ELSE
                MOVE SPACES TO WS-DEPT-NUM-OUT
            END-IF.
            IF WS-PLANT-GRP-SWITCH = "Y"
                MOVE WS-PLANT-CODE-IN-02 TO WS-PLANT-CODE-OUT
                MOVE "N" TO WS-PLANT-GRP-SWITCH
            ELSE
                MOVE SPACES TO WS-PLANT-CODE-OUT
            END-IF.
            MOVE WS-PR-SSN-3-IN-02 TO WS-SSN-3-OUT.
            MOVE WS-PR-SSN-2-IN-02 TO WS-SSN-2-OUT.
            MOVE WS-PR-SSN-4-IN-02 TO WS-SSN-4-OUT.
            MOVE WS-PR-EMPL-NAME-IN-02 TO WS-EMPL-NAME-OUT.
            MOVE WS-EARN-THIS-PER-IN-02 TO WS-EARN-THIS-PER-OUT.
            MOVE WS-YTD-EARNINGS-IN-02 TO WS-YTD-EARN-OUT.
            MOVE SPACES TO ER-REPORT-OUT.
            WRITE ER-REPORT-OUT FROM WS-DETAIL-LINE-01
                AFTER ADVANCING 1 LINE.
            MOVE "N" TO WS-DEPT-GRP-SWITCH.
            ADD WS-EARN-THIS-PER-IN-02 TO WS-DEPT-PER-TOT.
            ADD WS-YTD-EARNINGS-IN-02 TO WS-DEPT-YTD-TOT.
            ADD 2 TO WS-LINES-USED.
            IF WS-LINES-USED > 65 THEN
                PERFORM C10-HEADINGS-PARA
                MOVE ZERO TO WS-LINES-USED
            END-IF.            


            READ PAYROLL-FILE-IN INTO WS-PR-PAYROLL-REC-IN-02
               AT END MOVE "Y" TO WS-EOF-SWITCH.
      *
       B30-TOTAL-PARA.
           PERFORM C15-L2-PLANT-CTRL-BRK-PARA.
           MOVE WS-REPORT-PER-TOT TO WS-TD-EARN-PER-OUT.
           MOVE WS-REPORT-YTD-TOT TO WS-TD-YTD-EARN-OUT.
           MOVE SPACES TO ER-REPORT-OUT.
           WRITE ER-REPORT-OUT FROM WS-REPORT-TOTAL
               AFTER ADVANCING 1 LINE.
           ADD 1 TO WS-LINES-USED.
           IF WS-LINES-USED > 65 THEN
               PERFORM C10-HEADINGS-PARA
               MOVE ZERO TO WS-LINES-USED
           END-IF. 
      *
       C10-HEADINGS-PARA.
           ADD 1 TO WS-PAGE-COUNT.
           MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
           WRITE ER-REPORT-OUT FROM WS-HEADINGS-01
               AFTER ADVANCING PAGE.
           WRITE ER-REPORT-OUT FROM WS-HEADINGS-02
               AFTER ADVANCING 1 LINE.
           WRITE ER-REPORT-OUT FROM WS-BLANK-LINE
               AFTER ADVANCING 1 LINE.
           WRITE ER-REPORT-OUT FROM WS-COLUMN-HEADING-01
               AFTER ADVANCING 1 LINES.
           WRITE ER-REPORT-OUT FROM WS-COLUMN-HEADING-02
               AFTER ADVANCING 1 LINE.
           WRITE ER-REPORT-OUT FROM WS-BLANK-LINE
               AFTER ADVANCING 1 LINE.
      *     ADD 6 TO WS-LINES-USED.
           MOVE 2 TO WS-SPACING. 
           MOVE "Y" TO WS-DEPT-GRP-SWITCH.
           MOVE "Y" TO WS-PLANT-GRP-SWITCH.
      *
       C15-L2-PLANT-CTRL-BRK-PARA.
           PERFORM C20-L1-DEPT-CTRL-BRK-PARA.
           ADD WS-PLANT-PER-TOT TO WS-REPORT-PER-TOT.
           ADD WS-PLANT-YTD-TOT TO WS-REPORT-YTD-TOT.
           MOVE SPACES TO ER-REPORT-OUT.
           MOVE WS-PLANT-PER-TOT TO WS-TP-EARN-PER-OUT.
           MOVE WS-PLANT-YTD-TOT TO WS-TP-YTD-EARN-OUT.
           MOVE WS-PREV-PLANT TO WS-PLANT-CODE-OUT
                                 WS-TP-PLANT-CODE-OUT.
           MOVE WS-REPORT-PER-TOT TO WS-RT-EARN-PER-OUT.
           MOVE WS-REPORT-YTD-TOT TO WS-RT-YTD-EARN-OUT.
           WRITE ER-REPORT-OUT FROM WS-TOTAL-PLANT
               AFTER ADVANCING 1 LINE.
           ADD 1 TO WS-LINES-USED.
           MOVE SPACES TO ER-REPORT-OUT.
           WRITE ER-REPORT-OUT
               AFTER ADVANCING 1 LINE.
           ADD 1 TO WS-LINES-USED.
           MOVE ZERO TO WS-DEPT-PER-TOT
                        WS-PLANT-PER-TOT
                        WS-PLANT-YTD-TOT.
           MOVE WS-PLANT-CODE-IN-02 TO WS-PREV-PLANT.
           MOVE WS-PREV-PLANT TO WS-PLANT-CODE-OUT.
           MOVE SPACES TO ER-REPORT-OUT.
           MOVE "Y" TO WS-PLANT-GRP-SWITCH.
      * 
       C20-L1-DEPT-CTRL-BRK-PARA.
           ADD WS-DEPT-PER-TOT TO WS-PLANT-PER-TOT.
           ADD WS-DEPT-YTD-TOT TO WS-PLANT-YTD-TOT.
           MOVE WS-DEPT-PER-TOT TO WS-TD-EARN-PER-OUT.
           MOVE WS-DEPT-YTD-TOT TO WS-TD-YTD-EARN-OUT.
           MOVE WS-PREV-DEPT TO WS-TOT-DEPT-OUT.
           MOVE SPACES TO ER-REPORT-OUT.
           WRITE ER-REPORT-OUT
               AFTER ADVANCING 1 LINE.
           ADD 1 TO WS-LINES-USED.
           WRITE ER-REPORT-OUT FROM WS-TOTAL-DEPT
               AFTER ADVANCING 1 LINE.
           ADD 1 TO WS-LINES-USED.
           MOVE SPACES TO ER-REPORT-OUT.
           WRITE ER-REPORT-OUT
               AFTER ADVANCING 1 LINE.
           ADD 1 TO WS-LINES-USED.
           MOVE ZERO TO WS-DEPT-PER-TOT
                        WS-DEPT-YTD-TOT.
           MOVE WS-DEPT-NUM-IN-02 TO WS-PREV-DEPT.
           MOVE "Y" TO WS-DEPT-GRP-SWITCH.    