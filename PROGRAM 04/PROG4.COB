       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROG4.
      *AUTHOR.        ERIC.
      *INSTALLATION.  WSC.
      *DATE-WRITTEN.  NOV 10,2002.
      *DATE-COMPILED. NOV 10,2002.
      *SECURITY.      NONE.
      *
      *
      *  THIS IS A STUDENT TEST RESULT
      *  VALIDATION PROGRAM. IT WILL
      *  PREPARE AN AUDIT/ERROR REPORT
      *  THAT LISTS EACH RECORD & ANY
      *  ERROR CODES FOR THE ERRORS IN
      *  THAT RECORD. IT WILL PLACE AN
      *  ASTERISK LINE UNDER THE FIELD
      *  CONTAINING AN ERROR.
      *
      *
       ENVIRONMENT DIVISION.
      *
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT TEST-RESULTS-FILE-IN
               ASSIGN TO "TW9A9B.DAT".
           SELECT ERROR-REPORT-OUT
               ASSIGN TO "PROG4_OUT.DAT".
      *
      *
      *
       DATA DIVISION.
      *
      *
       FILE SECTION.
      *
       FD  TEST-RESULTS-FILE-IN
               RECORD CONTAINS 80 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS TR-TEST-RESULTS-REC-IN.
      *
       01  TR-TEST-RESULTS-REC-IN         PIC X(80).
      *
       FD  ERROR-REPORT-OUT
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS ER-ERROR-REPORT-OUT.
      *
       01  ER-ERROR-REPORT-OUT            PIC X(132).
      *
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SWITCH              PIC X(3).
           05  WS-MAIN-ERROR-SWITCH       PIC X(3).
      *
       01  WS-PAGE-LAYOUT.
           05  WS-LINES-PER-PAGE          PIC 9(2) VALUE 57.
           05  WS-LINES-USED              PIC 9(2).
           05  WS-PAGE-COUNT              PIC 9(2).
           05  WS-SPACING                 PIC 9(2).
      *
       01  WS-ACCUMULATORS.
           05  WS-ERROR-A                 PIC 99.
           05  WS-ERROR-B                 PIC 99.
           05  WS-ERROR-C                 PIC 99.
           05  WS-ERROR-D                 PIC 99.
           05  WS-ERROR-E                 PIC 99.
           05  WS-ERROR-F                 PIC 99.
      *
       01  WS-TR-TEST-RESULTS-REC-IN.
           05  TR-REC-CODE-IN             PIC X(2).
           05  FILLER                     PIC X(3).
           05  TR-STUD-NAME-IN            PIC X(20).
           05  FILLER                     PIC X(2).
           05  TR-CORRECT-ANS-IN-X        PIC X(3).
           05  TR-CORRECT-ANS-IN-9 REDEFINES TR-CORRECT-ANS-IN-X
                                          PIC 9(3).
           05  FILLER                     PIC X(11).
           05  FILLER                     PIC X(36).
      *
       01  WS-HEADINGS-1.
           05  FILLER                     PIC X(7) VALUE "STUDENT".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "TEST".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "RESULT".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(10) VALUE "VALIDATION".
           05  FILLER                     PIC X(6) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "PAGE".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  WS-PAGE-OUT                PIC Z9.
           05  FILLER                     PIC X(91) VALUE SPACES.
      *
       01  WS-COLUMN-HEADING-01.
           05  FILLER                     PIC X(4) VALUE "REC.".
           05  FILLER                     PIC X(22) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "CORR.".
           05  FILLER                     PIC X(18) VALUE SPACES.
           05  FILLER                     PIC X(83) VALUE SPACES.
      *
       01  WS-COLUMN-HEADING-02.
           05  FILLER                     PIC X(4) VALUE "CODE".
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  FILLER                     PIC X(7) VALUE "STUDENT".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "NAME".
           05  FILLER                     PIC X(8) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "ANS.".
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "ERROR".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "CODES".
           05  FILLER                     PIC X(5) VALUE SPACES.
           05  FILLER                     PIC X(83) VALUE SPACES.
      *
       01  WS-BLANK-LINE                  PIC X(132) VALUE SPACES.
      *
       01  WS-DETAIL-LINE-01.
           05  WS-REC-CODE-OUT            PIC X(2).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-STUD-NAME-OUT           PIC X(20).
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-CORRECT-ANS-OUT-X       PIC X(3).
           05  WS-CORRECT-ANS-OUT-9 REDEFINES WS-CORRECT-ANS-OUT-X
                                          PIC 9(3).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  ERROR-CODES-OUT.
               10  WS-ERROR-CODE-A-OUT    PIC X(1).       
               10  FILLER                 PIC X(1) VALUE SPACES.
               10  WS-ERROR-CODE-B-OUT    PIC X(1).
               10  FILLER                 PIC X(1) VALUE SPACES.
               10  WS-ERROR-CODE-C-OUT    PIC X(1).
               10  FILLER                 PIC X(1) VALUE SPACES.
               10  WS-ERROR-CODE-D-OUT    PIC X(1).
               10  FILLER                 PIC X(1) VALUE SPACES.
               10  WS-ERROR-CODE-E-OUT    PIC X(1).
               10  FILLER                 PIC X(1) VALUE SPACES.
               10  WS-ERROR-CODE-F-OUT    PIC X(1).
           05  FILLER                     PIC X(88) VALUE SPACES.
      *
       01  WS-DETAIL-LINE-02.
           05  WS-CODE-AST-OUT            PIC X(2).
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-NAME-AST-OUT            PIC X(20).
           05  FILLER                     PIC X(2) VALUE SPACES.
           05  WS-CORRECT-AST-OUT         PIC X(3).
           05  FILLER                     PIC X(102) VALUE SPACES.
      *
       01  WS-DETAIL-TOT-LINE-01.
           05  FILLER                     PIC X(10) VALUE SPACES.
           05  FILLER                     PIC X(5) VALUE "TOTAL".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "A".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(4) VALUE "TYPE".
           05  FILLER                     PIC X(1) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "ERRORS".
           05  FILLER                     PIC X(3) VALUE SPACES.
           05  WS-ERROR-A-OUT             PIC Z9.
           05  FILLER                     PIC X(98) VALUE SPACES.
      *
       01  WS-DETAIL-TOT-LINE-02.
           05  FILLER                     PIC X(16) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "B".
           05  FILLER                     PIC X(15) VALUE SPACES.
           05  WS-ERROR-B-OUT             PIC Z9.
           05  FILLER                     PIC X(98) VALUE SPACES.
      *
       01  WS-DETAIL-TOT-LINE-03.
           05  FILLER                     PIC X(16) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "C".
           05  FILLER                     PIC X(15) VALUE SPACES.
           05  WS-ERROR-C-OUT             PIC Z9.
           05  FILLER                     PIC X(98) VALUE SPACES.
      *
       01  WS-DETAIL-TOT-LINE-04.
           05  FILLER                     PIC X(16) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "D".
           05  FILLER                     PIC X(15) VALUE SPACES.
           05  WS-ERROR-D-OUT             PIC Z9.
           05  FILLER                     PIC X(98) VALUE SPACES.
      *
       01  WS-DETAIL-TOT-LINE-05.
           05  FILLER                     PIC X(16) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "E".
           05  FILLER                     PIC X(15) VALUE SPACES.
           05  WS-ERROR-E-OUT             PIC Z9.
           05  FILLER                     PIC X(98) VALUE SPACES.
      *
       01  WS-DETAIL-TOT-LINE-06.
           05  FILLER                     PIC X(16) VALUE SPACES.
           05  FILLER                     PIC X(1) VALUE "F".
           05  FILLER                     PIC X(15) VALUE SPACES.
           05  WS-ERROR-F-OUT             PIC Z9.
           05  FILLER                     PIC X(98) VALUE SPACES.
      *
      *
      *
       PROCEDURE DIVISION.
      *
      *
       A00-MAIN-LINE-PARA.
           OPEN INPUT TEST-RESULTS-FILE-IN
                OUTPUT ERROR-REPORT-OUT.
           PERFORM B10-INIT-PARA.
           READ TEST-RESULTS-FILE-IN INTO WS-TR-TEST-RESULTS-REC-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH.
           PERFORM B20-PROCESS-PARA
               UNTIL WS-EOF-SWITCH = "YES".
           PERFORM B30-TOTAL-PARA.
           CLOSE TEST-RESULTS-FILE-IN
                 ERROR-REPORT-OUT.
           STOP RUN.
      *
      *
       B10-INIT-PARA.
      *
           MOVE "NO" TO WS-EOF-SWITCH
                        WS-MAIN-ERROR-SWITCH.


           MOVE ZEROS TO WS-PAGE-COUNT
                         WS-SPACING
                         WS-ERROR-A
                         WS-ERROR-B
                         WS-ERROR-C
                         WS-ERROR-D
                         WS-ERROR-E
                         WS-ERROR-F.
      *     ADD 1 TO WS-LINES-PER-PAGE GIVING WS-LINES-USED.
           PERFORM C10-HEADINGS-PARA.
      *
       B20-PROCESS-PARA.
           MOVE SPACES TO WS-DETAIL-LINE-01
                          WS-DETAIL-LINE-02.
           MOVE "NO" TO WS-MAIN-ERROR-SWITCH.


           MOVE SPACES TO WS-ERROR-CODE-A-OUT
                          WS-ERROR-CODE-B-OUT
                          WS-ERROR-CODE-C-OUT
                          WS-ERROR-CODE-D-OUT
                          WS-ERROR-CODE-E-OUT
                          WS-ERROR-CODE-F-OUT.


           MOVE SPACES TO WS-CODE-AST-OUT
                          WS-NAME-AST-OUT
                          WS-CORRECT-AST-OUT.


           IF TR-REC-CODE-IN IS NOT EQUAL TO "52" THEN
               MOVE "YES" TO WS-MAIN-ERROR-SWITCH
               MOVE "A" TO WS-ERROR-CODE-A-OUT
               MOVE ALL "*" TO WS-CODE-AST-OUT
               ADD 1 TO WS-ERROR-A
               MOVE WS-ERROR-A TO WS-ERROR-A-OUT
           END-IF.
           MOVE TR-REC-CODE-IN TO WS-REC-CODE-OUT.


           IF TR-STUD-NAME-IN IS EQUAL TO SPACES THEN
               MOVE "YES" TO WS-MAIN-ERROR-SWITCH
               MOVE "B" TO WS-ERROR-CODE-B-OUT
               MOVE ALL "*" TO WS-NAME-AST-OUT
               ADD 1 TO WS-ERROR-B
               MOVE WS-ERROR-B TO WS-ERROR-B-OUT
           ELSE IF TR-STUD-NAME-IN(1:1) IS EQUAL TO SPACES THEN
               MOVE "YES" TO WS-MAIN-ERROR-SWITCH
               MOVE "C" TO WS-ERROR-CODE-C-OUT
               MOVE ALL "*" TO WS-NAME-AST-OUT
               ADD 1 TO WS-ERROR-C
               MOVE WS-ERROR-C TO WS-ERROR-C-OUT
           END-IF.
           MOVE TR-STUD-NAME-IN TO WS-STUD-NAME-OUT.


           IF TR-CORRECT-ANS-IN-X IS EQUAL TO SPACES THEN
               MOVE "YES" TO WS-MAIN-ERROR-SWITCH
               MOVE "D" TO WS-ERROR-CODE-D-OUT
               MOVE SPACES TO WS-CORRECT-ANS-OUT-X
               MOVE ALL "*" TO WS-CORRECT-AST-OUT
               ADD 1 TO WS-ERROR-D
               MOVE WS-ERROR-D TO WS-ERROR-D-OUT
           ELSE IF TR-CORRECT-ANS-IN-9 IS NOT NUMERIC THEN
               MOVE "YES" TO WS-MAIN-ERROR-SWITCH
               MOVE "E" TO WS-ERROR-CODE-E-OUT
               MOVE ALL "*" TO WS-CORRECT-AST-OUT
               ADD 1 TO WS-ERROR-E
               MOVE WS-ERROR-E TO WS-ERROR-E-OUT
               MOVE TR-CORRECT-ANS-IN-X TO WS-CORRECT-ANS-OUT-X
           ELSE IF TR-CORRECT-ANS-IN-9 > 50
           OR TR-CORRECT-ANS-IN-9 < 0 THEN
               MOVE "YES" TO WS-MAIN-ERROR-SWITCH
               MOVE "F" TO WS-ERROR-CODE-F-OUT
               MOVE TR-CORRECT-ANS-IN-9 TO WS-CORRECT-ANS-OUT-9
               MOVE ALL "*" TO WS-CORRECT-AST-OUT
               ADD 1 TO WS-ERROR-F
               MOVE WS-ERROR-F TO WS-ERROR-F-OUT
           ELSE
               MOVE TR-CORRECT-ANS-IN-9 TO WS-CORRECT-ANS-OUT-9
           END-IF.             
           
           IF WS-MAIN-ERROR-SWITCH = "YES" THEN
               WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-LINE-01
               WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-LINE-02
               WRITE ER-ERROR-REPORT-OUT FROM WS-BLANK-LINE
                   AFTER ADVANCING WS-SPACING
               ADD 3 TO WS-LINES-USED
           ELSE
               WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-LINE-01
               WRITE ER-ERROR-REPORT-OUT FROM WS-BLANK-LINE
                   AFTER ADVANCING WS-SPACING
               ADD 2 TO WS-LINES-USED
           END-IF.


           ADD WS-SPACING TO WS-LINES-USED.


           IF WS-LINES-USED >= 60 THEN
               MOVE ZERO TO WS-LINES-USED
               PERFORM C10-HEADINGS-PARA
           END-IF.


           READ TEST-RESULTS-FILE-IN INTO WS-TR-TEST-RESULTS-REC-IN
               AT END MOVE "YES" TO WS-EOF-SWITCH.
      *
       B30-TOTAL-PARA.
           WRITE ER-ERROR-REPORT-OUT FROM WS-BLANK-LINE
               AFTER ADVANCING 2 LINES.
           WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-TOT-LINE-01
               AFTER ADVANCING 1 LINE.
           WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-TOT-LINE-02
               AFTER ADVANCING 1 LINE.
           WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-TOT-LINE-03
               AFTER ADVANCING 1 LINE.
           WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-TOT-LINE-04
               AFTER ADVANCING 1 LINE.
           WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-TOT-LINE-05
               AFTER ADVANCING 1 LINE.
           WRITE ER-ERROR-REPORT-OUT FROM WS-DETAIL-TOT-LINE-06
               AFTER ADVANCING 1 LINE.
      *
       C10-HEADINGS-PARA.
           ADD 1 TO WS-PAGE-COUNT.
           MOVE WS-PAGE-COUNT TO WS-PAGE-OUT.
           WRITE ER-ERROR-REPORT-OUT FROM WS-HEADINGS-1
               AFTER ADVANCING PAGE.
           WRITE ER-ERROR-REPORT-OUT FROM WS-COLUMN-HEADING-01
               AFTER ADVANCING 2 LINES.
           WRITE ER-ERROR-REPORT-OUT FROM WS-COLUMN-HEADING-02
               AFTER ADVANCING 1 LINE.
           WRITE ER-ERROR-REPORT-OUT FROM WS-BLANK-LINE
               AFTER ADVANCING 1 LINE.
           ADD 5 TO WS-LINES-USED.
           MOVE 2 TO WS-SPACING. 