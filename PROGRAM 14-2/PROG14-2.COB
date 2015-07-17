       IDENTIFICATION DIVISION.
      *
      *
       PROGRAM-ID.    PROG14-2.
      *AUTHOR.        ERIC.
      *INSTALLATION.  WSC.
      *DATE-WRITTEN.  MAR 27,2003.
      *DATE-COMPILED. MAR 27,2003.
      *SECURITY.      NONE.
      *
      *
      *
      * THIS PROGRAM READS AN INPUT FILE AND SORTS IT GIVING
      * A SORTED STUDENT FILE. THE SORTED FILE IS SORTED IN
      * ASCENDING ORDER ON THE STUDENT NUMBER FIELD. 
      *
      * NEXT, READ EACH INPUT STUDENT COURSE RECORD-ASSUME
      * THIS FILE IS IN SEQUENCE ON THE STUDENT NUMBER FIELD.
      * FOR EACH TRANSACTION WITH A CORRESPONDING MASTER:
      * 
      * A. CALCULATE GRADE POINTS BY MULTIPLYING UNITS BY:
      *    4 - GRADE OF A
      *    3 - GRADE OF B
      *    2 - GRADE OF C
      *    1 - GRADE OF D
      * B. ADD UNITS TO UNITS-COMPLETED OF MASTER. ADD
      *    CALCULATED GRADE POINTS TO GRADE-POINTS OF MASTER.
      * C. FOR A GRADE F, ADD UNITS TO UNITS-COMPLETED OF MASTER,
      *    BUT LEAVE GRADE-POINTS OF MASTER UNCHANGED.
      * D. FOR A GRADE W, DO NO CALCULATIONS.
      *
      * WRITE EACH UNMATCHED TRANSACTION RECORD TO AN ERROR LOG FILE.
      *
      * PRINT A COURSE REPORT FOR EACH STUDENT, USING DATA FROM
      * BOTH THE INPUT MASTER AND COURSE FILES. EACH GRADE REPORT
      * SHOULD BE PRINTED ON A NEW PAGE.
      *
      *
      *
       ENVIRONMENT DIVISION.
      *
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT MASTER-FILE-IN
               ASSIGN TO "STUDENT.DAT".
           SELECT SORTED-MASTER-FILE
               ASSIGN TO "SORTED-MASTER.DAT".
           SELECT TRANSACTION-FILE
               ASSIGN TO "STUTRANS.DAT".
           SELECT GRADE-REPORT
               ASSIGN TO "PROG14-2_OUT.DAT".
           SELECT NEW-MASTER-FILE
               ASSIGN TO "NEW-MASTER.DAT".
           SELECT ERROR-LOG-FILE
               ASSIGN TO "ERROR-LOG.DAT".
           SELECT SORT-WORK-FILE
               ASSIGN TO "SORT-WORK.DAT".
      *
      *
      *
       DATA DIVISION.
      *
      *
       FILE SECTION.
      *
       FD  MASTER-FILE-IN
               RECORD CONTAINS 70 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS MASTER-REC-IN.
      *
       01  MASTER-REC-IN                PIC X(70).
      *
       FD  SORTED-MASTER-FILE
               RECORD CONTAINS 70 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS SORTED-MASTER-REC.
      *
       01  SORTED-MASTER-REC            PIC X(70).
      *
       FD  TRANSACTION-FILE
               RECORD CONTAINS 46 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS TRANSACTION-REC-IN.
      *
       01  TRANSACTION-REC-IN           PIC X(46).
      *
       FD  GRADE-REPORT
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS GRADE-REPORT-OUT.
      *
       01  GRADE-REPORT-OUT             PIC X(132).
      *
       FD  NEW-MASTER-FILE
               RECORD CONTAINS 70 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS NEW-MASTER-REC.
      *
       01  NEW-MASTER-REC               PIC X(70).
      *
       FD  ERROR-LOG-FILE
               RECORD CONTAINS 132 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS ERROR-LOG-OUT.
      *
       01  ERROR-LOG-OUT                PIC X(132).
      *
       SD  SORT-WORK-FILE
             DATA RECORD IS SORT-WORK-REC.
      *
       01  SORT-WORK-REC.
           05  FILLER                   PIC X(2).
           05  SR-STUD-ID-SW            PIC X(9).
           05  SR-FILLER                PIC X(59).
      *
      *
      *
       WORKING-STORAGE SECTION.
      *
      *
       01  WS-SWITCHES.
           05  MASTER-EOF-SW            PIC X.
           05  TRANS-EOF-SW             PIC X.
           05  GET-NEW-MASTER-SW        PIC X.
           05  GET-NEW-TRANS-SW         PIC X.
           05  INCOMP-GRADE-SW          PIC X.
      *
       01  WS-HOLD-AREA.
           05  MASTER-HOLD              PIC X(9).
           05  TRANS-HOLD               PIC X(9).
      *
       01  WS-PAGE-LAYOUT.
           05  WS-SPACING               PIC 9 VALUE 2.
      *
       01  WS-DATE-ACCEPT.
           05  DATE-ACCEPT              PIC X(6).
           05  DATE-WS REDEFINES DATE-ACCEPT.
               10  WS-YEAR              PIC X(2).
               10  WS-MONTH             PIC X(2).
               10  WS-DAY               PIC X(2).
      *
       01  WS-CALCULATIONS.
           05  CURR-SEM-CREDITS         PIC 9(3).
           05  CURR-SEM-GPA             PIC 9.99.
           05  CURR-SEM-GP              PIC 9(3).
           05  PREV-SEM-CREDITS         PIC 9(3).
           05  PREV-SEM-GPA             PIC 9.99.
           05  PREV-SEM-GP              PIC 9(3).
           05  UTOT-SEM-CREDITS         PIC 9(3).
           05  UTOT-SEM-GPA             PIC 9.99.
           05  UTOT-SEM-GP              PIC 9(3).
           05  GP-ACCUM                 PIC 9(3).
      *
       01  WS-SORTED-MASTER-REC.
           05  FILLER                   PIC X(2).
           05  STUD-ID-SM.
               10  STUD-ID-3-SM         PIC X(3).
               10  STUD-ID-2-SM         PIC X(2).
               10  STUD-ID-4-SM         PIC X(4).
           05  LNAME-SM                 PIC X(14).
           05  FNAME-SM                 PIC X(10).
           05  FILLER                   PIC X(9).
           05  GP-SM                    PIC 9(3).
           05  CREDITS-SM               PIC 9(3).
           05  FILLER                   PIC X(20).
      *
       01  WS-NEW-MASTER-REC.
           05  FILLER                   PIC X(2).
           05  STUD-ID-NM.
               10  STUD-ID-3-NM         PIC X(3).
               10  STUD-ID-2-NM         PIC X(2).
               10  STUD-ID-4-NM         PIC X(4).
           05  LNAME-NM                 PIC X(14).
           05  FNAME-NM                 PIC X(10).
           05  FILLER                   PIC X(9).
           05  GP-NM                    PIC 9(3).
           05  CREDITS-NM               PIC 9(3).
           05  FILLER                   PIC X(20).
      *
       01  WS-TRANSACTION-REC.
           05  FILLER                   PIC X(2).
           05  STUD-ID-TF               PIC X(9).
           05  REC-COUNT-TF             PIC 9.
           05  DEPT-TF                  PIC X(4).
           05  COURSE-NUM-TF            PIC X(4).
           05  COURSE-DESC-TF           PIC X(24).
           05  GRADE-TF                 PIC X.
           05  CREDITS-TF               PIC 9.
      *
       01  WS-GR-LINE-01.
           05  FILLER                   PIC X(19) VALUE SPACES.
           05  FILLER                   PIC X(7) VALUE "BAYVIEW".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(9) VALUE "INSTITUTE".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(2) VALUE "OF".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(8) VALUE "COMPUTER".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(10) VALUE "TECHNOLOGY".
           05  FILLER                   PIC X(73) VALUE SPACES.
      *
       01  WS-GR-LINE-02.
           05  FILLER                   PIC X(27) VALUE SPACES.
           05  FILLER                   PIC X(7) VALUE "STUDENT".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(5) VALUE "GRADE".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(6) VALUE "REPORT".
           05  FILLER                   PIC X(85) VALUE SPACES.
      *
       01  WS-GR-LINE-03.
           05  FILLER                   PIC X(7) VALUE "STUDENT".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(7) VALUE "NUMBER:".
           05  FILLER                   PIC X VALUE SPACE.
           05  STUD-ID-3-GR-OUT         PIC X(3).
           05  FILLER                   PIC X VALUE "-".
           05  STUD-ID-2-GR-OUT         PIC X(2).
           05  FILLER                   PIC X VALUE "-".
           05  STUD-ID-4-GR-OUT         PIC X(4).
           05  FILLER                   PIC X(28) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "DATE: ".
           05  RUN-DATE-GR-OUT.
               10  MONTH-GR-OUT         PIC X(2).
               10  FILLER               PIC X VALUE "/".
               10  DAY-GR-OUT           PIC X(2).
               10  FILLER               PIC X VALUE "/".
               10  YEAR-GR-OUT          PIC X(2).
           05  FILLER                   PIC X(63) VALUE SPACES.
      *
       01  WS-GR-LINE-04.
           05  FILLER                   PIC X(8) VALUE "STUDENT:".
           05  FILLER                   PIC X VALUE SPACE.
           05  LNAME-GR-OUT             PIC X(14).
           05  FILLER                   PIC X(2) VALUE SPACES.
           05  FNAME-GR-OUT             PIC X(10).
           05  FILLER                   PIC X(97) VALUE SPACES.
      *
       01  WS-GR-LINE-05.
           05  FILLER                   PIC X(7) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "COURSE".
           05  FILLER                   PIC X(51) VALUE SPACES.
           05  FILLER                   PIC X(5) VALUE "GRADE".
           05  FILLER                   PIC X(63) VALUE SPACES.
      *
       01  WS-GR-LINE-06.
           05  FILLER                   PIC X(5) VALUE "DEPT.".
           05  FILLER                   PIC X(2) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "NUMBER".
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  FILLER                   PIC X(11) VALUE "DESCRIPTION".
           05  FILLER                   PIC X(17) VALUE SPACES.
           05  FILLER                   PIC X(5) VALUE "UNITS".
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  FILLER                   PIC X(5) VALUE "GRADE".
           05  FILLER                   PIC X(5) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "POINTS".
           05  FILLER                   PIC X(62) VALUE SPACES.
      *
       01  WS-GR-DL-LINE-07.
           05  DEPT-GR                  PIC X(4).
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  COURSE-NUM-GR            PIC X(4).
           05  FILLER                   PIC X(5) VALUE SPACES.
           05  COURSE-DESC-GR           PIC X(24).
           05  FILLER                   PIC X(6) VALUE SPACES.
           05  CREDITS-GR               PIC 9.
           05  FILLER                   PIC X(8) VALUE SPACES.
           05  GRADE-GR                 PIC X.
           05  FILLER                   PIC X(8) VALUE SPACES.
           05  GP-GR                    PIC ZZ9.
           05  FILLER                   PIC X(64) VALUE SPACES.
      *
       01  WS-GR-LINE-08.
           05  FILLER                   PIC X(55) VALUE SPACES.
           05  FILLER                   PIC X(3) VALUE "GPA".
           05  FILLER                   PIC X(74) VALUE SPACES.
      *
       01  WS-GR-TOT-LINE-09.
           05  FILLER                   PIC X(20) VALUE SPACES.
           05  FILLER                   PIC X(7) VALUE "CURRENT".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(8) VALUE "SEMESTER".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(6) VALUE "TOTALS".
           05  FILLER                   PIC X(2) VALUE SPACES.
           05  CURR-SEM-CREDITS-OUT     PIC ZZ9.
           05  FILLER                   PIC X(7) VALUE SPACES.
           05  CURR-SEM-GPA-OUT         PIC Z.99.
           05  FILLER                   PIC X(6) VALUE SPACES.
           05  CURR-SEM-GP-OUT          PIC Z99.
           05  FILLER                   PIC X(64) VALUE SPACES.
      *
       01  WS-GR-TOT-LINE-10.
           05  FILLER                   PIC X(20) VALUE SPACES.
           05  FILLER                   PIC X(8) VALUE "PREVIOUS".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(8) VALUE "SEMESTER".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(6) VALUE "TOTALS".
           05  FILLER                   PIC X VALUE SPACE.
           05  PREV-SEM-CREDITS-OUT     PIC ZZ9.
           05  FILLER                   PIC X(7) VALUE SPACES.
           05  PREV-SEM-GPA-OUT         PIC Z.99.
           05  FILLER                   PIC X(6) VALUE SPACES.
           05  PREV-SEM-GP-OUT          PIC ZZ9.
           05  FILLER                   PIC X(64) VALUE SPACES.
      *
       01  WS-GR-TOT-LINE-11.
           05  FILLER                   PIC X(20) VALUE SPACES.
           05  FILLER                   PIC X(7) VALUE "UPDATED".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(6) VALUE "TOTALS".
           05  FILLER                   PIC X(11) VALUE SPACES.
           05  UPDATED-CREDITS-OUT      PIC ZZ9.
           05  FILLER                   PIC X(7) VALUE SPACES.
           05  UPDATED-GPA-OUT          PIC Z.99.
           05  FILLER                   PIC X(6) VALUE SPACES.
           05  UPDATED-GP-OUT           PIC ZZ9.
           05  FILLER                   PIC X(64) VALUE SPACES.
      *
       01  WS-EL-LINE-01.
           05  FILLER                   PIC X(19) VALUE SPACES.
           05  FILLER                   PIC X(7) VALUE "BAYVIEW".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(9) VALUE "INSTITUTE".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(2) VALUE "OF".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(8) VALUE "COMPUTER".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(10) VALUE "TECHNOLOGY".
           05  FILLER                   PIC X(73) VALUE SPACES.
      *
       01  WS-EL-LINE-02.
           05  FILLER                   PIC X(32) VALUE SPACES.
           05  FILLER                   PIC X(5) VALUE "ERROR".
           05  FILLER                   PIC X(2) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "REPORT".
           05  FILLER                   PIC X(87) VALUE SPACES.
      *
       01  WS-EL-LINE-03.
           05  FILLER                   PIC X(6) VALUE "NAME: ".
           05  FILLER                   PIC X(10) VALUE "ERIC ESTES".
           05  FILLER                   PIC X(39) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "DATE: ".
           05  RUN-DATE-EL-OUT.
               10  MONTH-EL-OUT         PIC X(2).
               10  FILLER               PIC X VALUE "/".
               10  DAY-EL-OUT           PIC X(2).
               10  FILLER               PIC X VALUE "/".
               10  YEAR-EL-OUT          PIC X(2).
           05  FILLER                   PIC X(63) VALUE SPACES.
      *
       01  WS-EL-LINE-04.
           05  FILLER                   PIC X(7) VALUE "STUDENT".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(6) VALUE "NUMBER".
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  FILLER                   PIC X(5) VALUE "DEPT.". 
           05  FILLER                   PIC X(9) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "COURSE".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(5) VALUE "DESC.".
           05  FILLER                   PIC X(11) VALUE SPACES.
           05  FILLER                   PIC X(5) VALUE "ERROR".
           05  FILLER                   PIC X VALUE SPACE.
           05  FILLER                   PIC X(7) VALUE "MESSAGE".
           05  FILLER                   PIC X(64) VALUE SPACES.
      *
       01  WS-EL-DL-LINE-05.
           05  FILLER                   PIC X VALUE SPACE.
           05  STUD-ID-EL.
               10  STUD-ID-3-EL         PIC X(3).
               10  FILLER               PIC X VALUE "-".
               10  STUD-ID-2-EL         PIC X(2).
               10  FILLER               PIC X VALUE "-".
               10  STUD-ID-4-EL         PIC X(4).
           05  FILLER                   PIC X(6) VALUE SPACES.
           05  DEPT-EL                  PIC X(4).
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  COURSE-DESC-EL           PIC X(24).
           05  FILLER                   PIC X(5) VALUE SPACES.
           05  FILLER                   PIC X(12) VALUE "BAD STUD. ID".
           05  FILLER                   PIC X(66) VALUE SPACES.
      *
      *
      *
       PROCEDURE DIVISION.
      *
      *
       A00-MAINLINE-PARA.
           PERFORM B30-SORT-PARA.
           OPEN INPUT  SORTED-MASTER-FILE
                       TRANSACTION-FILE
                OUTPUT GRADE-REPORT
                       NEW-MASTER-FILE
                       ERROR-LOG-FILE.
           PERFORM B10-INIT-PARA.
           PERFORM B20-PROCESS-PARA
               UNTIL MASTER-EOF-SW = "Y" AND TRANS-EOF-SW = "Y".
           CLOSE SORTED-MASTER-FILE
                 TRANSACTION-FILE
                 GRADE-REPORT
                 ERROR-LOG-FILE
                 NEW-MASTER-FILE.
           STOP RUN.
      *
      *
       B10-INIT-PARA.
           ACCEPT DATE-ACCEPT FROM DATE.
           MOVE WS-MONTH TO MONTH-GR-OUT.
           MOVE WS-DAY TO DAY-GR-OUT.
           MOVE WS-YEAR TO YEAR-GR-OUT.
           MOVE ZERO TO CURR-SEM-CREDITS.
           MOVE ZERO TO CURR-SEM-GPA.
           MOVE ZERO TO CURR-SEM-GP.
           MOVE ZERO TO PREV-SEM-CREDITS.
           MOVE ZERO TO PREV-SEM-GPA.
           MOVE ZERO TO PREV-SEM-GP.
           MOVE ZERO TO UTOT-SEM-CREDITS.
           MOVE ZERO TO UTOT-SEM-GPA.
           MOVE ZERO TO UTOT-SEM-GP.
           MOVE "N" TO INCOMP-GRADE-SW.
           MOVE "N" TO MASTER-EOF-SW.
           MOVE "N" TO TRANS-EOF-SW.
           MOVE "Y" TO GET-NEW-MASTER-SW.
           MOVE "Y" TO GET-NEW-TRANS-SW.
           PERFORM C20-ERROR-LOG-HEADINGS-PARA.
      *
      *
       B20-PROCESS-PARA.
           PERFORM B25-READ-PARA.
           IF MASTER-HOLD IS NOT EQUAL TO "999999999" AND
               TRANS-HOLD IS NOT EQUAL TO "999999999" THEN
               IF MASTER-HOLD = TRANS-HOLD THEN
                   MOVE "Y" TO GET-NEW-TRANS-SW
                   MOVE "Y" TO INCOMP-GRADE-SW
                   IF REC-COUNT-TF = 1 THEN
                       MOVE STUD-ID-3-SM TO STUD-ID-3-GR-OUT
                       MOVE STUD-ID-2-SM TO STUD-ID-2-GR-OUT
                       MOVE STUD-ID-4-SM TO STUD-ID-4-GR-OUT
                       MOVE LNAME-SM TO LNAME-GR-OUT
                       MOVE FNAME-SM TO FNAME-GR-OUT
                       PERFORM C10-GRADE-REPORT-HEADINGS-PARA
                   END-IF



                   MOVE CREDITS-SM TO PREV-SEM-CREDITS-OUT
                   MOVE GP-SM TO PREV-SEM-GP-OUT
                   COMPUTE PREV-SEM-GPA ROUNDED = GP-SM/CREDITS-SM



                   EVALUATE GRADE-TF
                       WHEN "A"
                           COMPUTE CURR-SEM-GP = CREDITS-TF * 4
                           ADD CREDITS-TF TO CURR-SEM-CREDITS
                           ADD CURR-SEM-GP TO GP-ACCUM
                           MOVE CURR-SEM-GP TO GP-GR
                       WHEN "B"
                           COMPUTE CURR-SEM-GP = CREDITS-TF * 3
                           ADD CREDITS-TF TO CURR-SEM-CREDITS
                           ADD CURR-SEM-GP TO GP-ACCUM
                           MOVE CURR-SEM-GP TO GP-GR
                       WHEN "C"
                           COMPUTE CURR-SEM-GP = CREDITS-TF * 2
                           ADD CREDITS-TF TO CURR-SEM-CREDITS
                           ADD CURR-SEM-GP TO GP-ACCUM
                           MOVE CURR-SEM-GP TO GP-GR
                       WHEN "D"
                           COMPUTE CURR-SEM-GP = CREDITS-TF * 1
                           ADD CREDITS-TF TO CURR-SEM-CREDITS
                           ADD CURR-SEM-GP TO GP-ACCUM
                           MOVE CURR-SEM-GP TO GP-GR
                       WHEN "F"
                           ADD CREDITS-TF TO CURR-SEM-CREDITS
                           MOVE ZEROS TO GP-GR
                       WHEN "W"
                           CONTINUE
                   END-EVALUATE


                   MOVE PREV-SEM-GPA TO PREV-SEM-GPA-OUT
                   MOVE SORTED-MASTER-REC TO WS-NEW-MASTER-REC
                   MOVE DEPT-TF TO DEPT-GR
                   MOVE COURSE-NUM-TF TO COURSE-NUM-GR
                   MOVE COURSE-DESC-TF TO COURSE-DESC-GR
                   MOVE CREDITS-TF TO CREDITS-GR
                   MOVE GRADE-TF TO GRADE-GR
                   WRITE GRADE-REPORT-OUT FROM WS-GR-DL-LINE-07
               END-IF
           END-IF.


           IF MASTER-HOLD < TRANS-HOLD THEN
               IF INCOMP-GRADE-SW = "Y" THEN
                  MOVE CURR-SEM-CREDITS TO CURR-SEM-CREDITS-OUT
                  MOVE GP-ACCUM TO CURR-SEM-GP-OUT
                  COMPUTE CURR-SEM-GPA ROUNDED
                      = GP-ACCUM/CURR-SEM-CREDITS
                  MOVE CURR-SEM-GPA TO CURR-SEM-GPA-OUT
                  ADD CURR-SEM-CREDITS TO CREDITS-SM GIVING
                      UTOT-SEM-CREDITS
                  MOVE UTOT-SEM-CREDITS TO UPDATED-CREDITS-OUT
                  ADD GP-ACCUM TO GP-SM GIVING UPDATED-GP-OUT
                  COMPUTE UTOT-SEM-GPA ROUNDED =
                      (GP-ACCUM + GP-SM)/UTOT-SEM-CREDITS
                  MOVE UTOT-SEM-GPA TO UPDATED-GPA-OUT


                  MOVE SPACES TO GRADE-REPORT-OUT
                  WRITE GRADE-REPORT-OUT
                  WRITE GRADE-REPORT-OUT FROM WS-GR-LINE-08
                  MOVE SPACES TO GRADE-REPORT-OUT
                  WRITE GRADE-REPORT-OUT
                  WRITE  GRADE-REPORT-OUT FROM WS-GR-TOT-LINE-09
                  MOVE SPACES TO GRADE-REPORT-OUT
                  WRITE GRADE-REPORT-OUT
                  WRITE GRADE-REPORT-OUT FROM WS-GR-TOT-LINE-10
                  MOVE SPACES TO GRADE-REPORT-OUT
                  WRITE GRADE-REPORT-OUT
                  WRITE GRADE-REPORT-OUT FROM WS-GR-TOT-LINE-11
               END-IF
               MOVE "N" TO INCOMP-GRADE-SW
               MOVE "Y" TO GET-NEW-MASTER-SW
               MOVE SORTED-MASTER-REC TO WS-NEW-MASTER-REC
               WRITE NEW-MASTER-REC FROM WS-NEW-MASTER-REC
               MOVE ZEROS TO CURR-SEM-CREDITS
               MOVE ZEROS TO GP-ACCUM
               MOVE ZEROS TO CURR-SEM-GPA
            END-IF.


            IF MASTER-HOLD > TRANS-HOLD THEN
               MOVE "Y" TO GET-NEW-TRANS-SW
               MOVE STUD-ID-TF TO STUD-ID-EL
               MOVE DEPT-TF TO DEPT-EL
               MOVE COURSE-DESC-TF TO COURSE-DESC-EL
               WRITE ERROR-LOG-OUT FROM WS-EL-DL-LINE-05
            END-IF.
      *
      *
       B25-READ-PARA.
           IF GET-NEW-MASTER-SW = "Y" THEN
               MOVE "N" TO GET-NEW-MASTER-SW
               READ SORTED-MASTER-FILE INTO WS-SORTED-MASTER-REC
                   AT END
                       MOVE "Y" TO MASTER-EOF-SW
                       MOVE ALL "9" TO MASTER-HOLD
                   NOT AT END
                       MOVE STUD-ID-SM TO MASTER-HOLD
               END-READ
           END-IF.


           IF GET-NEW-TRANS-SW = "Y" THEN
               MOVE "N" TO GET-NEW-TRANS-SW
               READ TRANSACTION-FILE INTO WS-TRANSACTION-REC
                   AT END
                       MOVE "Y" TO TRANS-EOF-SW
                       MOVE ALL "9" TO TRANS-HOLD
                   NOT AT END
                       MOVE STUD-ID-TF TO TRANS-HOLD
               END-READ
           END-IF.
      *
      *
       B30-SORT-PARA.
           SORT SORT-WORK-FILE
               ASCENDING KEY SR-STUD-ID-SW
                   USING  MASTER-FILE-IN
                   GIVING SORTED-MASTER-FILE.
      *
      *
       C10-GRADE-REPORT-HEADINGS-PARA.
           WRITE GRADE-REPORT-OUT FROM WS-GR-LINE-01
               AFTER ADVANCING PAGE.
           MOVE SPACES TO GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT FROM WS-GR-LINE-02
               AFTER ADVANCING WS-SPACING.
           MOVE SPACES TO GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT FROM WS-GR-LINE-03
               AFTER ADVANCING 1 LINES.
           MOVE SPACES TO GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT FROM WS-GR-LINE-04
               AFTER ADVANCING WS-SPACING.
           MOVE SPACES TO GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT FROM WS-GR-LINE-05
               AFTER ADVANCING WS-SPACING.
           WRITE GRADE-REPORT-OUT FROM WS-GR-LINE-06
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
           MOVE SPACES TO GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
           MOVE SPACES TO GRADE-REPORT-OUT.
           WRITE GRADE-REPORT-OUT.
      *
      *
       C20-ERROR-LOG-HEADINGS-PARA.
           MOVE WS-MONTH TO MONTH-EL-OUT.
           MOVE WS-DAY TO DAY-EL-OUT.
           MOVE WS-YEAR TO YEAR-EL-OUT.
           WRITE ERROR-LOG-OUT FROM WS-EL-LINE-01
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT FROM WS-EL-LINE-02
               AFTER ADVANCING 1 LINE.
           MOVE SPACES TO ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT FROM WS-EL-LINE-03.
           MOVE SPACES TO ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT FROM WS-EL-LINE-04.
           MOVE SPACES TO ERROR-LOG-OUT.
           WRITE ERROR-LOG-OUT. 