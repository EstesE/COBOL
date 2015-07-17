       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.      PROGRAM-15-4.
      *AUTHOR.          ERIC.
      *INSTALLATION.    WSC.
      *DATE-WRITTEN.    4/15/2003.
      *DATE-COMPILED.   4/15/2003.
      *SECURITY.        NONE.
      *
      *
      *
      * THIS PROGRAM ACCEPTS A STUDENT ID AND THEN READS STUTRANS.DAI
      * AND STUDENT.DAI AND DISPLAYS DATA FROM BOTH FILES. 
      *
      *
      *
       ENVIRONMENT DIVISION.
      *
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT STU-FILE 
               ASSIGN TO "STUDENT.DAI"
               ORGANIZATION INDEXED
               ACCESS RANDOM
               RECORD KEY IS STUDENT-ID.
           SELECT TRANS-FILE 
               ASSIGN TO "STUTRANS.DAI"
               ORGANIZATION INDEXED
               ACCESS DYNAMIC
               RECORD KEY IS STUDENT-KEY
               ALTERNATE RECORD KEY IS STDNT-ID WITH DUPLICATES.
      *
      *
      *
       DATA DIVISION.
      *
      *
       FILE SECTION.
      *
       FD  STU-FILE.
       01  STUDENT-REC.
           05  FILLER                     PIC XX.
           05  STUDENT-ID.
               10  STUD-ID-3              PIC 9(3).
               10  STUD-ID-2              PIC 9(2).
               10  STUD-ID-4              PIC 9(4).
           05  LASTN                      PIC X(14).
           05  FIRSTN                     PIC X(10).
           05  FILLER                     PIC X(3).
           05  MAJOR-CODE                 PIC X(3).
           05  FILLER                     PIC X(3).
           05  GRADE-POINTS               PIC 9(3).
           05  COMP-UNITS                 PIC 9(3).
           05  FILLER                     PIC X(14).
           05  FILLER                     PIC X(6).
      *
       FD  TRANS-FILE.
       01  TRANS-REC.
           05  FILLER                     PIC XX.
           05  STUDENT-KEY.
               10 STDNT-ID                PIC 9(9).
               10 REC-CNT                 PIC 9.
           05  DEPT                       PIC X(4).
           05  COURSE-NUM                 PIC X(4).
           05  COURSE-DESC                PIC X(24).
           05  GRADE                      PIC X.
           05  UNITS                      PIC 9.
      *
      *
       WORKING-STORAGE SECTION.
      *
      *
       01  CALCULATIONS.
           05  EARNED-GRADE-POINTS        PIC ZZ9.
           05  CURR-GRADE-POINTS          PIC 999.
           05  GPA                        PIC 9.99.
      *
       01  PROGRAM-SWITCHES.
           05  EOF-SW                     PIC XXX VALUE "NO".
           05  ANOTHER-SW                 PIC X VALUE "Y".
           05  STDNT-ID-HOLD              PIC 9(9).
           05  NO-COURSE-SW               PIC X VALUE "N".
      *
       01  SCREEN-LINE-01.
           05  BLANK-LINE                 PIC X(10) VALUE "          ".
           05  TOP-LEFT                   PIC X VALUE "l".
           05  TOP-LINE                   PIC X(68) VALUE ALL "q".
           05  TOP-RIGHT                  PIC X VALUE "k".
           05  LINE-01                    PIC X(40) VALUE
               "BAYVIEW INSTITUTE OF COMPUTER TECHNOLOGY".
           05  LINE-02                    PIC X(25) VALUE
               "STUDENT CLASS ASSIGNMENTS".
           05  LINE-03                    PIC X(23) VALUE
               "(COURSE LOOKUP PROGRAM)".
           05  LINE-04                    PIC X(48) VALUE
               "WELCOME TO THE STUDENT CLASS ASSIGNMENTS PROGRAM".
           05  LINE-05                    PIC X(21) VALUE
               "(CODED BY ERIC ESTES)".
           05  LINE-06                    PIC X(35) VALUE
               "TO DISPLAY A STUDENT'S GRADE REPORT".
           05  LINE-07                    PIC X(68) VALUE ALL "*".
           05  LINE-08                    PIC X(9) VALUE ALL " ".
           05  LINE-09                    PIC X(35) VALUE
               "ENTER THE STUDENT'S ID IN THE FIELD".
           05  LINE-10                    PIC X(27) VALUE
               "OR ENTER 999999999 TO EXIT:".
      *
       01  SIDE-LINES. 
           05  ROW-COUNT                  PIC 99 VALUE 1.
           05  ROW-MOVE                   PIC 99.
      *
       01  LINE-NUM                       PIC 99 VALUE 15.
       01  COL-NUM                        PIC 99 VALUE 11.
       01  GR-VLINE                       PIC 99 VALUE 3.
       01  LINE-TOT                       PIC 99.
      *
       01  GRADE-REPORT-HEADINGS.
           05  GR-LINE-01                 PIC X(40) VALUE
               "BAYVIEW INSTITUTE OF COMPUTER TECHNOLOGY".
           05  GR-LINE-02                 PIC X(25) VALUE
               "STUDENT CLASS ASSIGNMENTS".
           05  GR-LINE-03                 PIC X(16) VALUE
               "STUDENT NUMBER: ".
           05  GR-LINE-04                 PIC X(9) VALUE
               "STUDENT: ".
           05  GR-LINE-05                 PIC X(6) VALUE
               "COURSE".
           05  GR-LINE-06.
               10  GR-LINE-06A            PIC X(5) VALUE
                   "DEPT.".
               10  GR-LINE-06B            PIC X(6) VALUE
                   "NUMBER".
               10  GR-LINE-06C            PIC X(11) VALUE
                   "DESCRIPTION".
               10  GR-LINE-06D            PIC X(5) VALUE
                   "UNITS".
      *
      *
      *
       PROCEDURE DIVISION.
      *
      *
       MAIN-PARA.
           OPEN INPUT STU-FILE
                      TRANS-FILE.
           PERFORM C10-MENU-PARA.
           PERFORM PROCESS-IT
               UNTIL EOF-SW = "YES" AND ANOTHER-SW = "N".
           CLOSE STU-FILE TRANS-FILE.
           DISPLAY " " ERASE SCREEN.
           STOP RUN.
      *
      *
       C10-MENU-PARA.
           DISPLAY " "  ERASE SCREEN.
           DISPLAY "(0".
           DISPLAY "l" WITH NO ADVANCING LINE 3
                                         COLUMN 10.
           DISPLAY TOP-LINE WITH NO ADVANCING.
           DISPLAY "k".


      *    CREATE LEFT SIDE OF FORM.
           PERFORM UNTIL ROW-COUNT = 18
               ADD 3 TO ROW-COUNT GIVING ROW-MOVE
               DISPLAY "x" LINE ROW-MOVE
                                COLUMN 10
               ADD 1 TO ROW-COUNT
           END-PERFORM.
           MOVE 1 TO ROW-COUNT.
           MOVE 0 TO ROW-MOVE.


      *    CREATE RIGHT SIDE OF FORM.
           PERFORM UNTIL ROW-COUNT = 18
               ADD 3 TO ROW-COUNT GIVING ROW-MOVE
               DISPLAY "x" LINE ROW-MOVE
                                COLUMN 79
               ADD 1 TO ROW-COUNT
           END-PERFORM.


           DISPLAY "m" LINE 21
                       COLUMN 10.
           DISPLAY TOP-LINE WITH NO ADVANCING.
           DISPLAY "j".


           DISPLAY "(B".
           DISPLAY LINE-01
                           LINE 5
                           COLUMN 25.
           DISPLAY LINE-02 LINE 6
                           COLUMN 33.
           DISPLAY LINE-03 LINE 7
                           COLUMN 34 REVERSED.
           DISPLAY LINE-07 LINE 8
                           COLUMN 11.
           DISPLAY LINE-04 LINE 10
                           COLUMN 20.
           DISPLAY LINE-05 LINE 11
                           COLUMN 34.
           DISPLAY LINE-06 LINE 14
                           COLUMN 12.
           DISPLAY LINE-09 LINE 15
                           COLUMN 12.
           DISPLAY LINE-10 LINE 16
                           COLUMN 12.
           DISPLAY LINE-08 LINE 20
                           COLUMN 12 REVERSED.
           DISPLAY "(B".
           IF NO-COURSE-SW = "Y"
               DISPLAY "*** NO COURSES ***" LINE 18
                                            COLUMN 20 BLINKING.
           ACCEPT STUDENT-ID LINE 20 COLUMN 12.
      *
      *
       PROCESS-IT.
           IF STUDENT-ID = 999999999 THEN
               MOVE "YES" TO EOF-SW
               MOVE "N" TO ANOTHER-SW
           ELSE
               PERFORM B20-DATA-PARA UNTIL EOF-SW = "YES" AND
                   ANOTHER-SW = "N" OR STUDENT-ID = 999999999
           END-IF.
      *
      *
       B20-DATA-PARA.
           READ STU-FILE
               INVALID KEY
                   DISPLAY "*** INVALID STUDENT ID ***" 
                      LINE 18 COLUMN 12 BLINKING
                    DISPLAY LINE-08 LINE 20
                        COLUMN 12 REVERSED
                    ACCEPT STUDENT-ID LINE 20 COLUMN 12
               NOT INVALID KEY
                   MOVE STUDENT-ID TO STDNT-ID
                   MOVE 1 TO REC-CNT
                   READ TRANS-FILE
                       INVALID KEY
                           DISPLAY "*** NO COURSES ***" LINE 18
                                                COLUMN 12 BLINKING
                           MOVE ZEROS TO STUDENT-ID
                           MOVE 1 TO ROW-COUNT
                           MOVE "Y" TO NO-COURSE-SW
                           PERFORM C10-MENU-PARA
                       NOT INVALID KEY
                           ADD UNITS TO COMP-UNITS
                           PERFORM GET-GRADE
                           MOVE "N" TO NO-COURSE-SW
                           ADD CURR-GRADE-POINTS TO GRADE-POINTS
                           MOVE STDNT-ID TO STDNT-ID-HOLD     
                           PERFORM B25-GRADE-SCREEN-PARA
                           DISPLAY "(0"
                           DISPLAY "x" LINE LINE-NUM
                                       COLUMN 10
                           DISPLAY "x" LINE LINE-NUM
                                       COLUMN 75
                           PERFORM B30-GRADE-REPORT-BODY-PARA
                           PERFORM UNTIL EOF-SW = "YES" AND ANOTHER-SW
                               = "N" OR STDNT-ID NOT = STDNT-ID-HOLD
                               READ TRANS-FILE NEXT RECORD
                                   AT END
                                       MOVE 999999999 TO STDNT-ID-HOLD
                                       MOVE "YES" TO EOF-SW
                                       MOVE "N" TO ANOTHER-SW
                                       PERFORM B35-GR-BOTTOM
                                   NOT AT END
                                       IF STDNT-ID = STDNT-ID-HOLD
                                            ADD UNITS TO COMP-UNITS
                                            PERFORM GET-GRADE
                                            ADD CURR-GRADE-POINTS TO
                                                GRADE-POINTS
                                            DISPLAY "(0"
                                            DISPLAY "x" LINE LINE-NUM
                                                        COLUMN 10
                                            DISPLAY "x" LINE LINE-NUM
                                                        COLUMN 75
                                            DISPLAY "(B"
                                            PERFORM
                                             B30-GRADE-REPORT-BODY-PARA
                                       ELSE
                                           MOVE "YES" TO EOF-SW
                                           MOVE "N" TO ANOTHER-SW
                                           PERFORM B35-GR-BOTTOM
                                       END-IF
                               END-READ
                           END-PERFORM
                   END-READ
           END-READ.
      *
      *
       B25-GRADE-SCREEN-PARA.
           MOVE 11 TO COL-NUM.
           DISPLAY " " ERASE SCREEN.
           MOVE 3 TO GR-VLINE.
           DISPLAY "(0".
           DISPLAY "l" LINE 2 COLUMN 10.
           PERFORM UNTIL COL-NUM = 75
               DISPLAY "q" LINE 2 COLUMN COL-NUM
               ADD 1 TO COL-NUM
           END-PERFORM.
           PERFORM UNTIL GR-VLINE = 15
               DISPLAY "x" LINE GR-VLINE
                           COLUMN 10
               DISPLAY "x" LINE GR-VLINE
                           COLUMN 75
               ADD 1 TO GR-VLINE
           END-PERFORM.
           DISPLAY "k" LINE 2 COLUMN 75.
           DISPLAY "(B".
           DISPLAY GR-LINE-01 LINE 5 COLUMN 25.
           DISPLAY GR-LINE-02 LINE 6 COLUMN 33.
           DISPLAY GR-LINE-03 LINE 8 COLUMN 19.
           DISPLAY GR-LINE-04 LINE 10 COLUMN 19.
           DISPLAY GR-LINE-05 LINE 12 COLUMN 26.
           DISPLAY GR-LINE-06A LINE 13 COLUMN 19.
           DISPLAY GR-LINE-06B LINE 13 COLUMN 26.
           DISPLAY GR-LINE-06C LINE 13 COLUMN 37.
           DISPLAY GR-LINE-06D LINE 13 COLUMN 65.
           DISPLAY STUD-ID-3 LINE 8 COLUMN 35.
           DISPLAY "-" LINE 8 COLUMN 38.
           DISPLAY STUD-ID-2 LINE 8 COLUMN 39.
           DISPLAY "-" LINE 8 COLUMN 41.
           DISPLAY STUD-ID-4 LINE 8 COLUMN 42.
           DISPLAY LASTN LINE 10 COLUMN 28.
           DISPLAY FIRSTN LINE 10 COLUMN 42.
      *
      *
       B30-GRADE-REPORT-BODY-PARA.
           DISPLAY DEPT LINE LINE-NUM 
                        COLUMN 19.
           DISPLAY COURSE-NUM LINE LINE-NUM
                              COLUMN 28.
           DISPLAY COURSE-DESC LINE LINE-NUM
                                    COLUMN 37.
           DISPLAY UNITS LINE LINE-NUM
                              COLUMN 67.
           ADD 1 TO LINE-NUM.
      * 
      *
       B35-GR-BOTTOM.
           ADD 9 TO LINE-NUM GIVING LINE-TOT.
           DISPLAY "(0".
           PERFORM UNTIL LINE-NUM = LINE-TOT
               DISPLAY "x" LINE LINE-NUM
                           COLUMN 10
               DISPLAY "x" LINE LINE-NUM
                           COLUMN 75
               ADD 1 TO LINE-NUM
           END-PERFORM.
           DISPLAY "m" LINE LINE-TOT
                       COLUMN 10.
           DISPLAY "j" LINE LINE-TOT
                       COLUMN 75.
           MOVE 11 TO COL-NUM.
           PERFORM UNTIL COL-NUM = 75
               DISPLAY "q" LINE LINE-TOT
                           COLUMN COL-NUM
               ADD 1 TO COL-NUM
           END-PERFORM.
           DISPLAY "(B".
           SUBTRACT 2 FROM LINE-TOT
           DISPLAY "DO YOU WANT TO ACCESS ANOTHER STUDENT <Y/N>? "
                    LINE LINE-TOT
                    COLUMN 19.
           SUBTRACT 5 FROM LINE-TOT.
           DISPLAY "CUMULATIVE SUMMARY" LINE LINE-TOT
                                        COLUMN 19.
           ADD 1 TO LINE-TOT.
           DISPLAY "TOTAL UNITS:" LINE LINE-TOT
                                  COLUMN 19.
           DISPLAY COMP-UNITS LINE LINE-TOT
                              COLUMN 41.
           ADD 1 TO LINE-TOT.
           DISPLAY "EARNED GRADE POINTS:" LINE LINE-TOT
                                          COLUMN 19.
           DISPLAY GRADE-POINTS LINE LINE-TOT
                                    COLUMN 41.
           ADD 1 TO LINE-TOT.
           DISPLAY "GRADE POINT AVERAGE:" LINE LINE-TOT
                                          COLUMN 19.
           COMPUTE GPA ROUNDED = GRADE-POINTS/COMP-UNITS.
           DISPLAY GPA LINE LINE-TOT
                       COLUMN 41.
           ADD 2 TO LINE-TOT.
           ACCEPT ANOTHER-SW LINE LINE-TOT
                             COLUMN 64.
           MOVE 11 TO COL-NUM.
           MOVE 15 TO LINE-NUM.
           MOVE 3 TO GR-VLINE.
           MOVE 0 TO LINE-TOT.
           MOVE 1 TO ROW-COUNT.
           MOVE 0 TO ROW-MOVE.
           MOVE 0 TO LINE-TOT.
           MOVE ZEROS TO STUD-ID-3.
           MOVE ZEROS TO STUD-ID-2.
           MOVE ZEROS TO STUD-ID-4.
           MOVE ZEROS TO CURR-GRADE-POINTS.
           MOVE ZEROS TO GPA.
           IF ANOTHER-SW = "Y" THEN
               PERFORM C10-MENU-PARA
               MOVE "NO" TO EOF-SW
               MOVE "Y" TO ANOTHER-SW
           GO TO PROCESS-IT
           ELSE
               CLOSE STU-FILE TRANS-FILE
               DISPLAY " " ERASE SCREEN
               STOP RUN
           END-IF.
      *
      *
       GET-GRADE.
           EVALUATE GRADE
               WHEN "A"
                   COMPUTE CURR-GRADE-POINTS = UNITS * 4
               WHEN "B"
                   COMPUTE CURR-GRADE-POINTS = UNITS * 3
               WHEN "C"
                   COMPUTE CURR-GRADE-POINTS = UNITS * 2
               WHEN "D"
                   COMPUTE CURR-GRADE-POINTS = UNITS * 1
               WHEN "F"
                   COMPUTE CURR-GRADE-POINTS = UNITS * 0
               WHEN OTHER
                   CONTINUE
               END-EVALUATE. 